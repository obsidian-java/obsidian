package com.github.luzhuomi.obsidian

import scala.collection.Map._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Syntax
import com.github.luzhuomi.obsidian.ASTUtils._


/**
  * Issues
  * 
  *  #1. Desugaring do { stmt } while (exp) to
  *      { stmt; while (exp) {stmt;}; }
  *      causes label out of scope. e.g.
  * 
  *  int i = 1;
  *  L1: do { 
  *     if (i==0) { continue L1; }
  *     else { i = i -1; }
  *  }  while ( i > 0 ) 
  *  
  *  becomes
  * 
  * if (i==0) { continue L1; }
  *     else { i = i -1; }
  * L1: while (i > 0) {
  *     if (i==0) { continue L1; }
  *         else { i = i -1; }
  * }
  * 
  * the desugared version is invalid Java (syntatically correct, however the label is out of scoped)
  * 
  * it is ok, we should able to translate to CPS
  * 
  * what about there is no label?
  * 
  *  do { 
  *     if (i==0) { continue; }
  *     else { i = i -1; }
  *  }  while ( i > 0 ) 
  * 
  *  is desugared to 
  * 
  * if (i==0) { continue; } // this is dangling...
  *     else { i = i -1; }
  * while (i > 0) {
  *     if (i==0) { continue }
  *         else { i = i -1; }
  * }
  * 
  * maybe we should aggresively add labels to loop?
  * this have to be done before desugaring
  * 
  * alternatively, we add do ... loop to the CPS and SSA forms. 
  * 
  * let's try the first approach first (i.e. adding the label to loop)
  */


object Desugar {
  trait DSG[A] {
    def desugar(a: A): A
  }

  object dsgOps {
    def desugar[A](a: A)(implicit d: DSG[A]): A = {
      d.desugar(a)
    }
  }

  implicit def methodDSGInstance: DSG[MethodDecl] = {
    new DSG[MethodDecl] {
      override def desugar(a: MethodDecl): MethodDecl =
        a match {
          case MethodDecl(
                modifiers,
                type_params,
                return_ty,
                fname,
                formal_params,
                ex_types,
                exp,
                body
              ) =>
            MethodDecl(
              modifiers,
              type_params,
              return_ty,
              fname,
              formal_params,
              ex_types,
              exp,
              dsgOps.desugar(body)
            )
        }
    }
  }

  implicit def bodyDSGInstance: DSG[MethodBody] = {
    new DSG[MethodBody] {
      override def desugar(a: Syntax.MethodBody): Syntax.MethodBody =
        a match {
          case MethodBody(Some(blk)) => {
            val sblk = dsgOps.desugar(blk)
            MethodBody(Some(sblk))
          }
          case _ => a
        }
    }
  }

  implicit def blockDSGInstance: DSG[Block] = {
    new DSG[Block] {
      override def desugar(a: Syntax.Block): Syntax.Block =
        a match {
          case Block(stmts) => {
            val sstmts = stmts.map(stmt => dsgOps.desugar(stmt))
            Block(sstmts)
          }
        }
    }
  }
  implicit def blockStmtDSGInstance: DSG[BlockStmt] = {
    new DSG[BlockStmt] {
      override def desugar(a: Syntax.BlockStmt): Syntax.BlockStmt =
        a match {
          case LocalVars(modifiers, ty, var_decls) => a
          case LocalClass(class_decl)              => a
          case BlockStmt_(stmt)                    => BlockStmt_(dsgOps.desugar(stmt))
        }
    }
  }

  def appBlockStmts(stmt: Stmt, blkStmts: List[BlockStmt]): Stmt =
    stmt match {
      case StmtBlock(Block(blkStmts_)) =>
        StmtBlock(Block(blkStmts_ ++ blkStmts))
      case _ => StmtBlock(Block(BlockStmt_(stmt) :: blkStmts))
    }

  implicit def stmtDSGInstance: DSG[Stmt] = {
    new DSG[Stmt] {
      override def desugar(a: Syntax.Stmt): Syntax.Stmt =
        a match {
          case Assert(exp, msg) => Assert(dsgOps.desugar(exp), msg)
          /**
            * Basic For is compiled to while
            *
            * for (init, loop_cond, post_update, stmt) ==>
            *   {
            *     init;
            *     while(loop_cond) {
            *        stmt;
            *        post_update;
            *     }
            *   }
            */
          case BasicFor(init, loop_cond, post_update, stmt) => {

            val pre: List[BlockStmt] = init match {
              case None => Nil
              case Some(for_init) =>
                for_init match {
                  case ForLocalVars(modifiers, ty, var_decls) => {
                    List(LocalVars(modifiers, ty, var_decls))
                  }
                  case ForInitExps(exps) =>
                    exps.map(exp => {
                      BlockStmt_(ExpStmt(dsgOps.desugar(exp)))
                    })
                }
            }
            val cond = loop_cond match {
              case None      => Lit(BooleanLit(true))
              case Some(exp) => dsgOps.desugar(exp)
            }
            val stmt_post_update = post_update match {
              case None => dsgOps.desugar(stmt)
              case Some(exps) => {
                val post_update_blkstmts = exps.map(exp => {
                  BlockStmt_(ExpStmt(dsgOps.desugar(exp)))
                })
                appBlockStmts(stmt, post_update_blkstmts)
              }
            }
            val loop = While(dsgOps.desugar(cond), dsgOps.desugar(stmt))
            StmtBlock(Block(pre ++ List(BlockStmt_(stmt_post_update))))
          }
          case Break(id)    => Break(id)
          case Continue(id) => Continue(id)
          case Do(stmt, exp) =>
            StmtBlock(
              Block(
                List(
                  BlockStmt_(dsgOps.desugar(stmt)),
                  BlockStmt_(While(dsgOps.desugar(exp), dsgOps.desugar(stmt)))
                )
              )
            )
          case Empty => Empty
          /**
            * Enhanced For with primitive type arrays is desugared to BasicFor
            *  for (t x: exp) { stmt } ===> for (int i = 0; i < exp.length; i++) { t x = exp3[i]; stmt }
            */
          case EnhancedFor(modifiers, ty @ PrimType_(_), id, exp, stmt) => {
            val i = Ident(s"idx_loop${id.toString()}")
            val var_decls = List(
              VarDecl(VarId(i), Some(InitExp(Lit(IntLit(0)))))
            )
            val init = Some(ForLocalVars(Nil, PrimType_(IntT), var_decls))
            val exp2 = Some(
              BinOp(
                ExpName(Name(i :: Nil)),
                LThan,
                dotField(exp, Ident("length"))
              )
            )
            val exp3 = Some(List(PostIncrement(ExpName(Name(i :: Nil)))))
            val var_decls2 = List(
              VarDecl(
                VarId(id),
                Some(
                  InitExp(
                    ArrayAccess(
                      ArrayIndex(exp, List(ExpName(Name(i :: Nil))))
                    )
                  )
                )
              )
            )
            val basicFor: Stmt = BasicFor(
              init,
              exp2,
              exp3,
              prpDecl(modifiers, ty, var_decls2, stmt)
            )
            dsgOps.desugar(basicFor)
          }
          case EnhancedFor(modifiers, ty @ RefType_(t), id, exp, stmt) => {
            /**
            *  Enhanced For with object type arrays is desguared into While
            *   for (T x: exp) { stmt }  ===>
            *   {
            *     java.util.Iterator<T> l = exp.iterator();
            *     while (l.hasNext()) { T x = l.next(); stmt }
            *   }
            */
            val l = Ident(s"itr_loop_l_${id.toString()}")
            val iteratorTy = RefType_(
              ClassRefType(
                ClassType(
                  List(
                    (Ident("java"), Nil),
                    (Ident("util"), Nil),
                    (Ident("Iterator"), List(ActualType(t)))
                  )
                )
              )
            )
            val iteratorDecl = LocalVars(
              Nil,
              iteratorTy,
              List(
                VarDecl(
                  VarId(l),
                  Some(
                    InitExp(
                      MethodInv(
                        PrimaryMethodCall(exp, Nil, Ident("iterator"), Nil)
                      )
                    )
                  )
                )
              )
            )
            val txDecl = List(
              VarDecl(
                VarId(id),
                Some(
                  InitExp(
                    MethodInv(
                      PrimaryMethodCall(
                        ExpName(Name(l :: Nil)),
                        Nil,
                        Ident("next"),
                        Nil
                      )
                    )
                  )
                )
              )
            )
            val while_stmt:Stmt = While(
              MethodInv(
                PrimaryMethodCall(
                  (ExpName(Name(l :: Nil))),
                  Nil,
                  Ident("hasNext"),
                  Nil
                )
              ),
              prpDecl(modifiers, ty, txDecl, stmt)
            )
            val blk = Block(
              List(iteratorDecl, BlockStmt_(dsgOps.desugar(while_stmt)))
            )
            StmtBlock(blk)
          }
          /**
            * x = y--; ===> 
            * 
            * { x = y;
            *   y = y - 1;
            * }
            */
          case ExpStmt(Assign(lhs,op,PostDecrement(ExpName(name)))) => {
              val a = ExpStmt(Assign(lhs,op,ExpName(name)))
              val i = ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Sub ,Lit(IntLit(1)))))
              StmtBlock(Block(List(
                  BlockStmt_(a), BlockStmt_(i)
              )))            
          }
          case ExpStmt(Assign(lhs,op,PostIncrement(ExpName(name)))) => {
              val a = ExpStmt(Assign(lhs,op,ExpName(name)))
              val i = ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Add ,Lit(IntLit(1)))))
              StmtBlock(Block(List(
                  BlockStmt_(a), BlockStmt_(i)
              )))            
          }
          case ExpStmt(Assign(lhs,op,PreDecrement(ExpName(name)))) => {
              val a = ExpStmt(Assign(lhs,op,ExpName(name)))
              val i = ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Sub ,Lit(IntLit(1)))))
              StmtBlock(Block(List(
                  BlockStmt_(i), BlockStmt_(a)
              )))            
          }
          case ExpStmt(Assign(lhs,op,PreIncrement(ExpName(name)))) => {
              val a = ExpStmt(Assign(lhs,op,ExpName(name)))
              val i = ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Add ,Lit(IntLit(1)))))
              StmtBlock(Block(List(
                  BlockStmt_(i), BlockStmt_(a)
              )))            
          }
          // TODO: what about case like x = (y = y + 1)?
          case ExpStmt(exp) => ExpStmt(dsgOps.desugar(exp))
          case IfThen(exp, stmt) => // if then is desugar if then else with else branch empty
            IfThenElse(dsgOps.desugar(exp), dsgOps.desugar(stmt), Empty)
          case IfThenElse(exp, th, el) =>
            IfThenElse(
              dsgOps.desugar(exp),
              dsgOps.desugar(th),
              dsgOps.desugar(el)
            )
          case Labeled(id, stmt) => Labeled(id, dsgOps.desugar(stmt))
          case Return(exp) => Return(exp.map(e => dsgOps.desugar(e)))
          case StmtBlock(blk) => StmtBlock(dsgOps.desugar(blk))
          case Switch(exp, blocks) => Switch(dsgOps.desugar(exp), blocks.map(dsgOps.desugar(_)))
          case Synchronized(exp, blk) => Synchronized(dsgOps.desugar(exp), dsgOps.desugar(blk))
          case Throw(exp) => Throw(dsgOps.desugar(exp))
          // TODO: collapse catches into 1 catch with if else and instance of
          //       add an empty finally block if it is None
          case Try(try_blk, catches, finally_blk) => Try(dsgOps.desugar(try_blk), catches.map(c => dsgOps.desugar(c)), finally_blk.map(b => dsgOps.desugar(b)))
          case While(exp, stmt) => While(dsgOps.desugar(exp), dsgOps.desugar(stmt))
        }
    }
  }

  implicit def expDSGInstance: DSG[Exp] = {
    new DSG[Exp] {
      override def desugar(a: Exp): Exp = a match {
        case ArrayAccess(idx) => ArrayAccess(idx)
        case Cast(ty, exp) => Cast(ty, dsgOps.desugar(exp))
        case ArrayCreate(ty, exps, num_dims) => ArrayCreate(ty, exps.map(dsgOps.desugar(_)), num_dims)
        case ArrayCreateInit(ty, size, init) => ArrayCreateInit(ty, size, dsgOps.desugar(init))
        case Assign(lhs, EqualA, rhs) => Assign(lhs, EqualA, dsgOps.desugar(rhs))
        case Assign(lhs, aop, rhs) => {
          val rlhs:Exp = lhsToRhs(lhs)
          val op = aop match {
            case MultA    => Mult
            case DivA     => Div
            case RemA     => Rem
            case AddA     => Add
            case SubA     => Sub
            case LShiftA  => LShift
            case RShiftA  => RShift
            case RRShiftA => RRShift
            case AndA     => And
            case XorA     => Xor
            case OrA      => Or
            case EqualA   => Equal // this pattern should not be reached
            // the last case "EQualA" should never happen
          }
          Assign(lhs, EqualA, BinOp(dsgOps.desugar(rlhs), op, dsgOps.desugar(rhs)))
        }
        case BinOp(e1, op, e2) => BinOp(dsgOps.desugar(e1), op, dsgOps.desugar(e2))
        case ClassLit(ty) => ClassLit(ty)
        case Cond(cond, true_exp, false_exp) => Cond(dsgOps.desugar(cond), dsgOps.desugar(true_exp), dsgOps.desugar(false_exp))
        case ExpName(name) => ExpName(name)
        case FieldAccess_(access) => FieldAccess_(dsgOps.desugar(access))
        case InstanceCreation(type_args, type_decl, args, body) => InstanceCreation(type_args, type_decl, args.map(dsgOps.desugar(_)), body) // we don't desugar anonymous class body
        case InstanceOf(e, ref_type) => InstanceOf(dsgOps.desugar(e), ref_type)
        case Lambda(params, body) => Lambda(params, body) // we do not desugar the body of lambda term
        case Lit(lit) => Lit(lit)
        case MethodInv(methodInv) => MethodInv(dsgOps.desugar(methodInv))
        case MethodRef(name, id) => MethodRef(name, id)
        case PostDecrement(exp) => PostDecrement(exp) // nothigng to do, exp must be a name, this case should not be fired.
        case PostIncrement(exp) => PostIncrement(exp)
        case PreBitCompl(exp) => PreBitCompl(dsgOps.desugar(exp))
        case PreDecrement(exp) => PreDecrement(exp) 
        case PreIncrement(exp) => PreIncrement(exp) 
        case PreMinus(exp) => PreMinus(dsgOps.desugar(exp))
        case PreNot(exp) => PreNot(dsgOps.desugar(exp))
        case PrePlus(exp) => PrePlus(dsgOps.desugar(exp))
        case QualInstanceCreation(exp, type_args, id, args, body) => QualInstanceCreation(dsgOps.desugar(exp), type_args, id, args.map(dsgOps.desugar(_)), body) // we don't desugar body
        case This => This
        case ThisClass(name) => ThisClass(name)
      }
    }
  }

  implicit def methodInvDSGInstance:DSG[MethodInvocation] = {
    new DSG[MethodInvocation] {
      override def desugar(a: Syntax.MethodInvocation): Syntax.MethodInvocation = a match {
          case ClassMethodCall(name, ref_types, id, args) => ClassMethodCall(name, ref_types, id, args.map(dsgOps.desugar(_)))
          case MethodCall(name, args) => MethodCall(name, args.map(dsgOps.desugar(_)))
          case PrimaryMethodCall(e, ref_types, id, args) => PrimaryMethodCall(dsgOps.desugar(e),ref_types, id, args.map(dsgOps.desugar(_)))
          case SuperMethodCall(ref_types, id, args) => SuperMethodCall(ref_types, id, args.map(dsgOps.desugar(_)))
          case TypeMethodCall(name, ref_types, id, args) => TypeMethodCall(name, ref_types, id, args.map(dsgOps.desugar(_)))
      }
    }
  }
  

  implicit def arrayInitDSGInstance:DSG[ArrayInit] = {
    new DSG[ArrayInit] {
      override def desugar(a: Syntax.ArrayInit): Syntax.ArrayInit = a match {
          case ArrayInit(var_inits) => ArrayInit(var_inits.map(dsgOps.desugar(_)))
      }
    }
  }

  implicit def varInitDSGInstance:DSG[VarInit] = {
      new DSG[VarInit] {
          override def desugar(a: Syntax.VarInit): Syntax.VarInit = a match {
              case InitArray(array_init) => InitArray(dsgOps.desugar(array_init))
              case InitExp(exp) => InitExp(dsgOps.desugar(exp))
          }
      }
  }

  implicit def fieldAccessDSGInstance:DSG[FieldAccess] = {
    new DSG[FieldAccess] {
      override def desugar(a: Syntax.FieldAccess): Syntax.FieldAccess = a match {
          case ClassFieldAccess(name, id) => ClassFieldAccess(name, id) 
          case PrimaryFieldAccess(e, id) => PrimaryFieldAccess(dsgOps.desugar(e), id)
          case SuperFieldAccess(id) =>  SuperFieldAccess(id)
      }
    }
  }
  
  implicit def catchDSGInstance: DSG[Catch] = {
    new DSG[Catch] {
      override def desugar(a: Syntax.Catch): Syntax.Catch = a match {
        case Catch(params, blk) => Catch(params, dsgOps.desugar(blk))
      }
    }
  }
  
  implicit def switchBlockDSGInstance: DSG[SwitchBlock] = new DSG[SwitchBlock] {
    override def desugar(a: Syntax.SwitchBlock): Syntax.SwitchBlock = a match {
      case SwitchBlock(Default, blk_stmts) => SwitchBlock(Default, blk_stmts.map(dsgOps.desugar(_)))
      case SwitchBlock(SwitchCase(e), blk_stmts) => SwitchBlock(SwitchCase(dsgOps.desugar(e)), blk_stmts.map(dsgOps.desugar(_)))
    }
  }
}
