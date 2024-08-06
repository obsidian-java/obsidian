package obsidian.lang.java

import scala.collection.Map.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Syntax
import obsidian.lang.java.ASTUtils.*


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
    def desugar[A](a: A)(using d: DSG[A]): A = {
      d.desugar(a)
    }
  }

  given methodDSGInstance: DSG[MethodDecl] = {
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

  given bodyDSGInstance: DSG[MethodBody] = {
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

  given blockDSGInstance: DSG[Block] = {
    new DSG[Block] {
      override def desugar(a: Syntax.Block): Syntax.Block =
        a match {
          /** empty block is desugared to a block contain an empty statement
            * { } ==> { ; }
            */
          case Block(Nil) => { Block(List(BlockStmt_(Empty))) } 
          case Block(stmts) => {
            val sstmts = stmts.map(stmt => dsgOps.desugar(stmt))
            Block(sstmts)
          }
        }
    }
  }
  given blockStmtDSGInstance: DSG[BlockStmt] = {
    new DSG[BlockStmt] {
      override def desugar(a: Syntax.BlockStmt): Syntax.BlockStmt =
        a match {
          case LocalVars(modifiers, ty, var_decls) => a
          case LocalClass(class_decl)              => a
          case BlockStmt_(stmt)                    => BlockStmt_(dsgOps.desugar(stmt))
        }
    }
  }


  given stmtDSGInstance: DSG[Stmt] = {
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
                  BlockStmt_(desugar(ExpStmt(exp)))
                })
                appBlockStmts(stmt, post_update_blkstmts)
              }
            }
            val loop = While(dsgOps.desugar(cond), stmt_post_update)
            StmtBlock(Block(pre ++ List(BlockStmt_(loop))))
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
          case EnhancedFor(modifiers, ty @ PrimType_(_), id@Ident(n), exp, stmt) => {
            val i = Ident(s"idx_loop_${n}")
            val var_decls = List(
              VarDecl(VarId(i), Some(InitExp(Lit(IntLit(0)))))
            )
            val init = Some(ForLocalVars(Nil, PrimType_(IntT), var_decls))
            val exp2 = Some(
              BinOp(
                ExpName(Name(i :: Nil)),
                LThan,
                exp match { 
                  case ExpName(Name(ids)) => ExpName(Name(ids ++ List(Ident("length")))) // a.i is tread as name (id "a", id "length"), refer to test case TestDesugar4 
                  case _ => dotField(exp, Ident("length"))
                }
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
          case EnhancedFor(modifiers, ty @ RefType_(t), id@Ident(n), exp, stmt) => {
            /**
            *  Enhanced For with object type arrays is desguared into While
            *   for (T x: exp) { stmt }  ===>
            *   {
            *     java.util.Iterator<T> l = exp.iterator();
            *     while (l.hasNext()) { T x = l.next(); stmt }
            *   }
            */
            val l = Ident(s"itr_loop_l_${n}")
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
                        exp match {
                          case ExpName(Name(ids)) => MethodCall(Name(ids ++ List(Ident("iterator"))), Nil)
                          case _ =>  PrimaryMethodCall(exp, Nil, Ident("iterator"), Nil)
                        }
                        
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
                      MethodCall(Name(l :: List(Ident("next"))), Nil)
                    )
                  )
                )
              )
            )
            val while_stmt:Stmt = While(
              MethodInv(MethodCall(Name(l :: List(Ident("hasNext"))), Nil)),             
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
          /**
            * x = (y = y + 1); ==>
            * {
            *   y = y + 1;
            *   x = y;
            * }
            */
          case ExpStmt(Assign(lhs1,op1,Assign(NameLhs(name),op2,rhs))) => {
            val i = ExpStmt(Assign(NameLhs(name), op2, rhs))
            val a = ExpStmt(Assign(lhs1, op1, ExpName(name)))
            StmtBlock(Block(List(
              BlockStmt_(i), BlockStmt_(a)
            )))
          }
          /**
            * x--; ==>
            * x = x - 1;
            */
          case ExpStmt(PostDecrement(ExpName(name))) => ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Sub, Lit(IntLit(1)))))
          case ExpStmt(PreDecrement(ExpName(name))) => ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Sub, Lit(IntLit(1)))))
          case ExpStmt(PostIncrement(ExpName(name))) => ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Add, Lit(IntLit(1)))))
          case ExpStmt(PreIncrement(ExpName(name))) => ExpStmt(Assign(NameLhs(name), EqualA, BinOp(ExpName(name), Add, Lit(IntLit(1)))))
          case ExpStmt(exp) => ExpStmt(dsgOps.desugar(exp))
          case IfThen(exp, stmt) => // if then is desugar if then else with else branch empty
            IfThenElse(dsgOps.desugar(exp), dsgOps.desugar(stmt), StmtBlock(Block(List(BlockStmt_(Empty)))))
          case IfThenElse(exp, th, el) =>
            IfThenElse(
              dsgOps.desugar(exp),
              dsgOps.desugar(th),
              dsgOps.desugar(el)
            )
          case Labeled(id, stmt) => Labeled(id, dsgOps.desugar(stmt))
          case Return(exp) => Return(exp.map(e => dsgOps.desugar(e)))
          case StmtBlock(blk) => StmtBlock(dsgOps.desugar(blk))
          case Switch(exp, blocks) => {
            val e = dsgOps.desugar(exp)
            val sBlks = blocks.map(dsgOps.desugar(_))
            val sBlksWithDefault = sBlks match { // add empty default case if it is not present as the last case
              case Nil => List(SwitchBlock(Default, List(BlockStmt_(Empty))))
              case noEmpty => sBlks.last match {
                case SwitchBlock(Default,_) => sBlks
                case _ => sBlks ++ List(SwitchBlock(Default, List(BlockStmt_(Empty))))
              } 
            } 
            Switch(e, sBlksWithDefault)
          }
          case Synchronized(exp, blk) => Synchronized(dsgOps.desugar(exp), dsgOps.desugar(blk))
          case Throw(exp) => Throw(dsgOps.desugar(exp))
          // collapse catches into 1 catch with if else and instance of
          //       add an empty finally block if it is None (update, not doing this anymore as finally does not work well with break and continue)
          //  TODO: we should lift and clone the finally block to all the exits 
          case Try(try_blk, catches, finally_blk) => {
            val catches_p = catches.map(c => dsgOps.desugar(c))
            val catches_merged = mergeCatches(catches_p)
            val finally_blk_p = finally_blk.map(b => dsgOps.desugar(b)) match {
              case Some(b) => Some(b)
              case None => None // Some(Block(Nil))
            }
            Try(dsgOps.desugar(try_blk), List(catches_merged), finally_blk_p)
          }
          case While(exp, stmt) => While(dsgOps.desugar(exp), dsgOps.desugar(stmt))
        }
    }
  }

  /**
    * mergeCatches : merge a list of catch clauses into a single one with if else and instance of
    * 
    * e.g. 
    * 
    * catch (Ex1 ex1) { stmt1 }
    * catch (Ex2 ex2) { stmt2 }
    * 
    * ==>
    * 
    * catch (Exception ex) {
    *   if (ex instanceOf Ex1) {
    *      Ex1 ex1 = (Ex1) ex;
    *      stmt21
    *   } else {
    *      if (ex instanceOf Ex2) {
    *         Ex2 ex2 = (Ex2) ex;
    *         stmt2;
    *      } else {
    *         throw ex; 
    *      }
    *   }
    * }
    * 
    * Note that we have to throw most general Exception, which can be caught by the outer try - catch block 
    * The outer catch block can still catch a more specific exception, despite the exception has been up-casted.
    * 
    * @param catches
    * @return
    */
  def mergeCatches(catches:List[Catch]):Catch = {
    val exceptionTy:RefType = ClassRefType(ClassType(List((Ident("Exception"), Nil))))
    val ex_top:Ident = Ident("exception_desugared")
    val params_top:FormalParam = FormalParam(Nil, RefType_(exceptionTy), false, VarId(ex_top))
    val conds:List[Exp] = catches.map(
      c => c match {
        case Catch(FormalParam(_, RefType_(ty), _, _), blk_each) => {
          InstanceOf(ExpName(Name(List(ex_top))), ty)
        }
      }
    )
    val blks:List[Block] = catches.map(
      c => c match {
        case Catch(FormalParam(mods,rty,_ ,VarId(ex_inner)), Block(block_stmts)) => {
          val localVar = LocalVars(mods, rty, List(VarDecl(VarId(ex_inner), Some(InitExp(Cast(rty, ExpName(Name(List(ex_top)))))))))
          Block(localVar::block_stmts)
        } 
      }
    )
    val lastStmt:Stmt = StmtBlock(Block(List(BlockStmt_(Throw(ExpName(Name(List(ex_top))))))))
    val ifelse = conds.zip(blks).foldRight(lastStmt)( (condBlk, elStmt) => condBlk match {
      case (cond, blk) => {
        IfThenElse(cond, StmtBlock(blk), elStmt)
      }
    })
    Catch(params_top, Block(List(BlockStmt_(ifelse))))
  }

  given expDSGInstance: DSG[Exp] = {
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

  given methodInvDSGInstance:DSG[MethodInvocation] = {
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
  

  given arrayInitDSGInstance:DSG[ArrayInit] = {
    new DSG[ArrayInit] {
      override def desugar(a: Syntax.ArrayInit): Syntax.ArrayInit = a match {
          case ArrayInit(var_inits) => ArrayInit(var_inits.map(dsgOps.desugar(_)))
      }
    }
  }

  given varInitDSGInstance:DSG[VarInit] = {
      new DSG[VarInit] {
          override def desugar(a: Syntax.VarInit): Syntax.VarInit = a match {
              case InitArray(array_init) => InitArray(dsgOps.desugar(array_init))
              case InitExp(exp) => InitExp(dsgOps.desugar(exp))
          }
      }
  }

  given fieldAccessDSGInstance:DSG[FieldAccess] = {
    new DSG[FieldAccess] {
      override def desugar(a: Syntax.FieldAccess): Syntax.FieldAccess = a match {
          case ClassFieldAccess(name, id) => ClassFieldAccess(name, id) 
          case PrimaryFieldAccess(e, id) => PrimaryFieldAccess(dsgOps.desugar(e), id)
          case SuperFieldAccess(id) =>  SuperFieldAccess(id)
      }
    }
  }
  
  given catchDSGInstance: DSG[Catch] = {
    new DSG[Catch] {
      override def desugar(a: Syntax.Catch): Syntax.Catch = a match {
        case Catch(params, blk) => Catch(params, dsgOps.desugar(blk))
      }
    }
  }
  
  given switchBlockDSGInstance: DSG[SwitchBlock] = new DSG[SwitchBlock] {
    override def desugar(a: Syntax.SwitchBlock): Syntax.SwitchBlock = a match {
      case SwitchBlock(Default, blk_stmts) => SwitchBlock(Default, blk_stmts.map(dsgOps.desugar(_)))
      case SwitchBlock(SwitchCase(e), blk_stmts) => SwitchBlock(SwitchCase(dsgOps.desugar(e)), blk_stmts.map(dsgOps.desugar(_)))
    }
  }
}
