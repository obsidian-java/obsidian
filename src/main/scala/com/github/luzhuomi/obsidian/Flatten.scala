package com.github.luzhuomi.obsidian

import cats._
import cats.implicits._
import cats.data.StateT
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Syntax



/**
  * We need a flattening step before the desugaring step to handle nested post-/pre-increment and assignment
  * consider the following 

public class Test {
    public static void main(String [] args) {
	int x = 0;
	// System.out.println(x++ + x++);
	// is flattened to
	/*
	int x1, x2;
	x1 = x++;
	x2 = x++;
	System.out.println(x1 + x2);
	*/
	// is desugared to
	/*
	int x1, x2;
	{
	    x1 = x;
	    x = x + 1;
	}
	{ 
	    x2 = x;
	    x = x + 1;
	}
	System.out.println(x1 + x2);
	*/


	// System.out.println(++x + ++x);
	// is flattened to
	
	// int x1, x2;
	// x1 = ++x;
	// x2 = ++x;
	// System.out.println(x1 + x2);
	
	
	// is desugared to
	
	int x1, x2;
	{
	    x = x + 1;
	    x1 = x;
	}
	{ 
	    x = x + 1;
	    x2 = x;
	}
	System.out.println(x1 + x2);
	

    }


}


	Another example, what about 

	System.out.println(x = (y = (z++) + 1))

	identify the top level expression to be flattened, which is x = (y = (z++) + 1), because it is not an statement itself.

	we go left-most inner most (evaluate strategy)

	x = (y = (z++) +1 )  ==>_(z++)

	z1 = z++;
	x = (y = z1+1) 

	we are done with the flattening

	
	z1 = z++;
	x = (y = z1+1) 

	is desugared to 

	{
		z1 = z;
		z = z + 1;
	}
	{
		y = z1 + 1;
		x = y;
	}

	we are done with the deguaring


    After flattening, all post-increment should appearing in the top level of the RHS of 
    some assignment statement only

  */

  object Flatten {
	  case class StateInfo(
		  currNum:Int,
		  nameSecret:String
	  )

	  val initStateInfo= StateInfo(0,"_is__flattened_")

	  sealed trait FlatResult[+A]
	  case class FlatError(msg:String) extends FlatResult[Nothing]
	  case class FlatOk[A](result:A) extends FlatResult[A]

	  implicit def flatResultFunctor: Functor[FlatResult] = new Functor[FlatResult] {
		override def map[A, B](fa: FlatResult[A])(f: A => B): FlatResult[B] = fa match {
			case FlatError(s) => FlatError(s)
			case FlatOk(a)    => FlatOk(f(a))
		}
	  }

	  implicit def flatResultApplicative: ApplicativeError[FlatResult, String] = new ApplicativeError[FlatResult, String] {
		  override def ap[A, B](ff: FlatResult[A => B])(fa: FlatResult[A]): FlatResult[B] = ff match {
			  case FlatOk(f) => fa match {
				  case FlatOk(a)    => FlatOk(f(a))
				  case FlatError(s) => FlatError(s)
			  }
			  case FlatError(s) => FlatError(s)
		  }
		  override def pure[A](a:A):FlatResult[A] = FlatOk(a)
		  override def raiseError[A](e:String):FlatResult[A] = FlatError(e)
		  override def handleErrorWith[A](fa: FlatResult[A])(f:String => FlatResult[A]) : FlatResult[A] = fa match {
			  case FlatError(s) => f(s)
			  case FlatOk(a)    => FlatOk(a)
		  }
	  }

	  implicit def flatResultMonadError(implicit app:ApplicativeError[FlatResult, String]): MonadError[FlatResult, String] = new MonadError[FlatResult, String] {
		override def flatMap[A, B](fa: FlatResult[A])(f: A => FlatResult[B]): FlatResult[B] = fa match {
			case FlatOk(a)    => f(a)
			case FlatError(s) => FlatError(s)
		}
		override def handleErrorWith[A](fa: FlatResult[A])(f: String => FlatResult[A]): FlatResult[A] = app.handleErrorWith(fa)(f)
		override def pure[A](x: A): FlatResult[A] = app.pure(x)
		override def raiseError[A](e: String): FlatResult[A] = app.raiseError(e)
		override def tailRecM[A, B](a: A)(f: A => FlatResult[Either[A,B]]): FlatResult[B] = f(a) match {
			case FlatError(msg)   => FlatError(msg)
			case FlatOk(Right(b)) => FlatOk(b)
			case FlatOk(Left(a))  => tailRecM(a)(f)
		}
	  }

	  def get: StateT[FlatResult, StateInfo, StateInfo] = StateT{ st => FlatOk(st, st) }
	  def put(st:StateInfo): StateT[FlatResult, StateInfo, Unit] = StateT{ _ => FlatOk(st, ())}

	  type FlatState[A] = StateT[FlatResult, StateInfo,A]
	
	  def flatBlock(a:Block)(implicit m:Monad[FlatState]):FlatState[Block] = a match {
		  case Block(blk_stmts) => for {
			  _ <- m.pure(())
		  } yield Block(blk_stmts) // TODO
	  }

	  def flatStmt(a:Stmt)(implicit m:MonadError[FlatState, String]):FlatState[List[Stmt]] =  a match {
		  case StmtBlock(blk) => for { f_blk <- flatBlock(blk) } yield List(StmtBlock(f_blk))
		  case IfThen(exp, stmt) => for {
			  stmts_exp <- liftAll(exp);
			  r <- stmts_exp match {
				case (Nil,_) => { // nothing to be flattened in exp
					stmt match {
						case StmtBlock(blk) => for {
							f_blk <- flatBlock(blk) 
						} yield List(IfThen(exp, StmtBlock(f_blk)))
				  		case others => for { 
							f_stmts <- flatStmt(stmt) 
						} yield List(IfThen(exp, StmtBlock(Block(f_stmts.map(BlockStmt_(_))))))
					}
				}
				case (stmts,expFinal) => { // something flattened in exp
					stmt match {
						case StmtBlock(blk) => for { 
							f_blk <- flatBlock(blk) 
						} yield stmts++List(IfThen(expFinal, StmtBlock(f_blk)))
				  		case _ => for {
							f_stmts <- flatStmt(stmt) 
						} yield stmts++List(IfThen(expFinal, StmtBlock(Block( f_stmts.map(BlockStmt_(_))))))
					}
				}
			  }
		  } yield r
		  case Assert(exp, msg) => for {
			  stmts_exp <- liftAll(exp);
			  r <- stmts_exp match {
				  case (Nil, _) => m.pure(List(Assert(exp, msg)))
				  case (stmts, expFinal) => m.pure(stmts ++ List(Assert(expFinal, msg)))
			  }
		  } yield r
		  case Continue(id) => m.pure(List(Continue(id)))
		  case BasicFor(init, loop_cond, post_update, stmt) => m.pure(List()) // TODO
		  
		  // TODO: more cases
	  }


	  


	  def liftAll(exps:List[Exp])(implicit m:MonadError[FlatState, String]):FlatState[(List[Stmt], List[Exp])] = {
		  def go(stmts_exps_acc:(List[Stmt], List[Exp]), exp:Exp):FlatState[(List[Stmt], List[Exp])] = { 
			  val stmts_acc = stmts_exps_acc._1
			  val exps_acc = stmts_exps_acc._2 
			  for {
				  stmts_exp <- liftAll(exp)
			  } yield (stmts_acc ++ stmts_exp._1, exps_acc ++ List(stmts_exp._2))
		  }
		  val empty_stmts:List[Stmt] = List()
		  val empty_exps:List[Exp] = List()
		  exps.foldM((empty_stmts,empty_exps))(go)(m)
	  }

	  /**
		* liftAll - lift all nested assignment expression from the input expression, turns them into simple assignment statements of shape (x = nested_assignment). 
		*   applying lhs xs back to their repsective context to form the flattened expression
		*
		* @param exp input expression
		* @return a list of simple assignment statements, and the flattened exprssion.
		*/
	  def liftAll(exp:Exp)(implicit m:MonadError[FlatState, String]):FlatState[(List[Stmt], Exp)] = {
		  def go(exp:Exp, acc:List[Stmt]):FlatState[(List[Stmt], Exp)] = naOps.nestedAssignment(exp) match {
			  case None => m.pure((acc,exp))
			  case Some((e, ctxt)) => for {
				  n_stmt <- mkAssignStmt(e)(m);
				  r <- go(ctxt(ExpName(n_stmt._1)), acc++List(n_stmt._2))
			  } yield r
		  }
		  go(exp,List())
	  }

	  /**
		* mkAssignStmt - turn an nested assignment / increment statement into an assignment statement whose lhs is a fresh variable
		*
		*  Note that this is a partial function. It only accepts exp that are nested assignment. c.f. nestedAssignment for Exp.
		* 
		* @param exp
		* @param m
		* @return a pair of name (lhs variable) and statement (the generated statement lhs = nested_assignment)
		*/
	  def mkAssignStmt(exp:Exp)(implicit m:MonadError[FlatState, String]):FlatState[(Name, Stmt)] = exp match {
		  case PostDecrement(ExpName(n)) => for {
			  fresh_name <- rename(n)
		  } yield (fresh_name, ExpStmt(Assign(NameLhs(fresh_name), EqualA, exp)))
		  case PostIncrement(ExpName(n)) => for {
			  fresh_name <- rename(n)
		  } yield (fresh_name, ExpStmt(Assign(NameLhs(fresh_name), EqualA, exp)))
		  case PreDecrement(ExpName(n)) => for {
			  fresh_name <- rename(n)
		  } yield (fresh_name, ExpStmt(Assign(NameLhs(fresh_name), EqualA, exp)))
		  case PreIncrement(ExpName(n)) => for {
			  fresh_name <- rename(n)
		  } yield (fresh_name, ExpStmt(Assign(NameLhs(fresh_name), EqualA, exp)))		  
		  case Assign(NameLhs(n), op, rhs) => for {
			  fresh_name <- rename(n)
		  } yield (fresh_name, ExpStmt(Assign(NameLhs(fresh_name), EqualA, exp)))
		  case _ => m.raiseError(s"mkAssignStmt: Failed to create statement from expression ${exp.toString}")
		// TODO other cases
	  }

	  def rename(n:Name)(implicit m:MonadError[FlatState,String]):FlatState[Name] = n match {
		  case Name(Nil) => m.pure(n) // should we signal a failure here?
		  case Name(ids) => { 
			  val inits = ids.init
			  ids.last match {
				  case Ident(s) => for {
					  st <- get
					  _ <- put(st.copy(currNum=st.currNum+1))
				  } yield Name(inits++List(Ident(s"${s}${st.nameSecret}${st.currNum}")))
			  }
			}
		}

	  trait NestedAssignment[A] {
		  /**
			* nestedAssign: return the left most inner most nested assignment if it exists
			*
			* @param a
			* @return an optional pair consists of the nested assignment and the context 
			*   a context is A with an expression place holder
			*/
		  def nestedAssignment(a:A):Option[(Exp, Exp => A)]
	  }

	  object naOps {
		  def nestedAssignment[A](a:A)(implicit d:NestedAssignment[A]):Option[(Exp, Exp => A)] = {
			  d.nestedAssignment(a)
		  }
	  }

	  implicit def ExpNestedAssignmentInstance:NestedAssignment[Exp] = new NestedAssignment[Exp] {
			override def nestedAssignment(e:Exp):Option[(Exp, Exp => Exp)] = e match {
				case Lit(_) => None
				case ClassLit(_) => None 
				case This => None
				case ThisClass(_) => None
				case InstanceCreation(type_args, type_decl, args, body) => None
				case QualInstanceCreation(exp, type_args, id, args, body) => None
				case ArrayCreate(ty, exps, num_dims) => None
				case ArrayCreateInit(ty, size, init) => naOps.nestedAssignment(init) match {
					case None => None
					case Some((exp, initCtxt)) => Some((exp, e1 => ArrayCreateInit(ty,size, initCtxt(e1))))
				}
				case FieldAccess_(access) => None // the exp in field access should not contain assignment
				case MethodInv(methdInv) => naOps.nestedAssignment(methdInv) match {
					case None => None 
					case Some((exp, methdInvCtxt)) => Some((exp, e1=> MethodInv(methdInvCtxt(e1))))
				}
				case ArrayAccess(idx) => naOps.nestedAssignment(idx) match {
					case None => None
					case Some((exp, idxCtxt)) => Some((exp, e1=>ArrayAccess(idxCtxt(e1))))
				}
				case ExpName(name) => None
				case PostIncrement(exp) => naOps.nestedAssignment(exp) match {
					case None => Some((e, x => x))
					case Some((e0, expCtxt)) => Some((e0, e1=>PostIncrement(e1)))
				}
				case PostDecrement(exp) => naOps.nestedAssignment(exp) match {
					case None => Some((e, x => x))
					case Some((e0, expCtxt)) => Some((e0, e1=>PostDecrement(e1)))
				}
				case PreIncrement(exp) => naOps.nestedAssignment(exp) match {
					case None => Some((e, x => x))
					case Some((e0, expCtxt)) => Some((e0, e1=>PreIncrement(e1)))
				}
				case PreDecrement(exp) => naOps.nestedAssignment(exp) match {
					case None => Some((e, x => x))
					case Some((e0, expCtxt)) => Some((e0, e1=>PreDecrement(e1)))
				}
				case PrePlus(exp) => naOps.nestedAssignment(exp) match {
					case None => None
					case Some((e0, expCtxt)) => Some((e0, e1=>PrePlus(e1)))
				}
				case PreMinus(exp) => naOps.nestedAssignment(exp) match {
					case None => None
					case Some((e0, expCtxt)) => Some((e0, e1=>PreMinus(e1)))
				} 
				case PreBitCompl(exp) => naOps.nestedAssignment(exp) match {
					case None => None
					case Some((e0, expCtxt)) => Some((e0, e1=>PreBitCompl(e1)))
				} 
				case PreNot(exp) => naOps.nestedAssignment(exp) match {
					case None => None
					case Some((e0, expCtxt)) => Some((e0, e1=>PreNot(e1)))
				}
				case Cast(ty,exp) => naOps.nestedAssignment(exp) match {
					case None => None
					case Some((e0, expCtxt)) => Some((e0, e1=>Cast(ty,e1)))
				}
				case BinOp(e1, op, e2) => naOps.nestedAssignment(e1) match {
					case None => naOps.nestedAssignment(e2) match {
						case None => None
						case Some((e2p, e2ctxt)) => Some((e2p, e => BinOp(e1, op, e2ctxt(e))))
					}
					case Some((e1p, e1ctxt)) => Some((e1p, e => BinOp(e1ctxt(e), op, e2)))
				}
				case InstanceOf(exp, ref_type) => naOps.nestedAssignment(exp) match {
					case None => None 
					case Some((ep, ectxt)) => Some((ep, e=>InstanceOf(ectxt(e), ref_type)))
				}
				case Cond(cond, true_exp, false_exp) => naOps.nestedAssignment(cond) match {
					case None => naOps.nestedAssignment(true_exp) match {
						case None => naOps.nestedAssignment(false_exp) match {
							case None => None
							case Some((false_exp_n, false_exp_ctxt)) => Some((false_exp_n, e=>Cond(cond, true_exp, false_exp_ctxt(e))))
						}
						case Some((true_exp_n, true_exp_ctxt)) => Some((true_exp_n, e=>Cond(cond, true_exp_ctxt(e), false_exp)))
					}
					case Some((cond_n, cond_ctxt)) => Some((cond_n, e=>Cond(cond_ctxt(e), true_exp, false_exp)))
				}
				case Assign(lhs@NameLhs(n), op, rhs) => naOps.nestedAssignment(rhs) match {
					case None => Some((e, x => x))
					case Some((rhs_n, rhs_ctxt)) => Some((rhs_n, e=>Assign(lhs, op, rhs_ctxt(e))))
				}
				case Assign(lhs, op, rhs) => naOps.nestedAssignment(lhs) match { // what about lhs is not a Name
					case None => naOps.nestedAssignment(rhs) match {
						case None => None
						case Some((rhs_n, rhs_ctxt)) => Some((rhs_n, e=>Assign(lhs, op, rhs_ctxt(e))))
					}
					case Some((lhs_n, lhs_ctxt)) => Some((lhs_n, e=>Assign(lhs_ctxt(e), op, rhs)))
				}
				case Lambda(params, body) => None // we don't need to check further, coz all unbound variables in lambda body must be final
				case MethodRef(name, id) => None
			}

	  }
	  implicit def LhsNestedAssignmentInstance:NestedAssignment[Lhs] = new NestedAssignment[Lhs] {
		  override def nestedAssignment(a: Syntax.Lhs): Option[(Syntax.Exp, Syntax.Exp => Syntax.Lhs)] = a match {
			  case NameLhs(name) => None
			  case FieldLhs(field_access) => naOps.nestedAssignment(field_access) match {
				  case None => None
				  case Some((field_access_p, field_access_ctxt)) => Some((field_access_p, e=>FieldLhs(field_access_ctxt(e))))
			  }
			  case ArrayLhs(array_idx) => naOps.nestedAssignment(array_idx) match {
				  case None => None
				  case Some((array_idx_p, array_idx_ctx)) => Some((array_idx_p, e => ArrayLhs(array_idx_ctx(e))))
			  }
		  }
	  }

	  implicit def FieldAccessAssignmentInstance:NestedAssignment[FieldAccess] = new NestedAssignment[FieldAccess] {
		  override def nestedAssignment(a: Syntax.FieldAccess): Option[(Syntax.Exp, Syntax.Exp => Syntax.FieldAccess)] = a match {
			  case PrimaryFieldAccess(e, id) => naOps.nestedAssignment(e) match {
				  case None => None 
				  case Some((e_p, ectxt)) => Some((e_p, e => PrimaryFieldAccess(ectxt(e), id)))
			  }
			  case SuperFieldAccess(id) => None
			  case ClassFieldAccess(name, id) => None
		  }
	  }

	  implicit def ArrayInitNestedAssignmentInstance:NestedAssignment[ArrayInit] = new NestedAssignment[ArrayInit] {
		  override def nestedAssignment(a: Syntax.ArrayInit): Option[(Syntax.Exp, Syntax.Exp => Syntax.ArrayInit)] = a match {
			  case ArrayInit(var_inits) => naOps.nestedAssignment(var_inits) match {
				  case None => None 
				  case Some((exp, ctxt)) => Some((exp, e => ArrayInit(ctxt(e))))
			  }
		  }
	  }

	  implicit def VarInitNestedAssignmentInstance:NestedAssignment[VarInit] = new NestedAssignment[VarInit] {
		  override def nestedAssignment(a: Syntax.VarInit): Option[(Syntax.Exp, Syntax.Exp => Syntax.VarInit)] = a match {
			  case InitExp(exp) => naOps.nestedAssignment(exp) match {
				  case None => None
				  case Some((e, ctxt)) => Some((e, e=>InitExp(ctxt(e))))
			  }
			  case InitArray(array_init) => naOps.nestedAssignment(array_init) match {
				  case None => None
				  case Some((e, ctxt)) => Some((e, e=>InitArray(ctxt(e))))
			  }
		  }
	  }

	  implicit def MethodInvocationNestedAssignmentInstance:NestedAssignment[MethodInvocation] = new NestedAssignment[MethodInvocation] {
		  override def nestedAssignment(a: Syntax.MethodInvocation): Option[(Syntax.Exp, Syntax.Exp => Syntax.MethodInvocation)] = a match {
			  case MethodCall(name, args) => naOps.nestedAssignment(args) match {
				  case None => None 
				  case Some((exp, ctxt)) => Some((exp, e => MethodCall(name, ctxt(e))))
			  }
			  case PrimaryMethodCall(e, ref_types, id, args) => naOps.nestedAssignment(e) match {
				  case None => naOps.nestedAssignment(args) match {
					  case None => None 
					  case Some((exp, ctxt)) => Some((exp, e1=> PrimaryMethodCall(e, ref_types, id, ctxt(e1))))
				  }
				  case Some((exp, ctxt)) => Some((exp, e1 => PrimaryMethodCall(ctxt(e1), ref_types, id, args)))
			  }
			  case SuperMethodCall(ref_types, id, args) => naOps.nestedAssignment(args) match {
				  case None => None 
				  case Some((exp, ctxt)) => Some((exp, e => SuperMethodCall(ref_types, id, ctxt(e))))
			  }
			  case ClassMethodCall(name, ref_types, id, args) => naOps.nestedAssignment(args) match {
				  case None => None 
				  case Some((exp, ctxt)) => Some((exp, e => ClassMethodCall(name, ref_types, id, ctxt(e))))
			  }
			  case TypeMethodCall(name, ref_types, id, args) => naOps.nestedAssignment(args) match {
				  case None => None 
				  case Some((exp, ctxt)) => Some((exp, e => TypeMethodCall(name, ref_types, id, ctxt(e))))
			  }
		  }
	  }
	  
	  implicit def ArrayIndexNestedAssignmentInstance:NestedAssignment[ArrayIndex] = new NestedAssignment[ArrayIndex] {
		  override def nestedAssignment(a: Syntax.ArrayIndex): Option[(Syntax.Exp, Syntax.Exp => Syntax.ArrayIndex)] = a match {
			  case ArrayIndex(e, es) => naOps.nestedAssignment(e) match {
				  case None => naOps.nestedAssignment(es) match {
					  case None => None
					  case Some((exp, ctxt)) => Some((exp, x => ArrayIndex(e, ctxt(x))))
				  }
				  case Some((exp, ctxt)) => Some((exp, x => ArrayIndex(ctxt(e), es)))
			  }
		  }
	  }

	  implicit def ListNestedAssignmentInstance[A](implicit na:NestedAssignment[A]):NestedAssignment[List[A]] = new NestedAssignment[List[A]] {
		  override def nestedAssignment(a: List[A]): Option[(Syntax.Exp, Syntax.Exp => List[A])] = a match {
			  case Nil => None 
			  case (x::xs) => na.nestedAssignment(x) match {
				  case None => nestedAssignment(xs) match {
					  case None => None 
					  case Some((exp, ctxt)) => Some((exp, e=>(x::ctxt(e))))
				  }
				  case Some((exp, ctxt)) => Some ((exp, e=>(ctxt(e)::xs)))
			  }
		  }
	  }
  }