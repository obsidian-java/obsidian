package com.github.luzhuomi.obsidian

import cats._
import cats.implicits._
import cats.data.StateT
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Syntax
import com.github.luzhuomi.obsidian.ASTUtils._



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
	  /**
		* StateInfo - the state object for flattening monad
		*
		* @param currNum - the running number for creating fresh variable names
		* @param nameSecret - the secret string to create fresh variable names
		* @param typeEnv - a mini type mapping for numerical local variables that are renamed.
		* @param renaming - the mapping from renamed variable to original name
		*/
	  case class StateInfo(
		  currNum:Int,
		  nameSecret:String,
		  typeEnv:Map[Name,(List[Modifier],Type)],
		  renamed:Map[Name,Name] 
	  )

	  val initStateInfo= StateInfo(0,"_is__flattened_", Map(), Map())

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

	  def flatMethodDecl(a:MethodDecl)(implicit m:MonadError[FlatState, String]):FlatState[MethodDecl] = a match { 
		  case MethodDecl(modifier, type_params, ty, id, formal_params, ex_types, exp, body) => for {
			  _      <- formal_params.traverse_(addFormalParamToTypeEnv(_))
			  f_body <- flatMethodBody(body)
		  } yield MethodDecl(modifier, type_params, ty, id, formal_params, ex_types, exp, f_body)
	  }

	  def flatMethodBody(a:MethodBody)(implicit m:MonadError[FlatState, String]):FlatState[MethodBody] = a match { 
		  case MethodBody(None) => m.pure(a)
		  case MethodBody(Some(blk)) => for {
			  f_blk          <- flatBlock(blk)
			  temp_var_decls <- tempVarDecls()
			  // add the declaration for the renamed variables.
		  } yield f_blk match {
			  case Block(blk_stmts) => 	MethodBody(Some(Block(temp_var_decls ++ blk_stmts)))
		  }
	  }

	  def tempVarDecls()(implicit m:MonadError[FlatState, String]):FlatState[List[BlockStmt]] = for {
		  st <- get
		  r  <- {
			  val tyEnv = st.typeEnv
			  def go(nn:(Name,Name))(implicit m:MonadError[FlatState, String]):FlatState[BlockStmt] = nn match {
				  case (new_name@Name(ids), ori_name) => tyEnv.get(ori_name) match {
					  case None => m.raiseError(s"Flatten failed: unable to identify the type of ${ori_name}!")
					  case Some((mods,ty)) => ids match {
						  case Nil => m.raiseError(s"Flatten failed: unable to extract ID from new name ${new_name}!")
						  case _   => m.pure(LocalVars(mods,ty,List(VarDecl(VarId(ids.last), None))))
						}
					}
				}
				st.renamed.toList.traverse(go)
			}
		} yield r 
	
	  def flatBlock(a:Block)(implicit m:Monad[FlatState]):FlatState[Block] = a match {
		  case Block(blk_stmts) => for {
			  f_blk_stmts <- blk_stmts.traverse(flatBlockStmt(_))
		  } yield Block(f_blk_stmts.flatMap(x => x)) 
	  }

	  def flatStmt(a:Stmt)(implicit m:MonadError[FlatState, String]):FlatState[List[Stmt]] =  a match {

		  case IfThen(exp, stmt) => for {
			  stmts_exp <- laOps.liftAll(exp);
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

		  case IfThenElse(exp, then_stmt, else_stmt) => for {
			  stmts_exp <- laOps.liftAll(exp)
			  r <- stmts_exp match {
				  case (Nil, _) => { // nothing to be flattened in exp 
					(then_stmt, else_stmt) match {
						case (StmtBlock(then_blk), StmtBlock(else_blk)) => for {
							f_then_blk <- flatBlock(then_blk)
							f_else_blk <- flatBlock(else_blk)
						} yield List(IfThenElse(exp, StmtBlock(f_then_blk), StmtBlock(f_else_blk)))
						case (StmtBlock(then_blk), _) => for {
							f_then_blk <- flatBlock(then_blk) 
							f_stmts    <- flatStmt(else_stmt) 
						} yield { 
							val f_else_blk = Block(f_stmts.map(BlockStmt_(_)))
							List(IfThenElse(exp, StmtBlock(f_then_blk), StmtBlock(f_else_blk))) 
						}
						case (_, StmtBlock(else_blk)) => for {
							f_stmts    <- flatStmt(then_stmt)
							f_else_blk <- flatBlock(else_blk)
						} yield {
							val f_then_blk = Block(f_stmts.map(BlockStmt_(_)))
							List(IfThenElse(exp, StmtBlock(f_then_blk), StmtBlock(f_else_blk)))
						}
						case (_, _) => for {
							f_then_stmts <- flatStmt(then_stmt)
							f_else_stmts <- flatStmt(else_stmt)
						} yield {
							val f_then_blk = Block(f_then_stmts.map(BlockStmt_(_)))
							val f_else_blk = Block(f_else_stmts.map(BlockStmt_(_)))
							List(IfThenElse(exp, StmtBlock(f_then_blk), StmtBlock(f_else_blk)))
						}
					}
				  }
				  case (stmts, expFinal) => {
					(then_stmt, else_stmt) match {
						case (StmtBlock(then_blk), StmtBlock(else_blk)) => for {
							f_then_blk <- flatBlock(then_blk)
							f_else_blk <- flatBlock(else_blk)
						} yield stmts ++ List(IfThenElse(expFinal, StmtBlock(f_then_blk), StmtBlock(f_else_blk)))
						case (StmtBlock(then_blk), _) => for {
							f_then_blk <- flatBlock(then_blk) 
							f_stmts    <- flatStmt(else_stmt) 
						} yield { 
							val f_else_blk = Block(f_stmts.map(BlockStmt_(_)))
							stmts ++ List(IfThenElse(expFinal, StmtBlock(f_then_blk), StmtBlock(f_else_blk))) 
						}
						case (_, StmtBlock(else_blk)) => for {
							f_stmts    <- flatStmt(then_stmt)
							f_else_blk <- flatBlock(else_blk)
						} yield {
							val f_then_blk = Block(f_stmts.map(BlockStmt_(_)))
							stmts ++ List(IfThenElse(expFinal, StmtBlock(f_then_blk), StmtBlock(f_else_blk)))
						}
						case (_, _) => for {
							f_then_stmts <- flatStmt(then_stmt)
							f_else_stmts <- flatStmt(else_stmt)
						} yield {
							val f_then_blk = Block(f_then_stmts.map(BlockStmt_(_)))
							val f_else_blk = Block(f_else_stmts.map(BlockStmt_(_)))
							stmts ++ List(IfThenElse(expFinal, StmtBlock(f_then_blk), StmtBlock(f_else_blk)))
						}
					}
				  
				  }
			  }
		  } yield r

		  case Assert(exp, msg) => for {
			  stmts_exp <- laOps.liftAll(exp);
			  r <- stmts_exp match {
				  case (Nil, _) => m.pure(List(Assert(exp, msg)))
				  case (stmts, expFinal) => m.pure(stmts ++ List(Assert(expFinal, msg)))
			  }
		  } yield r

		  case BasicFor(init, loop_cond, post_update, stmt) => { 
			  // we move the post update expressions into the body before apply flattening
			  val post_update_stmts:List[Stmt] = post_update match {
				  case None => List()
				  case Some(exps) => exps.map(ExpStmt(_))
			  }
			  val block_stmts_p:List[BlockStmt] = stmt match { // appending post update statement
				  case StmtBlock(Block(block_stmts)) => block_stmts ++  post_update_stmts.map(BlockStmt_(_)) 
				  case _ => List(BlockStmt_(stmt)) ++ post_update_stmts.map(BlockStmt_(_))
			  }
			  for {
				stmts_init <- laOps.liftAll(init) 
				stmts_loop_cond <- laOps.liftAll(loop_cond)
				f_blk <- flatBlock(Block(block_stmts_p)) // the new body append with the post update statement
		
		  		} yield {
					val stmtp = appBlockStmts(StmtBlock(f_blk), stmts_loop_cond._1.map(BlockStmt_(_))) // append the flattened stmts from the cond exp to the body
					stmts_init._1 ++ stmts_loop_cond._1 ++ List(BasicFor(stmts_init._2, stmts_loop_cond._2, None, stmtp))
				}
		  }

		  case Break(id) => m.pure(List(Break(id)))

		  case Continue(id) => m.pure(List(Continue(id)))

		  case Do(stmt@StmtBlock(blk), exp) => for {
			  f_blk     <- flatBlock(blk)
			  stmts_exp <- laOps.liftAll(exp)
		  } yield {
			  val stmtp = appBlockStmts(StmtBlock(f_blk), stmts_exp._1.map(BlockStmt_(_)))
			  List(Do(stmtp, stmts_exp._2))
		  }

		  case Do(stmt, exp) => for {
			  f_stmts   <- flatStmt(stmt)
			  stmts_exp <- laOps.liftAll(exp) 
		  } yield {
			  val stmtsp = StmtBlock(Block((f_stmts ++ stmts_exp._1).map(BlockStmt_(_))))
			  List(Do(stmtsp, stmts_exp._2))
		  }

		  case Empty => m.pure(List(Empty))

		  case EnhancedFor(modifiers, ty, id, exp, stmt@StmtBlock(blk)) => for {
			  _ <- addVarDeclsToTypeEnv(List(VarDecl(VarId(id), None)), modifiers, ty)
			  stmts_exp <- laOps.liftAll(exp) 
			  f_blk     <- flatBlock(blk)
		  } yield {
			  val stmtp = appBlockStmts(StmtBlock(f_blk), stmts_exp._1.map(BlockStmt_(_)))
			  stmts_exp._1 ++ List(EnhancedFor(modifiers, ty, id, stmts_exp._2, stmtp))
		  }

		  case EnhancedFor(modifiers, ty, id, exp, stmt) => for {
			  _ <- addVarDeclsToTypeEnv(List(VarDecl(VarId(id), None)), modifiers, ty)
			  stmts_exp <- laOps.liftAll(exp)
			  f_stmts   <- flatStmt(stmt) 
		  } yield {
			  val stmtsp  = StmtBlock(Block((f_stmts ++ stmts_exp._1).map(BlockStmt_(_))))
			  stmts_exp._1 ++ List(EnhancedFor(modifiers, ty, id, stmts_exp._2, stmtsp))
		  }

		  case ExpStmt(exp) => exp match {
			  case ArrayAccess(ArrayIndex(e, es)) => for {
				  stmts_e  <- laOps.liftAll(e)
				  stmts_es <- laOps.liftAll(es)  
			  } yield stmts_e._1 ++ stmts_es._1 ++ List(ExpStmt(ArrayAccess(ArrayIndex(stmts_e._2, stmts_es._2))))

			  case Cast(ty, exp) => m.pure(List(ExpStmt(exp)))

			  case ArrayCreate(ty, exps, num_dims) => for {
				  stmts_exps <- laOps.liftAll(exps)
			  } yield stmts_exps._1 ++ List(ExpStmt(ArrayCreate(ty, stmts_exps._2, num_dims)))

			  case ArrayCreateInit(ty, size, init) => for {
				  stmts_init <- laOps.liftAll(init) 
			  } yield stmts_init._1 ++ List(ExpStmt(ArrayCreateInit(ty,size, stmts_init._2)))
			  
			  case Assign(lhs, op, rhs) => for {
				  stmts_lhs <- laOps.liftAll(lhs)
				  stmts_rhs <- laOps.liftAll(rhs) 
			  } yield stmts_lhs._1 ++ stmts_rhs._1 ++ List(ExpStmt(Assign(stmts_lhs._2, op, stmts_rhs._2)))
			  
			  case BinOp(e1, op, e2) => for {
				  stmts_e1 <- laOps.liftAll(e1) 
				  stmts_e2 <- laOps.liftAll(e2)
			  } yield stmts_e1._1 ++ stmts_e2._1 ++ List(ExpStmt(BinOp(stmts_e1._2, op, stmts_e2._2)))
			  
			  case ClassLit(ty) => m.pure(List(ExpStmt(exp)))
			  
			  case Cond(cond, true_exp, false_exp) => for {
				  stmts_cond <- laOps.liftAll(cond)
				  stmts_true_exp <- laOps.liftAll(true_exp) 
				  stmts_false_exp <- laOps.liftAll(false_exp)
			  } yield stmts_cond._1 ++ stmts_true_exp._1 ++ stmts_false_exp._1 ++ List(ExpStmt(Cond(stmts_cond._2, stmts_true_exp._2, stmts_false_exp._2)))
			  
			  case ExpName(name) => m.pure(List(ExpStmt(exp)))
			  
			  case FieldAccess_(access) => m.pure(List(ExpStmt(exp)))
			  
			  case InstanceCreation(type_args, type_decl, args, body) => for {
				  stmts_args <- laOps.liftAll(args)
			  } yield stmts_args._1 ++ List(ExpStmt(InstanceCreation(type_args, type_decl, stmts_args._2, body)))
			 
			  case InstanceOf(e, ref_type) => for {
				  stmts_e <- laOps.liftAll(e) 
			  } yield stmts_e._1 ++ List(ExpStmt(InstanceOf(stmts_e._2, ref_type)))
			  
			  case Lambda(params, body) => m.pure(List(ExpStmt(exp)))
			  
			  case Lit(lit) => m.pure(List(ExpStmt(exp)))
			  
			  case MethodInv(methodInv) => for {
				  stmts_methodInv <- laOps.liftAll(methodInv) 
			  } yield stmts_methodInv._1 ++ List(ExpStmt(MethodInv(stmts_methodInv._2)))
			  
			  case MethodRef(name, id) => m.pure(List(ExpStmt(exp)))
			  
			  case PostIncrement(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++  List(ExpStmt(PostIncrement(stmts_exp._2)))
			  
			  case PostDecrement(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++  List(ExpStmt(PostDecrement(stmts_exp._2)))
			  
			  case PreIncrement(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++  List(ExpStmt(PreIncrement(stmts_exp._2)))
			  
			  case PreDecrement(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++  List(ExpStmt(PreDecrement(stmts_exp._2)))
			  
			  case PreBitCompl(exp) => for {
				  stmts_exp <- laOps.liftAll(exp) 
			  } yield stmts_exp._1 ++ List(ExpStmt(PreBitCompl(stmts_exp._2)))
			  
			  case PreMinus(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++ List(ExpStmt(PreMinus(stmts_exp._2)))
			  
			  case PreNot(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++ List(ExpStmt(PreNot(stmts_exp._2)))
			  
			  case PrePlus(exp) => for {
				  stmts_exp <- laOps.liftAll(exp)
			  } yield stmts_exp._1 ++ List(ExpStmt(PrePlus(stmts_exp._2)))
			  
			  case QualInstanceCreation(exp, type_args, id, args, body) => for {
				  stmts_exp <- laOps.liftAll(exp)
				  stmts_args <- laOps.liftAll(args)
			  } yield stmts_exp._1 ++ stmts_args._1 ++ List(ExpStmt(QualInstanceCreation(stmts_exp._2, type_args, id, stmts_args._2, body)))
			  
			  case ThisClass(name) => m.pure(List(ExpStmt(exp)))
			  
			  case This => m.pure(List(ExpStmt(exp)))
		  }
		  
		  case StmtBlock(blk) => for { f_blk <- flatBlock(blk) } yield List(StmtBlock(f_blk))
		  
		  case Switch(exp, blocks) => for {
			  stmts_exp <- laOps.liftAll(exp) 
			  flat_blocks <- blocks.traverse(flatSwitchBlock(_))
		  } yield stmts_exp._1 ++  List(Switch(stmts_exp._2, flat_blocks))
		  
		  case Labeled(id, stmt) => for {
			f_stmts <- flatStmt(stmt)
		  } yield {
			  f_stmts match {
				  case Nil => Nil
				  case (s::ss) => Labeled(id,s)::ss
			  }
		  }
		  
		  case Return(exp) => for {
			  stmts_exp <- laOps.liftAll(exp)
		  } yield stmts_exp._1 ++ List(Return(stmts_exp._2))

		  case Throw(exp) => for {
			  stmts_exp <- laOps.liftAll(exp)
		  } yield stmts_exp._1 ++ List(Throw(stmts_exp._2))

		  case Synchronized(exp, blk) => m.raiseError("Flattening failed: Synchronized statement is not supported.")
		  
		  case Try(try_blk, catches, finally_blk) => for {
			  f_try_blk <- flatBlock(try_blk)
			  f_catches <- catches.traverse(flatCatch(_))
			  f_finally_blk <- finally_blk.traverse(flatBlock(_))
		  } yield List(Try(f_try_blk, f_catches, f_finally_blk))
 
		  case While(exp, stmt@StmtBlock(blk)) => for {
			  stmts_exp <- laOps.liftAll(exp) // the flattened stmts from exp need to be placed before the while loop and at the end of the while loop body
			  f_blk     <- flatBlock(blk) 
		  } yield {
			  val stmtp = appBlockStmts(StmtBlock(f_blk), stmts_exp._1.map(BlockStmt_(_)))
			  stmts_exp._1 ++ List(While(stmts_exp._2, stmtp))
		  }

		  case While(exp, stmt) => for {
			  stmts_exp <- laOps.liftAll(exp) 
			  f_stmts   <- flatStmt(stmt) 
		  } yield {
			  val stmtsp = StmtBlock(Block((f_stmts ++ stmts_exp._1).map(BlockStmt_(_))))
			  stmts_exp._1 ++ List(While(stmts_exp._2, stmtsp))
		  }
	  }

	  def flatSwitchBlock(switch_block:SwitchBlock)(implicit m:Monad[FlatState]):FlatState[SwitchBlock] = switch_block match {
		  case SwitchBlock(label, blk_stmts) => for {
			  f_blk_stmts <- blk_stmts.traverse(flatBlockStmt(_))
		  } yield SwitchBlock(label, f_blk_stmts.flatMap(x => x))
	  }

	  def flatBlockStmt(blk_stmt:BlockStmt)(implicit m:Monad[FlatState]):FlatState[List[BlockStmt]] = blk_stmt match {
		  case BlockStmt_(stmt) => for {
			  f_stmts <- flatStmt(stmt)
		  } yield f_stmts.map(BlockStmt_(_))
		  case LocalClass(class_decl) => m.pure(List(blk_stmt))
		  case LocalVars(modifiers, ty, var_decls) => for {
			  _ <- addVarDeclsToTypeEnv(var_decls, modifiers, ty)
			  stmts_var_decls <- laOps.liftAll(var_decls)
		  } yield stmts_var_decls._1.map(BlockStmt_(_)) ++ List(LocalVars(modifiers, ty, stmts_var_decls._2))
	  }

	  def flatCatch(c:Catch)(implicit m:Monad[FlatState]):FlatState[Catch] = c match {
		  case Catch(params, blk) => for {
			  f_blk <- flatBlock(blk)
		  } yield Catch(params,f_blk)
	  } 

	  trait LiftAll[A] {
		/**
		* liftAll - lift all nested assignment expression from the input construct, turns them into simple assignment statements of shape (x = nested_assignment). 
		*   applying lhs xs back to their repsective context to form the flattened construct
		*
		* @param a input language  construct, e.g. an expression
		* @return a list of simple assignment statements, and the flattened expression or other construct.
		*/
		  def liftAll(a:A)(implicit m:MonadError[FlatState, String]):FlatState[(List[Stmt], A)]
	  }

	  object laOps {
		  def liftAll[A](a:A)(implicit la:LiftAll[A], m:MonadError[FlatState, String]):FlatState[(List[Stmt], A)] = {
			  la.liftAll(a)(m)
		  }
	  }

	  implicit def ListLiftAllInstance[A](implicit la:LiftAll[A]):LiftAll[List[A]] = new LiftAll[List[A]] {
		  override def liftAll(as: List[A])(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], List[A])] = {
			def go(stmts_as_acc:(List[Stmt], List[A]), x:A):FlatState[(List[Stmt], List[A])] = { 
				val stmts_acc = stmts_as_acc._1
				val exps_acc = stmts_as_acc._2 
				for {
					stmts_exp <- la.liftAll(x)
				} yield (stmts_acc ++ stmts_exp._1, exps_acc ++ List(stmts_exp._2))
			}
		  val empty_stmts:List[Stmt] = List()
		  val empty_exps:List[A] = List()
		  as.foldM((empty_stmts,empty_exps))(go)(m)
		  }
	  }

	  implicit def OptionLiftAllInstance[A](implicit la:LiftAll[A]):LiftAll[Option[A]] = new LiftAll[Option[A]] {
		  override def liftAll(a: Option[A])(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Option[A])] = a match {
			  case None => m.pure((List(), None))
			  case Some(x) => for {
				  stmts_y <- la.liftAll(x) 
			  } yield (stmts_y._1, Some(stmts_y._2))
		  }
	  }
	
	  implicit def ExpLiftAllInstance:LiftAll[Exp] = new LiftAll[Exp] {
		  override def liftAll(a: Syntax.Exp)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.Exp)] = {
			def go(exp:Exp, acc:List[Stmt]):FlatState[(List[Stmt], Exp)] = naOps.nestedAssignment(exp) match {
				case None => m.pure((acc,exp))
				case Some((e, ctxt)) => for {
					n_stmt <- mkAssignStmt(e)(m);
					r <- go(ctxt(ExpName(n_stmt._1)), acc++List(n_stmt._2))
				} yield r
			}
			go(a,List())
		}
	  }

	  implicit def ForInitLiftAllInstance:LiftAll[ForInit] = new LiftAll[ForInit] {
		  override def liftAll(a: Syntax.ForInit)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.ForInit)] = a match {
			  case ForInitExps(exps) => for { 
				  stmts_exps <- laOps.liftAll(exps) 
			  } yield (stmts_exps._1, ForInitExps(stmts_exps._2))
			  case ForLocalVars(modifiers, ty, var_decls) => for {
				  _ <- addVarDeclsToTypeEnv(var_decls, modifiers, ty) // update the type environment
				  stmts_var_decls <- laOps.liftAll(var_decls)
			  } yield (stmts_var_decls._1, ForLocalVars(modifiers, ty, stmts_var_decls._2))
		  }
	  }

	  implicit def VarDeclLiftAllInstance:LiftAll[VarDecl] = new LiftAll[VarDecl] {
		  override def liftAll(a: Syntax.VarDecl)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.VarDecl)] = a match {
			  case VarDecl(id, var_init) => for {
				  stmts_var_init <- laOps.liftAll(var_init) 
			  } yield (stmts_var_init._1, VarDecl(id, stmts_var_init._2))
		  }
	  }

	  implicit def VarInitLiftAllInstance:LiftAll[VarInit] = new LiftAll[VarInit] {
		  override def liftAll(a: Syntax.VarInit)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.VarInit)] = a match {
			  case InitArray(array_init) => for {
				  stmts_array_init <- laOps.liftAll(array_init)
			  } yield (stmts_array_init._1, InitArray(stmts_array_init._2))
			  case InitExp(exp) => for {
				  stmts_exp <- laOps.liftAll(exp) 
			  } yield (stmts_exp._1, InitExp(stmts_exp._2))
		  }
	  }

	  implicit def ArrayInitLiftAllInstance:LiftAll[ArrayInit] = new LiftAll[ArrayInit] {
		  override def liftAll(a: Syntax.ArrayInit)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.ArrayInit)] = a match {
			  case ArrayInit(var_inits) => for {
				  stmts_var_inits <- laOps.liftAll(var_inits)
			  } yield (stmts_var_inits._1, ArrayInit(stmts_var_inits._2))
		  }
	  }

	  implicit def LhsLiftAllInstance:LiftAll[Lhs] = new LiftAll[Lhs] {
		  override def liftAll(a: Syntax.Lhs)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.Lhs)] = a match {
			  case NameLhs(name) => m.pure((List(), a))
			  case ArrayLhs(array_idx) => for {
				  stmts_array_idx <- laOps.liftAll(array_idx)
			  } yield (stmts_array_idx._1, ArrayLhs(stmts_array_idx._2))
			  case FieldLhs(field_access) => m.pure(List(), a)
		  }
	  }

	  implicit def ArrayIndexLiftAllInstance:LiftAll[ArrayIndex] = new LiftAll[ArrayIndex] {
		  override def liftAll(a: Syntax.ArrayIndex)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.ArrayIndex)] = a match {
			  case ArrayIndex(e, es) => for {
				  stmts_e <- laOps.liftAll(e) 
				  stmts_es <- laOps.liftAll(es)
			  } yield (stmts_e._1 ++ stmts_es._1, ArrayIndex(stmts_e._2, stmts_es._2))
		  }
	  }

	  implicit def MethodInvLiftAllInstance:LiftAll[MethodInv] = new LiftAll[MethodInv] {
		  override def liftAll(a: Syntax.MethodInv)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.MethodInv)] = a match {
			  case MethodInv(methodInv) => for {
				  stmts_methodInv <- laOps.liftAll(methodInv) 
			  } yield (stmts_methodInv._1, MethodInv(stmts_methodInv._2))
		  }
	  }

	  implicit def MethodInvocationLiftAllInstance:LiftAll[MethodInvocation] = new LiftAll[MethodInvocation] {
		  override def liftAll(a: Syntax.MethodInvocation)(implicit m: MonadError[FlatState,String]): FlatState[(List[Syntax.Stmt], Syntax.MethodInvocation)] = a match {
			  case ClassMethodCall(name, ref_types, id, args) => for {
				  stmts_args <- laOps.liftAll(args) 
			  } yield (stmts_args._1, ClassMethodCall(name, ref_types, id, stmts_args._2)) 
			  case MethodCall(name, args) => for {
				  stmts_args <- laOps.liftAll(args)
			  } yield (stmts_args._1, MethodCall(name, stmts_args._2))
			  case PrimaryMethodCall(e, ref_types, id, args) => for {
				  stmts_e <- laOps.liftAll(e)
				  stmts_args <- laOps.liftAll(args)
			  } yield (stmts_e._1 ++ stmts_args._1, PrimaryMethodCall(stmts_e._2, ref_types, id, stmts_args._2))
			  case SuperMethodCall(ref_types, id, args) => for {
				  stmts_args <- laOps.liftAll(args)
			  } yield (stmts_args._1, SuperMethodCall(ref_types, id, stmts_args._2))
			  case TypeMethodCall(name, ref_types, id, args) => for {
				  stmts_args <- laOps.liftAll(args)
			  } yield (stmts_args._1, TypeMethodCall(name, ref_types, id, stmts_args._2) )
		  }
	  }


	  def addFormalParamToTypeEnv(formal_param:FormalParam)(implicit m: MonadError[FlatState,String]):FlatState[Unit] = formal_param match {
		  case FormalParam(modifiers, ty, has_arity, VarId(id)) => for {
			st <- get
			_  <- put(st.copy(
				typeEnv=st.typeEnv + (Name(List(id)) -> (modifiers, ty))
			))
		  } yield ()
		  case FormalParam(modifiers, ty, has_arity, _) => m.pure(()) // we skip array args since they can be ++/--
		}

	  def addVarDeclsToTypeEnv(var_decls:List[VarDecl], mods:List[Modifier], ty:Type)(implicit m: MonadError[FlatState,String]):FlatState[Unit] = for {
		  st <- get
		  _  <- put(st.copy(
			  typeEnv=var_decls.foldLeft(st.typeEnv)( (tyEnv, var_decl) => var_decl match {
				  case VarDecl(vdid,_) => vdid match {
					  case VarId(id) => tyEnv + (Name(List(id)) -> (mods, ty))
					  case _ => tyEnv
				  }
			  }) 
		  ))
	  } yield ()

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
					  np <- {
						  val nextNum = st.currNum+1
						  val newName =  Name(inits++List(Ident(s"${s}${st.nameSecret}${st.currNum}")))
						  val renamed1 = st.renamed + (newName -> n)
						  for {
							_ <- put(st.copy(currNum=nextNum, renamed = renamed1)) // insert the new name to the mappnig
						  } yield newName
					  }
				  } yield np
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