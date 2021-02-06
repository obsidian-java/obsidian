package com.github.luzhuomi.obsidian



/**
  * We need a labelling step before the desugaring step (or even before the flattening step?) to loop / switch with continue and break statements
  * 
  * 1. all loop and switch statements must be explicitly label.
  * 2. all break statments must have a target label
  * 3. all continue statement must have a target label
  * 
  * consider the following 
  * 
  * switch (e) {
  *    case x:
  *      stmt1;
  *      break;
  *    ...
  * }
  * becomes
  * 
  * L1: switch(e) {
  *    case x:
  *     stmt1;
  *     break L1;
  * }
  * 
  * 
  * while (e) {
  *    stmt1;
  *    continue;
  * }
  * 
  * becomes
  * 
  * L2: while (e) {
  *    stmt1;
  *    continue L2;
  * }
  * 
  * 
  * A simple state monad should serve the purpose
  * 
  * 
  *  Wait! we need to keep track of two labels? because we might have a case
  * 
  * while (e) {
  *   switch (e2) {
  *       case c1:
  *        continue
  *   }
  * }
  * 
  * where we expect
  * 
  * 
  * l1: while  (e) {
  *    l2: switch (e2) {
  *         case c1:
  *           continue l1
  * }

  * 
  * 
  * We use the judgetment l_c, l_b |- stmt => stmt, 
  * l_c is a continuable label, l_b is a breakable label,   when we don't care a label in the context, we use _
  * 
  * 
  * -----------------------(BreakNone)
  * l_c, l_b |- break => break l_b
  * 
  *
  * -------------------------(BreakLabel)
  * l_c, l_b |- break l' => break l' 
  * 
  * -----------------------(ContinueNone)
  * l_c, l_b |- continue => continue l_c
  * 
  * 
  * -----------------------(ContinueLabel)
  * l_c, l_b |- continue l' => continue l'
  * 
  * 
  * l_b' is fresh
  * l_c, l_b' |- stmt1 => stmt1' ... l' |- stmtn => stmtn' 
  * ----------------------------------------------------------------------------------------------------------------------(SwitchNone)
  * l_c, _ |- switch e ( case c1: stmt1; ; case c2: stmt2; ... )  => l_b' : switch e ( case c1: stmt1'; ; case c2: stmt2'; ... ) 
  * 
  * l_c, l_b' |- stmt1 => stmt1' ... l' |- stmtn => stmtn' 
  * ----------------------------------------------------------------------------------------------------------------------(SwitchLabel)
  * l_c, _ |- l_b' : switch e ( case c1: stmt1; ; case c2: stmt2; ... )  => l_b' : switch e ( case c1: stmt1'; ; case c2: stmt2'; ... ) 
  * 
  * 
  * l' is fresh
  * l', l' |- stmt => stmt'
  * ----------------------------------------------------------------------------------(WhileNone)
  * _, _ |- while (e) { stmt } => while (e) { stmt' } => l' : while (e) { stmt' } 
  * 
  * 
  * l', l' |- stmt => stmt'
  * ----------------------------------------------------------------------------------(WhileLabel)
  * _, _ |- l' : while (e) { stmt } => while (e) { stmt' } => l' : while (e) { stmt' } 
  * 
  * --------------------(Assignment)
  * l |- x = e => x = e
  * 
  * 
  * l_c, l_b |- stmt1 => stmt1'  
  * l_c, l_b |- stmt2 => stmt2'
  * ------------------------------------------(Seq)
  * l_c, l_b |- stmt1; stmt2 => stmt1'; stmt2';
  * 
  * l_c, l_b |- stmt => stmt' 
  * ----------------------------------------(Label)  do we need this case, yes we need, but l' will be ignored by Java, no break or continue statement can refer to l'
  * l_c, l_b |- l' : stmt => l' : stmt'
  * 
  * l_c, l_b |- stmt1 => stmt1'
  * l_c, l_b |- stmt2 => stmt2'
  * l_c, l_b |- stmt3 => stmt3'
  * --------------------------------------------------------------------------------------------------------------------(Try)
  * l_c, l_b |- try { stmt1 } catch (T e) { stmt2 } finally {stmt3} => try { stmt1' } catch (T e) { stmt2' } finally {stmt3'}
  * 
  * 
  * 
  * */

import cats._
import cats.implicits._
import cats.data.StateT
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Syntax
import com.github.luzhuomi.obsidian.ASTUtils._
object Label {

  /**
    * State object of the label monad
    *
    * @param currNum - current unused number
    * @param labelPrefix - label prefix to be used in generating label
    */
  case class StateInfo(
    currNum:Int,
    labelPrefix:String 
  )


  val initStateInfo = StateInfo(0, "obsLbl")

  sealed trait LabelResult[+A] 
  case class LabelError(msg:String) extends LabelResult[Nothing]
  case class LabelOk[A](result:A) extends LabelResult[A]

  implicit def labelResultFunctor: Functor[LabelResult] = new Functor[LabelResult] {
    override def map[A, B](fa: LabelResult[A])(f: A => B): LabelResult[B] = fa match {
      case LabelError(s) => LabelError(s)
			case LabelOk(a)    => LabelOk(f(a))
    }
  }

  implicit def labelResultApplicative: ApplicativeError[LabelResult, String] = new ApplicativeError[LabelResult, String] {
    override def ap[A, B](ff: LabelResult[A => B])(fa: LabelResult[A]): LabelResult[B] = ff match {
			  case LabelOk(f) => fa match {
				  case LabelOk(a)    => LabelOk(f(a))
				  case LabelError(s) => LabelError(s)
			  }
			  case LabelError(s) => LabelError(s)
    }
    override def pure[A](a:A):LabelResult[A] = LabelOk(a)
    override def raiseError[A](e:String):LabelResult[A] = LabelError(e)
    override def handleErrorWith[A](fa: LabelResult[A])(f:String => LabelResult[A]) : LabelResult[A] = fa match {
			  case LabelError(s) => f(s)
			  case LabelOk(a)    => LabelOk(a)
		  }
  }

  implicit def labelResultMonadError(implicit app:ApplicativeError[LabelResult, String]): MonadError[LabelResult, String] = new MonadError[LabelResult, String] {
    override def flatMap[A, B](fa: LabelResult[A])(f: A => LabelResult[B]): LabelResult[B] = fa match {
			case LabelOk(a)    => f(a)
			case LabelError(s) => LabelError(s)
    }
    override def handleErrorWith[A](fa: LabelResult[A])(f: String => LabelResult[A]): LabelResult[A] = app.handleErrorWith(fa)(f)
		override def pure[A](x: A): LabelResult[A] = app.pure(x)
		override def raiseError[A](e: String): LabelResult[A] = app.raiseError(e)
		override def tailRecM[A, B](a: A)(f: A => LabelResult[Either[A,B]]): LabelResult[B] = f(a) match {
			case LabelError(msg)   => LabelError(msg)
			case LabelOk(Right(b)) => LabelOk(b)
			case LabelOk(Left(a))  => tailRecM(a)(f)
		}
  }

  def get: StateT[LabelResult, StateInfo, StateInfo] = StateT{ st => LabelOk(st, st) }
  def put(st:StateInfo): StateT[LabelResult, StateInfo, Unit] = StateT{ _ => LabelOk(st, ())}


  type LabelState[A] = StateT[LabelResult, StateInfo,A]


  def newLabel:LabelState[Ident] = for {
    st <- get
    _  <- put(st.copy(currNum= st.currNum+1))
  } yield Ident(s"${st.labelPrefix}_${st.currNum}")

  
  trait Label[A] {
    /**
      * 
      *
      * @param a
      * @param labelC - continable label
      * @param labelB - breakable label
      * @param m
      * @return
      */
    def label(a:A, labelB:Option[Ident], labelC:Option[Ident])(implicit m:MonadError[LabelState, String]):LabelState[A]
  }

  object labelOps {
    def label[A](a:A, labelB:Option[Ident], labelC:Option[Ident])(implicit d:Label[A], m:MonadError[LabelState, String]):LabelState[A] = {
      d.label(a,labelB, labelC)(m)
    }
  }

  implicit def methodDeclLabelInstance:Label[MethodDecl] = new Label[MethodDecl] {
    override def label(a: Syntax.MethodDecl, labelB:Option[Ident], labelC:Option[Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.MethodDecl] = a match {
      case MethodDecl(modifier, type_params, ty, id, formal_params, ex_types, exp, body) => for {
        l_body <- labelOps.label(body,labelB, labelC) 
      } yield MethodDecl(modifier, type_params, ty, id, formal_params, ex_types, exp, l_body)
    }
  }

  implicit def methodBodyLabelInstance:Label[MethodBody] = new Label[MethodBody] {
    override def label(a: Syntax.MethodBody, labelB:Option[Ident], labelC:Option[Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.MethodBody] = a match {
      case MethodBody(None) => m.pure(a)
      case MethodBody(Some(blk)) => for {
        l_blk      <- labelOps.label(blk, labelB, labelC)
      } yield MethodBody(Some(l_blk))
    }
  }

  implicit def blockLabelInstance:Label[Block] = new Label[Block] {
    override def label(a: Syntax.Block, labelB:Option[Ident], labelC:Option[Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.Block] = a match {
      case Block(blk_stmts) => for {
        l_blk_stmts <- blk_stmts.traverse(labelOps.label(_,labelB, labelC))
      } yield Block(l_blk_stmts)
    }
  }

  implicit def blockStmtLabelInstance:Label[BlockStmt] = new Label[BlockStmt] {
    override def label(a: Syntax.BlockStmt, labelB:Option[Ident], labelC:Option[Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.BlockStmt] = a match {
      case BlockStmt_(stmt) => for {
        l_stmt <- labelOps.label(stmt, labelB, labelC)
      } yield BlockStmt_(l_stmt)
      case LocalClass(class_decl) => m.pure(a) // we don't label nested class for now
      case LocalVars(modifiers, ty, var_decls) => m.pure(a)
    }
  }

  implicit def stmtLabelInstance:Label[Stmt] = new Label[Stmt] {
    override def label(a: Syntax.Stmt, labelB:Option[Ident], labelC:Option[Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.Stmt] = a match {
      case Assert(exp, msg) => m.pure(a)
      case Labeled(id, stmt) => stmt match {
        case BasicFor(init, loop_cond, post_update, stmt) => for {
          l_stmt <- labelOps.label(stmt, Some(id), Some(id))
        } yield Labeled(id, BasicFor(init, loop_cond, post_update, l_stmt))
        case EnhancedFor(modifiers, ty, id, exp, stmt) => for {
          l_stmt <- labelOps.label(stmt, Some(id), Some(id))
        } yield Labeled(id, EnhancedFor(modifiers, ty, id, exp, stmt))
        case Do(stmt, exp) => for {
          l_stmt <- labelOps.label(stmt, Some(id), Some(id))
        } yield Labeled(id, Do(l_stmt, exp))
        case While(exp, stmt) => for {
          l_stmt <- labelOps.label(stmt, Some(id), Some(id))
        } yield Labeled(id, While(exp, l_stmt))
        case Switch(exp, blocks) => for {
          l_blocks <- blocks.traverse(labelOps.label(_, Some(id), labelC))
        } yield Labeled(id, Switch(exp, l_blocks))
        case _ => for {
          l_stmt <- labelOps.label(stmt, labelB, labelC) 
        } yield Labeled(id, l_stmt)
      }
      case BasicFor(init, loop_cond, post_update, stmt) => for {
        id <- newLabel
        l_stmt <- labelOps.label(stmt, Some(id), Some(id))
      } yield Labeled(id, BasicFor(init, loop_cond, post_update, l_stmt))
      case EnhancedFor(modifiers, ty, id, exp, stmt) => for {
        id <- newLabel
        l_stmt <- labelOps.label(stmt, Some(id), Some(id))
      } yield Labeled(id, EnhancedFor(modifiers, ty, id, exp, stmt))
      case Do(stmt, exp) => for {
        id <- newLabel
        l_stmt <- labelOps.label(stmt, Some(id), Some(id))
      } yield Labeled(id, Do(l_stmt, exp))
      case While(exp, stmt) => for {
        id <- newLabel
        l_stmt <- labelOps.label(stmt, Some(id), Some(id))
      } yield Labeled(id, While(exp, l_stmt))
      case Switch(exp, blocks) => for {
        id <- newLabel
        l_blocks <- blocks.traverse(labelOps.label(_, Some(id), labelC))
      } yield Labeled(id, Switch(exp, l_blocks))
      case Break(mid) => mid match {
        case None => labelB match { 
          case None => m.raiseError("Labelling failed. An orphan break statement is encountered,")
          case Some(_) => m.pure(Break(labelB))
        }
        case Some(id) => m.pure(Break(mid))
      }
      case Continue(mid) => mid match {
        case None => labelC match {
          case None => m.raiseError("Labelling failed. An orphan continue statement is encountered.")
          case Some(_) => m.pure(Continue(labelC))
        }
        case Some(id) => m.pure(Continue(mid))
      }
      case Empty => m.pure(Empty)
      case ExpStmt(exp) => m.pure(ExpStmt(exp))
      case IfThen(exp, stmt) => for {
        l_stmt <- labelOps.label(stmt, labelB, labelC)
      } yield IfThen(exp, l_stmt)
      case IfThenElse(exp, then_stmt, else_stmt) => for {
        l_then_stmt <- labelOps.label(then_stmt, labelB, labelC)
        l_else_stmt <- labelOps.label(else_stmt, labelB, labelC)
      } yield IfThenElse(exp, l_then_stmt, l_else_stmt)
      case Return(exp) => m.pure(Return(exp))
      case StmtBlock(blk) => for {
        l_blk <- labelOps.label(blk, labelB, labelC)
      } yield StmtBlock(l_blk)
      case Synchronized(exp, blk) => for {
        l_blk <- labelOps.label(blk, labelB, labelC)
      } yield Synchronized(exp, l_blk)
      case Throw(exp) => m.pure(Throw(exp))
      case Try(try_blk, catches, finally_blk) => for {
        l_try_blk <- labelOps.label(try_blk, labelB, labelC) 
        l_catches <- catches.traverse(labelOps.label(_, labelB, labelC))
        l_finally_blk <- finally_blk.traverse(labelOps.label(_, labelB, labelC))
      } yield Try(l_try_blk, l_catches, l_finally_blk)
    }
 
    implicit def switchBlockLabelInstance:Label[SwitchBlock] = new Label[SwitchBlock] {
      override def label(a: Syntax.SwitchBlock, labelB:Option[Ident], labelC:Option[Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.SwitchBlock] = a match {
        case SwitchBlock(sw_label, blk_stmts) => for {
          l_blk_stmts <- blk_stmts.traverse(labelOps.label(_, labelB, labelC))
        } yield SwitchBlock(sw_label, l_blk_stmts)
      }
    }

    implicit def catchLabelInstance:Label[Catch] = new Label[Catch] {
      override def label(a: Syntax.Catch, labelB: Option[Syntax.Ident], labelC: Option[Syntax.Ident])(implicit m: MonadError[LabelState,String]): LabelState[Syntax.Catch] = a match {
        case Catch(params, blk) => for {
          l_blk <- labelOps.label(blk, labelB, labelC) 
        } yield Catch(params, l_blk)
      }
    }
  }
}