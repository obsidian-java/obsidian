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
  * ----------------------------------------(Label)  do we need this case?
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

  case class StateInfo(
    currNum:Int
  )


  val initStateInfo = StateInfo(0)

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

}