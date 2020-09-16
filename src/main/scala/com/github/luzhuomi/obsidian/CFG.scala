package com.github.luzhuomi.obsidian

import cats._
import cats.data.StateT

import com.github.luzhuomi.scalangj.Syntax._
import java.beans.Expression
/*
 Control Flow Graph construction
 */

object CFG {

  type CFG = Map[NodeId, Node]

  /**
    * Node in a control flow graph
    *
    * @param stmts the list of statments contained in this node
    * @param lVars variables appearing on the left hand side of assignment statement
    * @param rVars variables appearing on the right hand side of assignment statement
    * @param localDecls locally declared variables
    * @param preds predecessor nodes
    * @param succs successor nodes
    * @param nodeType node type, assignments, loop, switch, if-else, try-catch or throw
    */
  case class Node(
      stmts: List[BlockStmt],
      lVars: List[Ident],
      rVars: List[Ident],
      localDecls: List[Ident],
      preds: List[NodeId],
      succs: List[NodeId],
      nodeType: NodeType
  )

  type NodeId = Ident

  sealed trait NodeType

  case object AssignmentNode extends NodeType
  case object LoopNode extends NodeType
  case object SwitchNode extends NodeType
  case object IfElseNode extends NodeType
  case object TryCatchNode extends NodeType
  case object ThrowNode extends NodeType

  case class StateInfo(
      currId: Int,
      cfg: CFG,
      currPreds: List[NodeId],
      continuable: Boolean,
      contNodes: List[
        NodeId
      ], // it seems some of these are needed to support duff's device, which is only doable in C.
      breakNodes: List[NodeId],
      caseNodes: List[CaseExp],
      formalArgs: List[Ident],
      fallThroughCases: List[Exp]
  )

  sealed trait CaseExp
  case class DefaultCase(wrapperId: NodeId, rhs: NodeId)
  case class ExpCase(
      condExp: Exp,
      fallThrough: List[Exp],
      wrapperId: NodeId,
      rhs: NodeId
  )

  val labPref = "myLabel"
  val initStateInfo = StateInfo(
    0,
    Map[NodeId, Node](),
    List(),
    false,
    List(),
    List(),
    List(),
    List(),
    List()
  )

  sealed trait CFGResult[+A]
  case class CFGError(msg: String) extends CFGResult[Nothing]
  case class CFGOk[A](result: A) extends CFGResult[A]

  implicit def cfgResultFunctor: Functor[CFGResult] = new Functor[CFGResult] {
    override def map[A,B](fa:CFGResult[A])(f:A=>B): CFGResult[B] = fa match {
      case CFGError(s) => CFGError(s)
      case CFGOk(a) => CFGOk(f(a))
    }
  } 

  implicit def cfgResultApplicative: Applicative[CFGResult] = new Applicative[CFGResult] {
    override def ap[A,B](ff:CFGResult[A=>B])(fa:CFGResult[A]) : CFGResult[B] = ff match {
      case CFGOk(f) => fa match {
        case CFGOk(a) => CFGOk(f(a))
        case CFGError(s) => CFGError(s)
      }
      case CFGError(s) => CFGError(s)
    }

    override def pure[A](a:A):CFGResult[A] = CFGOk(a)
  }

  implicit def cfgResultMonad(implicit app: Applicative[CFGResult]) =
    new Monad[CFGResult] {
      // Define flatMap using Option's flatten method
      override def flatMap[A, B](
          fa: CFGResult[A]
      )(f: A => CFGResult[B]): CFGResult[B] = fa match {
        case CFGOk(a) => f(a)
        case CFGError(s) => CFGError(s)
      }
      override def pure[A](a: A): CFGResult[A] = app.pure(a)

      @annotation.tailrec
      def tailRecM[A, B](
          init: A
      )(fn: A => CFGResult[Either[A, B]]): CFGResult[B] =
        fn(init) match {
          case CFGError(msg)   => CFGError(msg)
          case CFGOk(Right(b)) => CFGOk(b)
          case CFGOk(Left(a))  => tailRecM(a)(fn)
        }
    }

  
  /*
  def runCFG(methodDecl: MethodDecl): CFGResult[(Unit, StateInfo)] = {
    buildCFG.run(methodDecl).value
  }
  */

  type State[S, A] = StateT[CFGResult, S, A]
  type SIState[A] = State[StateInfo,A]

  trait CFGClass[A] {
    def buildCFG(a: A)(implicit m:Monad[SIState]): State[StateInfo, Unit]
  }

  object ops {
    def buildCFG[A](
        a: A
    )(implicit aCFGCl: CFGClass[A]): State[StateInfo, Unit] = {
      aCFGCl.buildCFG(a)
    }
  }

  def get:State[StateInfo,StateInfo] = StateT { stateInfo => 
    CFGOk((stateInfo, stateInfo))
  }

  def put(st:StateInfo):State[StateInfo,Unit] = StateT { _ =>
    CFGOk((st, ()))
  }

  def idFromVarDeclId(vdid:VarDeclId):Ident = vdid match {
    case VarId(id) => id
    case VarDeclArray(vid) => idFromVarDeclId(vid)
  }

  implicit def methodCFGInstance: CFGClass[MethodDecl] = new CFGClass[MethodDecl] {
    override def buildCFG(a: MethodDecl)(implicit m:Monad[SIState]): State[StateInfo, Unit] = a match {
      case MethodDecl(modifiers, type_params, return_ty, fname, formal_params, ex_types, exp, body) => for 
      { fargs <- m.pure(formal_params.map(fp => idFromVarDeclId(fp.var_decl_id)));
        _     <- ops.buildCFG(body);
        st    <- get;
        st1   <- m.pure(st); // TODO:we skip insertGoto and insertPhantom (double check)
        st2   <- m.pure(st.copy( cfg = formalArgsAsDecls(fargs,st1.cfg),
                                 formalArgs = fargs));
        _     <- put(st2)
      } yield ()
    } 
  }

  implicit def bodyCFGInstance: CFGClass[MethodBody] = new CFGClass[MethodBody] { 
    override def buildCFG(a: MethodBody)(implicit m:Monad[SIState]): State[StateInfo, Unit] = a match {
      case MethodBody( None ) => m.pure(())
      case MethodBody( Some(blk)) => ops.buildCFG(blk)
    }
  }

  implicit def blockCFGInstance: CFGClass[Block] = new CFGClass[Block]  {
    override def buildCFG(a: Block)(implicit m:Monad[SIState]): State[StateInfo, Unit] = a match {
      case Block(Nil) => {
        val lhs = Nil
        val rhs = Nil
        for {
          st  <- get;
          max <- m.pure(st.currId);
          currNodeId <- m.pure(internalIdent(s"${labPref}${max}"));
          max1 <- m.pure(max+1);
          cfg0 <- m.pure(st.cfg);
          preds0 <- m.pure(st.currPreds);
          cfgNode <- m.pure(Node(Nil, lhs, rhs, Nil, preds0, Nil, AssignmentNode));
          cfg1p   <- m.pure(preds0.foldLeft(cfg0)( (g,pred)=> {
            val n:Node = g(pred)
            g + (pred -> n.copy(succs = n.succs ++ List(currNodeId)))
          }));
          cfg1    <- m.pure(cfg1p + (currNodeId -> cfgNode));
          _       <- put(st.copy(cfg = cfg1, currId=max1, currPreds=List(currNodeId), continuable=false))
        } yield ()
      }
      case Block(stmts) => m.pure(())
    }
  }

  def internalIdent(s:String) :Ident = Ident(s)
  def formalArgsAsDecls(idents:List[Ident], cfg:CFG):CFG = cfg // TODO:fixme

}
