package obsidian.lang.java

import cats.*
import cats.implicits.*
import cats.data.StateT

import com.github.luzhuomi.scalangj.Syntax.*
import obsidian.lang.java.ASTUtils.*
import obsidian.lang.java.ASTPath.*

/*
 Control Flow Graph construction
 
 Do we still need this module?
 */

object CFG {

  type NodeId = ASTPath

  type CFG = Map[ASTPath, Node]

  /**
   * redesigning the CFG data type. Unlike the C CFG, which has only a single Node type
   * the Java CFG should have a proper algebraic data type node. Some node has no statement
   * e.g. If-else, while, try catch finally.
   */

  sealed trait Node

  /**
   * A CFG node contains a sequence of assignment statments
   *
   * @param id    node ID
   * @param stmts lits of locations of the enclosed statements
   * @param lVars variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
   * @param rVars variables on the rhs of assignments
   * @param preds predecessor node ids
   * @param succs successor node ids
   */
  case class AssignmentsNode(
                              id: ASTPath,
                              stmts: List[ASTPath],
                              localDecls: List[Ident],
                              lVars: List[Ident],
                              rVars: List[Ident],
                              preds: List[NodeId],
                              succs: List[NodeId]
                            ) extends Node

  /**
   * A CFG node containing an if-else statement
   *
   * @param id
   * @param thenNode then node id
   * @param elseNode else node id
   * @param lVars    variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
   * @param rVars    variables on the rhs of assignments
   * @param preds    predecessor ids
   * @param succs    successor ids
   */
  case class IfThenElseNode(
                             id: ASTPath,
                             thenNode: NodeId,
                             elseNode: NodeId,
                             lVars: List[Ident],
                             rVars: List[Ident],
                             preds: List[NodeId],
                             succs: List[NodeId]
                           ) extends Node


  /**
   * A CFG node containing a switch statement
   *
   * @param id
   * @param caseNodes the list of case nodes location
   * @param lVars
   * @param rVars
   * @param preds
   * @param succs
   */

  case class SwitchNode(
                         id: ASTPath,
                         caseNodes: List[NodeId],
                         lVars: List[Ident],
                         rVars: List[Ident],
                         preds: List[NodeId],
                         succs: List[NodeId]
                       ) extends Node


  /**
   * A CFG node containing a while loop
   *
   * @param id
   * @param bodyNode the location of the body
   * @param lVars    variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
   * @param rVars    variables on the rhs of assignments
   * @param preds    predecessor ids
   * @param succs    successor ids
   */
  case class WhileNode(
                        id: ASTPath,
                        bodyNode: NodeId,
                        lVars: List[Ident],
                        rVars: List[Ident],
                        preds: List[NodeId],
                        succs: List[NodeId]
                      ) extends Node

  /**
   * A CFG node containing a try catch finally statement
   *
   * @param id
   * @param tryNode        the location of the try node
   * @param catchNodes     the locations of the catch blocks
   * @param catchLocalVars the exception variables used in the catch block
   * @param finallyNode    the locations of the finally blocks
   * @param preds          predecessor ids
   * @param succs          successor ids
   */

  case class TryCatchFinallyNode(
                                  id: ASTPath,
                                  tryNode: NodeId,
                                  catchNodes: NodeId,
                                  catchLocalVars: List[Ident],
                                  finallyNode: NodeId,
                                  preds: List[NodeId],
                                  succs: List[NodeId]
                                ) extends Node


  /**
   * A CFG node containing a return statement
   *
   * @param id
   * @param lVars variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
   * @param rVars variables on the rhs of assignments
   * @param preds predecessor ids
   */

  case class ReturnNode(
                         id: ASTPath,
                         lVars: List[Ident],
                         rVars: List[Ident],
                         preds: List[NodeId]
                       ) extends Node

  /**
   * A CFG node containing a throw statement
   *
   * @param id
   * @param lVars variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
   * @param rVars variables on the rhs of assignments
   * @param preds predecessor ids
   * @param succs successor ids
   */

  case class ThrowNode(
                        id: ASTPath,
                        lVars: List[Ident],
                        rVars: List[Ident],
                        preds: List[NodeId],
                        succs: List[NodeId]
                      ) extends Node

  /**
   * A CFG node containing an assert statement
   *
   * @param id
   * @param lVars variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
   * @param rVars variables on the rhs of assignments
   * @param preds
   * @param succs
   */

  case class AssertNode(
                         id: ASTPath,
                         lVars: List[Ident],
                         rVars: List[Ident],
                         preds: List[NodeId],
                         succs: List[NodeId]
                       ) extends Node


  /**
   * a CFG mode containing a break statement
   *
   * @param id
   * @param preds
   * @param succs
   */

  case class BreakNode(
                        id: ASTPath,
                        preds: List[NodeId],
                        succs: List[NodeId]
                      ) extends Node

  /**
   * a CFG node containing a continue statement
   *
   * @param id
   * @param preds
   * @param succs
   */
  case class ContNode(
                       id: ASTPath,
                       preds: List[NodeId],
                       succs: List[NodeId]
                     ) extends Node

  /**
   * a CFG node containing a default statement
   *
   * @param id
   * @param stmts
   * @param preds
   * @param succs
   */
  case class DefaultNode(
                          id: ASTPath,
                          stmts: List[ASTPath],
                          preds: List[NodeId],
                          succs: List[NodeId]
                        ) extends Node


  /**
   * A CFG Node containing a case statement
   *
   * @param id
   * @param stmts
   * @param preds
   * @param succs
   */

  case class CaseNode(
                       id: ASTPath,
                       stmts: List[ASTPath],
                       preds: List[NodeId],
                       succs: List[NodeId]
                     ) extends Node


  // update functions for nodes

  def setSuccs(n: Node, s: List[NodeId]): Node = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, s)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, s)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => SwitchNode(id, caseNodes, lVars, rVars, preds, s)
    case ReturnNode(id, lVars, rVars, preds) => n
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rVars, preds, s)
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, s)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lVars, rVars, preds, s)
    case AssertNode(id, lvars, rvars, preds, succs) => AssertNode(id, lvars, rvars, preds, s)
    case BreakNode(id, preds, succs) => BreakNode(id, preds, s)
    case ContNode(id, preds, cuccs) => ContNode(id, preds, s)
    case DefaultNode(id, stmts, preds, succs) => DefaultNode(id, stmts, preds, s)
    case CaseNode(id, stmts, preds, succs) => CaseNode(id, stmts, preds, s)
  }

  def setPreds(n: Node, p: List[NodeId]): Node = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lVars, rVars, p, succs)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lVars, rVars, p, succs)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => SwitchNode(id, caseNodes, lVars, rVars, p, succs)
    case ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lVars, rVars, p)
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rVars, p, succs)
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, p, succs)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lVars, rVars, p, succs)
    case AssertNode(id, lvars, rvars, preds, succs) => AssertNode(id, lvars, rvars, p, succs)
    case BreakNode(id, preds, succs) => BreakNode(id, p, succs)
    case ContNode(id, preds, succs) => ContNode(id, p, succs)
    case DefaultNode(id, stmts, preds, succs) => DefaultNode(id, stmts, p, succs)
    case CaseNode(id, stmts, preds, succs) => CaseNode(id, stmts, p, succs)
  }

  def setLVars(n: Node, lv: List[Ident]): Node = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lv, rVars, preds, succs)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lv, rVars, preds, succs)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => SwitchNode(id, caseNodes, lv, rVars, preds, succs)
    case ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lv, rVars, preds)
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lv, rVars, preds, succs)
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lv, rVars, preds, succs)
    case AssertNode(id, lvars, rvars, preds, succs) => AssertNode(id, lv, rvars, preds, succs)
    case BreakNode(id, preds, succs) => n
    case ContNode(id, preds, cuccs) => n
    case DefaultNode(id, stmts, preds, succs) => n
    case CaseNode(id, stmts, preds, succs) => n

  }

  def setRVars(n: Node, rv: List[Ident]): Node = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lVars, rv, preds, succs)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lVars, rv, preds, succs)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => SwitchNode(id, caseNodes, lVars, rv, preds, succs)
    case ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lVars, rv, preds)
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rv, preds, succs)
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lVars, rv, preds, succs)
    case AssertNode(id, lvars, rvars, preds, succs) => AssertNode(id, lvars, rv, preds, succs)
    case BreakNode(id, preds, succs) => n
    case ContNode(id, preds, cuccs) => n
    case DefaultNode(id, stmts, preds, succs) => n
    case CaseNode(id, stmts, preds, succs) => n
  }

  def getSuccs(n: Node): List[NodeId] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => succs
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => succs
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => succs
    case ReturnNode(id, lVars, rVars, preds) => Nil
    case ThrowNode(id, lVars, rVars, preds, succs) => succs
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => succs
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => succs
    case AssertNode(id, lVars, rVars, preds, succs) => succs
    case BreakNode(id, preds, succs) => succs
    case ContNode(id, preds, succs) => succs
    case DefaultNode(id, stmts, preds, succs) => succs
    case CaseNode(id, stmts, preds, succs) => succs
  }

  def getPreds(n: Node): List[NodeId] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => preds
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => preds
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => preds
    case ReturnNode(id, lVars, rVars, preds) => preds
    case ThrowNode(id, lVars, rVars, preds, succs) => preds
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => preds
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => preds
    case AssertNode(id, lVars, rVars, preds, succs) => preds
    case BreakNode(id, preds, succs) => preds
    case ContNode(id, preds, cuccs) => preds
    case DefaultNode(id, stmts, preds, succs) => preds
    case CaseNode(id, stmts, preds, succs) => preds
  }

  def getLVars(n: Node): List[Ident] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => lVars
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => lVars
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => lVars
    case ReturnNode(id, lVars, rVars, preds) => lVars
    case ThrowNode(id, lVars, rVars, preds, succs) => lVars
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => Nil
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => lVars
    case AssertNode(id, lVars, rVars, preds, succs) => lVars
    case BreakNode(id, preds, succs) => Nil
    case ContNode(id, preds, cuccs) => Nil
    case DefaultNode(id, stmts, preds, succs) => Nil
    case CaseNode(id, stmts, preds, succs) => Nil
  }

  def getRVars(n: Node): List[Ident] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => rVars
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => rVars
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) => rVars
    case ReturnNode(id, lVars, rVars, preds) => rVars
    case ThrowNode(id, lVars, rVars, preds, succs) => rVars
    case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) => Nil
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => rVars
    case AssertNode(id, lVars, rVars, preds, succs) => rVars
    case BreakNode(id, preds, succs) => Nil
    case ContNode(id, preds, cuccs) => Nil
    case DefaultNode(id, stmts, preds, succs) => Nil
    case CaseNode(id, stmts, preds, succs) => Nil
  }

  def appSucc(n: Node, succ: NodeId) = setSuccs(n, (getSuccs(n) ++ List(succ)).toSet.toList)

  def appPred(n: Node, pred: NodeId) = setPreds(n, (getPreds(n) ++ List(pred)).toSet.toList)

  def appPreds(n: Node, preds: List[NodeId]) = setPreds(n, (getPreds(n) ++ preds).toSet.toList)

  def appLVars(n: Node, lvs: List[Ident]) = setLVars(n, getLVars(n) ++ lvs)

  def appRVars(n: Node, rvs: List[Ident]) = setRVars(n, getRVars(n) ++ rvs)

  def appStmt(n: Node, stmt: ASTPath) = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => {
      val stmtSet = stmts.toSet
      if (stmtSet.contains(stmt)) {
        n
      } else {
        AssignmentsNode(id, stmts ++ List(stmt), localDecls, lVars, rVars, preds, succs)
      }
    }
    case _ => n
  }

  def appLocalDecls(n: Node, lds: List[Ident]) = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => {
      AssignmentsNode(id, stmts, (localDecls ++ lds).toSet.toList, lVars, rVars, preds, succs)
    }
    case _ => n
  }

  /**
   * StateInfo, an state datatype for the CFG state monad
   *
   * @param currId
   * @param cfg
   * @param currPreds
   * @param continuable
   * @param contNodes
   * @param breakNodes
   * @param caseNodes
   * @param formalArgs
   * @param fallThroughCases
   * @param throwNodes
   * @param catchNodes
   * @param labelMap
   */
  case class StateInfo(
                        currId: NodeId,
                        cfg: CFG,
                        currPreds: List[NodeId],
                        continuable: Boolean, // is this still needed
                        contNodes: Map[NodeId, List[NodeId]], // label node id -> [continue statement id]
                        breakNodes: Map[NodeId, List[NodeId]], // label node id -> [break statement id]
                        caseNodes: List[CaseExp],
                        formalArgs: List[Ident],
                        fallThroughCases: List[Exp],
                        throwNodes: List[NodeId],
                        catchNodes: List[NodeId],

                        labelMap: Map[Ident, NodeId] // mapping source code label to CFG labels

                      )

  /**
   * addpend:
   * add a value associated wtih key to a map,
   * if the key exits, the value is appended to the existing value list.
   * otherwise, a new entry added
   *
   * @param key   the key
   * @param value the value to be insered
   * @param m     a map from key to list of values
   * @return an udpated map
   */
  def addpend[A, B](key: A, value: B, m: Map[A, List[B]]): Map[A, List[B]] = m.get(key) match {
    case None => m + (key -> List(value))
    case Some(vs) => m + (key -> (vs ++ List(value)))
  }

  sealed trait CaseExp {
    def getWrapperId(): NodeId
  }

  case class DefaultCase(wrapperId: NodeId, rhs: NodeId) extends CaseExp {
    override def getWrapperId() = wrapperId
  }

  case class ExpCase(
                      condExp: Exp,
                      fallThrough: List[Exp],
                      wrapperId: NodeId,
                      rhs: NodeId
                    ) extends CaseExp {
    override def getWrapperId() = wrapperId
  }

  // val labPref = "myLabel"
  val initStateInfo = StateInfo(
    rootPath,
    Map[NodeId, Node](),
    List(),
    false,
    Map(),
    Map(),
    List(),
    List(),
    List(),
    List(),
    List(),
    Map()
  )

  enum CFGResult[+A] {
    case CFGError(msg: String) extends CFGResult[Nothing]
    case CFGOk[A](result: A) extends CFGResult[A]
  }

  import CFGResult.*
  
  given cfgResultFunctor: Functor[CFGResult] =
    new Functor[CFGResult] {
      override def map[A, B](fa: CFGResult[A])(f: A => B): CFGResult[B] =
        fa match {
          case CFGError(s) => CFGError(s)
          case CFGOk(a) => CFGOk(f(a))
        }
    }


  given cfgResultApplicative: ApplicativeError[CFGResult, String] =
    new ApplicativeError[CFGResult, String] {
      override def ap[A, B](
                             ff: CFGResult[A => B]
                           )(fa: CFGResult[A]): CFGResult[B] =
        ff match {
          case CFGOk(f) =>
            fa match {
              case CFGOk(a) => CFGOk(f(a))
              case CFGError(s) => CFGError(s)
            }
          case CFGError(s) => CFGError(s)
        }

      override def pure[A](a: A): CFGResult[A] = CFGOk(a)

      override def raiseError[A](e: String): CFGResult[A] = CFGError(e)

      override def handleErrorWith[A](
                                       fa: CFGResult[A]
                                     )(f: String => CFGResult[A]): CFGResult[A] =
        fa match {
          case CFGError(s) => f(s)
          case CFGOk(a) => CFGOk(a)
        }
    }

  given cfgResultMonadError(using
                                   app: ApplicativeError[CFGResult, String]
                                  ): MonadError[CFGResult, String] =
    new MonadError[CFGResult, String] {
      override def raiseError[A](e: String): CFGResult[A] = app.raiseError(e)

      override def handleErrorWith[A](fa: CFGResult[A])(
        f: String => CFGResult[A]
      ): CFGResult[A] = app.handleErrorWith(fa)(f)

      override def flatMap[A, B](
                                  fa: CFGResult[A]
                                )(f: A => CFGResult[B]): CFGResult[B] =
        fa match {
          case CFGOk(a) => f(a)
          case CFGError(s) => CFGError(s)
        }

      override def pure[A](a: A): CFGResult[A] = app.pure(a)

      @annotation.tailrec
      def tailRecM[A, B](
                          init: A
                        )(fn: A => CFGResult[Either[A, B]]): CFGResult[B] =
        fn(init) match {
          case CFGError(msg) => CFGError(msg)
          case CFGOk(Right(b)) => CFGOk(b)
          case CFGOk(Left(a)) => tailRecM(a)(fn)
        }
    }

  /*
  def runCFG(methodDecl: MethodDecl): CFGResult[(Unit, StateInfo)] = {
    buildCFG.run(methodDecl).value
  }
   */

  type State[S, A] = StateT[CFGResult, S, A]
  type SIState[A] = State[StateInfo, A]

  /*
  build a partial CFG, it is "partial" in the sense that
  1) the original goto are not yet connected to the labeled block
      i.e. labeled block's preds list is yet to include the goto's block label
      hm.. not might be the case for java, coz continue L appear inside L:{...}
  2) the succs of the continue and break blocks yet need to updated
        but the continue and break's block label should be associated with the parent (switch or loop) block label
  3) the non-native continue (serving as goto) need to be generated non-goto block (end of the block, a "goto succLabel" is required to be inserted before SSA to be built)
      no need for Java?
   */
  trait CFGClass[A] {
    def buildCFG(a: A, p: ASTPath)(using
                                   m: MonadError[SIState, String]
    ): State[StateInfo, Unit]
  }

  object cfgOps {
    def buildCFG[A](
                     a: A, p: ASTPath
                   )(using aCFGCl: CFGClass[A]): State[StateInfo, Unit] = {
      aCFGCl.buildCFG(a, p)
    }
  }

  def get: State[StateInfo, StateInfo] =
    StateT { stateInfo =>
      CFGOk((stateInfo, stateInfo))
    }

  def put(st: StateInfo): State[StateInfo, Unit] =
    StateT { _ =>
      CFGOk((st, ()))
    }



  given methodCFGInstance: CFGClass[MethodDecl] =
    new CFGClass[MethodDecl] {
      override def buildCFG(
                             a: MethodDecl, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
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
            for {
              fargs <- m.pure(
                formal_params.map(fp => idFromVarDeclId(fp.var_decl_id))
              );
              _ <- cfgOps.buildCFG(body, p);
              st <- get;
              st1 <- m.pure(
                st
              ); // TODO:we skip insertGoto and insertPhantom (double check)
              st2 <- m.pure(
                st.copy(
                  cfg = formalArgsAsDecls(fargs, st1.cfg),
                  formalArgs = fargs
                )
              );
              _ <- put(st2)
            } yield ()
        }
    }

  given bodyCFGInstance: CFGClass[MethodBody] =
    new CFGClass[MethodBody] {
      override def buildCFG(
                             a: MethodBody, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case MethodBody(None) => m.pure(())
          case MethodBody(Some(blk)) => cfgOps.buildCFG(blk, p)
        }
    }

  given blockCFGInstance: CFGClass[Block] =
    new CFGClass[Block] {
      override def buildCFG(
                             a: Block, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case Block(Nil) => { // TODO: this case is not needed? since empty block is deguared into a block containing an empty statement?
            val lhs = Nil
            val rhs = Nil
            val stmts = Nil
            val localDecls = Nil
            val currNodeId = p
            for {
              st <- get;
              cfg0 <- m.pure(st.cfg);
              preds0 <- m.pure(st.currPreds);
              cfgNode <- m.pure(AssignmentsNode(currNodeId, stmts, localDecls, lhs, rhs, preds0, Nil));
              cfg1p <- m.pure(preds0.foldLeft(cfg0)((g, pred) => {
                val n: Node = g(pred)
                g + (pred -> appSucc(n, currNodeId))
              }));
              cfg1 <- m.pure(cfg1p + (currNodeId -> cfgNode));
              _ <- put(
                st.copy(
                  cfg = cfg1,
                  currPreds = List(currNodeId),
                  continuable = false
                )
              )
            } yield ()
          }
          case Block(stmts) =>
            for {
              _ <- stmts.zip(0 to stmts.size - 1).traverse_(stmt_idx => stmt_idx match {
                case (stmt, idx) => {
                  cfgOps.buildCFG(stmt, childOf(p, idx))
                }
              })
              // TOOD: check whether the following hack is still necessary for Java
              _ <-
                if (stmts.isEmpty) {
                  m.pure(())
                }
                else {
                  stmts.last match {
                    /* the last blockItem is a while. e.g.
                    int f() {
                      int c = 0;
                      while (1) {
                        if (c > 10)
                          return c;
                        c++;
                      }
                      1 == 1; // inserted automatically
                    }
                    */
                    case BlockStmt_(stmt)
                      if isWhileStmt(stmt) || isForStmt(stmt) => {
                      val empty: Stmt = Empty
                      cfgOps.buildCFG(empty, childOf(p, stmts.size))
                    }
                    case _ => m.pure(())
                  }
                }
            } yield ()
        }
    }

  given blockStmtCFGInstance: CFGClass[BlockStmt] =
    new CFGClass[BlockStmt] {
      override def buildCFG(
                             a: BlockStmt, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case LocalClass(_) => m.raiseError("Local Class is not supported.")
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls match {
              case Nil => m.pure(())
              case (var_decl :: Nil) =>
                /*
        CFG1 = CFG update { pred : {stmts = stmts ++ [path], lVars = lVars ++ [x] } }
        --------------------------------------------------------
        CFG, path, preds, true |- ty x = exp[] => CFG1,  [] , false


        CFG1 = CFG update { pred : {succ = path} |  pred <- preds } union { path : {ty x = exp[] } }
        --------------------------------------------------------
        CFG, path, preds, false |- ty x = exp[] => CFG1, [], false
                 */
                for {
                  st <- get;
                  _ <-
                    if (st.continuable) {
                      val cfg0 = st.cfg
                      val preds0 = st.currPreds
                      val s = a
                      val lvars = HasVarcfgOps.getLVarsFrom(var_decl)
                      val rvars = HasVarcfgOps.getVarsFrom(var_decl)
                      val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                        val n: Node = g(pred)
                        val n1 = appLocalDecls(appStmt(appRVars(appLVars(n, lvars), rvars), p), lvars)
                        g + (pred -> n1)
                      })
                      for {
                        _ <- put(st.copy(cfg = cfg1))
                      } yield ()
                    } else {
                      val currNodeId = p
                      val cfg0 = st.cfg
                      val preds0 = st.currPreds
                      val s = a
                      val lvars = HasVarcfgOps.getLVarsFrom(var_decl)
                      val rvars = HasVarcfgOps.getVarsFrom(var_decl)
                      val cfgNode = AssignmentsNode(
                        currNodeId,
                        List(p),
                        lvars,
                        lvars,
                        rvars,
                        preds0,
                        Nil
                      )
                      val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                        val n: Node = g(pred)
                        val n1 = appSucc(n, currNodeId)
                        g + (pred -> n1)
                      })
                      val cfg1 = cfg1p + (currNodeId -> cfgNode)
                      for {
                        _ <- put(
                          st.copy(
                            cfg = cfg1,
                            currPreds = List(currNodeId),
                            continuable = true
                          )
                        )
                      } yield ()
                    }
                } yield ()
              case (var_decl :: rest) =>
                for { // TODO Check whether multiple declaration in one statement is working fine.
                  // if it does not work, we can add sub path to p, such as 0, 1, 2,
                  _ <- buildCFG(LocalVars(modifiers, ty, var_decl :: Nil), p)
                  _ <- buildCFG(LocalVars(modifiers, ty, rest), p)
                      } yield ()
            }
          case BlockStmt_(stmt) => cfgOps.buildCFG(stmt, p)
        }
    }

  given stmtCFGInstance: CFGClass[Stmt] =
    new CFGClass[Stmt] {
      override def buildCFG(
                             a: Stmt, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case StmtBlock(blk) => cfgOps.buildCFG(blk, p)
          case IfThen(exp, stmt) => buildCFG(IfThenElse(exp, stmt, Empty), p)
          case IfThenElse(exp, true_stmt, false_stmt) =>
            /*
      
      CFG1 = CFG update { pred : {succ = path} |  pred <- preds } union { path : { id= path, succ = [], preds = preds} }
      CFG1, thenOf(path), {path}, false |-n trueStmt => CFG2, max2, preds1, _
      CFG2, elseOf(max), {path}, false |-n falseStmt => CFG3, max3, preds2, _
      -------------------------------------------------------------------------------------------------------------
      CFG, path, preds, _ |- if exp { trueStmt } else { falseStmt }  => CFG3, preds1 U preds2, false

             */
            for {
              st <- get;
              _ <- {
                val ifElseNodeId = p
                val lhs = HasVarcfgOps.getLVarsFrom(exp)
                val rhs = HasVarcfgOps.getVarsFrom(exp)
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val ifElseNode = IfThenElseNode(ifElseNodeId, thenOf(p), elseOf(p), lhs, rhs, preds0, Nil)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  val n1 = appSucc(n, ifElseNodeId)
                  g + (pred -> n1)
                })
                val cfg1 = cfg1p + (ifElseNodeId -> ifElseNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = List(ifElseNodeId),
                      continuable = false
                    )
                  );
                  _ <- buildCFG(true_stmt, thenOf(ifElseNodeId))
                  st1 <- get // after thenStmt being processed
                  _ <- put(
                    st1.copy(
                      currPreds = List(ifElseNodeId),
                      continuable = false
                    )
                  )
                  _ <- buildCFG(false_stmt, elseOf(ifElseNodeId))
                  st2 <- get // after elseStmt being processed
                  _ <- put(
                    st2.copy(
                      currPreds = st1.currPreds ++ st2.currPreds,
                      continuable = false
                    )
                  )
                } yield ()
              }
            } yield ()
          case Switch(exp, blocks) =>

            /*
            CFG0  = CFG union { p : switchNode(p,path++[0], ..., path++[n], preds, [path++[0], ..., path++[n]] ) } update { pred : {succ: succ u {path} | pred \in preds}}
            CFG0, path++[0], {path}, false, breakNodes, contNodes, caseNodes |- case1 => CFG1, preds1, continuable1, breakNodes1, contNodes1, caseNodes1
            CFG1, path++[1], {path} u preds1, false, breakNodes1, contNodes1, caseNodes1 |- case2 => CFG2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2
            CFG2, path++[2], {path} u preds2, false, breakNodes2, contNodes2, caseNodes2 |- case3 => CFG3, preds3, continuable3, breakNodes3, contNodes3, caseNodes3
            ...
            CFGn-1, path++[n-1], {path} u predsn-1, false, breakNodesn-1, contNodem-1, caseNodesn-1 |- casen => CFGn, predsn, continuable3, breakNodesn, contNodesn, caseNodesn
            ---------------------------------------------------------------------------------------------------------------------------------------------------------
            CFG, path, preds, continuable, breakNodes, contNodes, caseNodes |- swtch exp { case1, ..., casen } => CFGn, predsn u breakNodesn(p), false,  breakNodesn, contNodesn, caseNodesn 
            */
            for {
              st <- get
              _ <- {
                val currNodeId = p
                val lhs = HasVarcfgOps.getLVarsFrom(exp)
                val rhs = HasVarcfgOps.getVarsFrom(exp)
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val caseNodes0 = st.caseNodes
                val childNodeIds = (0 to blocks.size - 1).map(idx => childOf(p, idx)).toList
                val switchNode = SwitchNode(currNodeId, childNodeIds, lhs, rhs, preds0, childNodeIds)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n, currNodeId))
                })
                val cfg1 = cfg1p + (currNodeId -> switchNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = List(currNodeId),
                      fallThroughCases = Nil,
                      continuable = false,
                      caseNodes = Nil
                    )
                  )
                  _ <- blocks.zip(childNodeIds).traverse_(block_path => block_path match {
                    case (block, child_path) => for {
                      st <- get
                      _ <- put(
                        st.copy(currPreds = (List(currNodeId) ++ st.currPreds).toSet.toList)
                      )
                      _ <- cfgOps.buildCFG(block, child_path)
                    } yield ()
                  })
                  st1 <- get
                  _ <- {
                    val preds1 = st1.currPreds
                    val breakNodes1 = st1.breakNodes
                    val caseNodes1 = st1.caseNodes
                    for {
                      _ <- put(st1.copy(
                        currPreds = preds1 ++ breakNodes1(currNodeId).toList,
                        caseNodes = caseNodes0,
                        continuable = false
                      )
                      )
                    } yield ()
                  }
                } yield ()
              }
            } yield ()
          case While(exp, stmt) =>
            /*

            Note: the childNode of while node is a container node, i.e. { }, not a leaf node. Hence the childNode of while is not a successor of while, 
            the first child of the childNode is the successor of the while. similar observation applies to if-else and try. 

          CFG1 = CFG update { pred: {succ = p} | pred <- preds } union { path: whilenode(childOF(path), lhs, rhs, preds, childOf(path))}
          CFG1, childOF(path), {path}, _, {}, {} |- stmt |- CFG2,  preds2, continuable2, contNodes2, breakNodes2
          CFG3 = CFG3 update { nodeid: { succ = {path} | nodeid <- contNodes2(path)++preds2 } } update { path : { preds = preds ++ contNodes2(path) ++ preds2 } }
          -------------------------------------------------------------------------------------------------------------------------------
          CFG, path, preds, _, contNodes, breakNodes |- while (exp) { stmt } =>  CFG3, {path} ++ breakNodes2(path), false, breakNodes2 ,contNodes2
             */
            for {
              st <- get
              _ <- {

                val currNodeId = p
                val lhs = HasVarcfgOps.getLVarsFrom(exp)
                val rhs = HasVarcfgOps.getVarsFrom(exp)

                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val contNodes0 = st.contNodes
                val breakNodes0 = st.breakNodes
                /* 
                val childNodeId = stmt match {
                  case StmtBlock(_) => childOf(childOf(currNodeId, 0), 0)
                  case _ => childOf(currNodeId, 0)
                }
                */
                val childNodeId = childOf(currNodeId, 0)
                val cfgNode =
                  WhileNode(currNodeId, childNodeId, lhs, rhs, preds0, List(childOf(childNodeId,0)))
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n, currNodeId))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = List(currNodeId),
                      continuable = false,
                    )
                  )
                  _ <- buildCFG(stmt, childOf(p, 0))
                  st1 <- get
                  _ <- {
                    val preds2 = st1.currPreds
                    val contNodes2 = st1.contNodes
                    val cfg2 = st1.cfg
                    val conts2: List[NodeId] = contNodes2.get(currNodeId) match {
                      case None => Nil
                      case Some(ids) => ids
                    }
                    val cfg2p = (preds2 ++ conts2).foldLeft(cfg2)((g, pred) => {
                      val n = g(pred)
                      g + (pred -> appSucc(n, currNodeId))
                    })
                    val breakNodes2 = st1.breakNodes
                    val currNode = cfg2p(currNodeId)
                    val cfg2pp = cfg2p + (currNodeId -> appPreds(cfg2p(currNodeId), preds2 ++ conts2))
                    val cfg3 = conts2.foldLeft(cfg2pp)((g, l) => { // update the break and cont immediately
                      val n = g(l)
                      g + (l -> appSucc(n, currNodeId))
                    }
                    )
                    val cfg3p = cfg3
                    val breaks2 = breakNodes2.get(currNodeId) match {
                      case None => Nil
                      case Some(ids) => ids
                    }
                    for {
                      _ <- put(
                        st1.copy(
                          cfg = cfg3p,
                          currPreds = List(currNodeId) ++ breaks2,
                          continuable = false
                        )
                      )
                    } yield ()
                  }
                } yield ()
              }
            } yield ()
          case BasicFor(init, exp2, exp3, stmt) => m.raiseError("Basic For is encountered during CFG construction, which should have been desugared!")
          case EnhancedFor(modifiers, ty, id, exp, stmt) => m.raiseError("Enhanced For is encountered during CFG construction, which should have been desugared!")

          /*
          CFG1 = CFG update { pred : stmts=[empty], preds = preds, pred \in preds }
          ----------------------------------------------------
          CFG, path, preds, false |- CFG1, preds, true

          CFG1 = CFG update { pred: stmts = stmts ++ [empty], pred \in preds }
          ---------------------------------------------
          CFG, path, preds, true |- CFG1, preds, true
          */
          case Empty => for {
            st <- get
            _ <- if (st.continuable) {
              val cfg = st.cfg
              val preds = st.currPreds
              val cfg1 = preds.foldLeft(cfg)((g, pred) => {
                val n = g(pred)
                g + (pred -> appStmt(n, p))
              })
              for {
                _ <- put(st.copy(cfg = cfg1))
              } yield ()
            } else {
              val cfg = st.cfg
              val preds = st.currPreds
              val node = AssignmentsNode(p, List(p), Nil, Nil, Nil, preds, Nil)
              val cfg1 = preds.foldLeft(cfg + (p -> node))((g,pred) => {
                val n = g(pred)
                g + (pred -> appSucc(n, p))
              })
              for {
                _ <- put(st.copy(cfg = cfg1, currPreds = List(p), continuable = true))
              } yield ()
            }
          } yield ()
          /*
      CFG1 = CFG update { pred : {succ = path} |  pred <- preds } union { path : { stmt={x = exp}, lVars = lVars ++[x], preds = preds, succ = {} }  }
      --------------------------------------------------------
      CFG, path, preds, false |- x = exp => CFG1, {path}, true


      CFG1 = CFG update { pred: {stmts = stmts ++ [path], lVars = lVars ++[x] } }
      x \not in {v | v \in lVars preds, preds \in preds}
      ----------------------------------------------------------------
      CFG, path, preds, true |- x = exp => CFG1, preds, true
           */
          case s@ExpStmt(Assign(lhs, EqualA, rhs)) => {
            val xs = HasVarcfgOps.getLVarsFrom(lhs).toSet
            val ys = HasVarcfgOps.getVarsFrom(rhs)
            for {
              st <- get
              _ <- {
                val preds0 = st.currPreds
                val cfg0 = st.cfg
                val is_reassigned = preds0.foldLeft(false)((b, pred) => {
                  val n = cfg0(pred)
                  (b || (getLVars(n).exists(v => xs(v))))
                })
                if ((st.continuable) && (!is_reassigned)) {
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> appRVars(appLVars(appStmt(n, p), xs.toList), ys))
                  })
                  for {
                    _ <- put(st.copy(cfg = cfg1, continuable = true))
                  } yield ()
                } else {
                  val currNodeId = p
                  val cfgNode = AssignmentsNode(
                    currNodeId,
                    List(currNodeId),
                    Nil,
                    xs.toList,
                    ys,
                    preds0,
                    Nil)
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> appSucc(n, currNodeId))
                  })
                  val cfg1 = cfg1p + (currNodeId -> cfgNode)
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        currPreds = List(currNodeId),
                        continuable = true
                      )
                    )
                  } yield ()
                }
              }
            } yield ()

          }
          /*
      CFG, max, preds, true |- lhs = lrhs + rhs  => CFG', max', preds', continuable
      ---------------------------------------------------------------------------------------
      CFG, max, preds, true |- lhs += rhs  => CFG', max', preds', continuable
           */
          case ExpStmt(Assign(lhs, aop, rhs)) => m.raiseError("Accumulative assignment is encountered during CFG construction, which should have been desugared!")
          case ExpStmt(PostIncrement(exp)) => m.raiseError("PostIncrement is encountered during CFG construction, which should have been desugared!")
          case ExpStmt(PostDecrement(exp)) => m.raiseError("PostDecrement is encountered during CFG construction, which should have been desugared!")
          case ExpStmt(PreIncrement(exp)) => m.raiseError("PreIncrement is encountered during CFG construction, which should have been desugared!")
          case ExpStmt(PreDecrement(exp)) => m.raiseError("PreDecrement is encountered during CFG construction, which should have been desugared!")
          case ExpStmt(e) =>
            for {
              st <- get
              _ <- {
                val cfg0 = st.cfg
                val lhs = HasVarcfgOps.getLVarsFrom(e)
                val rhs = HasVarcfgOps.getVarsFrom(e)
                val preds0 = st.currPreds
                val s = BlockStmt_(ExpStmt(e))
                if (st.continuable) {
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> appRVars(appLVars(appStmt(n, p), lhs), rhs))
                  })
                  put(st.copy(cfg = cfg1, continuable = true))
                } else {
                  val currNodeId = p
                  val cfgNode = AssignmentsNode(currNodeId, List(p), Nil, lhs, rhs, preds0, Nil)
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> appSucc(n, currNodeId))
                  })
                  val cfg1 = cfg1p + (currNodeId -> cfgNode)
                  put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = List(currNodeId),
                      continuable = true
                    )
                  )
                }
              }
            } yield ()

          /*
      Assertion might throw an unchecked exception, which only be enabled when java is run with -ea flag.
      It is a good practice not to catch AssertionException

      exp must be of type boolean
      any local side effect (to the call stack) in exp will be discarded

      max1 = max + 1
      CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : {assert exp : msg} }
      --------------------------------------------------------
      CFG, max, preds, false |- assert exp : msg => CFG1, max1, [], false
           */
          case s@Assert(exp, msg) =>
            for {
              st <- get
              _ <- {
                val es = List(exp) ++ msg.toList
                val lhs = es.flatMap(HasVarcfgOps.getLVarsFrom(_))
                val rhs = es.flatMap(HasVarcfgOps.getVarsFrom(_))
                val currNodeId = p
                val preds0 = st.currPreds
                val cfg0 = st.cfg
                val cfgNode = AssertNode(
                  p,
                  lhs,
                  rhs,
                  preds0,
                  Nil
                )
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n, currNodeId))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = List(currNodeId),
                      continuable = false
                    )
                  )
                } yield ()
              }
            } yield ()

          case Break(None) => m.raiseError("Break without label is encountered during CFG construction, which should have been eliminated during the labeling step.")
          case Break(Some(id)) =>
            for {
              st <- get
              _ <- {
                val labelMap = st.labelMap
                labelMap.get(id) match {
                  case None => m.raiseError(s"A label (${id}) is associated with now AST Path location during CFG construction. ")
                  case Some(targetNodeId) => {
                    val currNodeId = p
                    val cfg0 = st.cfg
                    val preds0 = st.currPreds
                    val cfgNode = BreakNode(p, preds0, Nil)
                    val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                      val n = g(pred)
                      g + (pred -> appSucc(n, p))
                    })
                    val cfg1 = cfg1p + (currNodeId -> cfgNode)
                    for {
                      _ <- put(
                        st.copy(
                          cfg = cfg1,
                          currPreds = Nil,
                          continuable = false,
                          breakNodes = addpend(targetNodeId, currNodeId, st.breakNodes)
                        )
                      )
                    } yield ()
                  }
                }
              }
            } yield ()

          case Continue(Some(id)) =>
            for {
              st <- get
              _ <- st.labelMap.get(id) match {
                case None =>
                  m.raiseError(s"A label (${id}) is associated with now AST Path location during CFG construction. ")
                case Some(targetNodeId) => {
                  /*
            l' = labelMap(l)
            max1 = max + 1
            CFG1 = CFG update { pred: {succs = succs ++ [path] } | pred <- preds } union { path : { preds= preds, succs = [l'] }}
            contNodes1 = contNodes + (l' -> contNodes(l') ++ [path])
            -----------------------------------------------------------------------------------------------------------------------------
            CFG, path, preds, false, contNode, breaknodes, labelMap |- continue l => CFG1, {}, false, contNodes1, breakNodes, labelMap
                   */
                  val cfg0 = st.cfg
                  val preds0 = st.currPreds
                  val currNodeId = p
                  val cfgNode = ContNode(currNodeId, preds0, List(targetNodeId)) // seems redundant, the succs is also updated in the while loop case.
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> appSucc(n, currNodeId))
                  })
                  val cfg1 = cfg1p + (currNodeId -> cfgNode)
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        continuable = false,
                        contNodes = addpend(targetNodeId, currNodeId, st.contNodes)
                      )
                    )
                  } yield ()
                }
              }
            } yield ()

          case Continue(None) => m.raiseError("Continue without label is encountered during CFG construction, which should have been eliminated during the labeling step.")
          /*
          CFG1 = CFG update { pred : {succ = path} |  pred <- preds } union { path : {preds =preds } }
          --------------------------------------------------------
          CFG, path, preds, false |- return exp => CFG1, [], false
           */
          case Return(o_exp) =>
            for {
              st <- get
              _ <- {
                val currNodeId = p
                val lhs = o_exp.toList.flatMap(HasVarcfgOps.getLVarsFrom(_))
                val rhs = o_exp.toList.flatMap(HasVarcfgOps.getVarsFrom(_))
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val cfgNode = ReturnNode(currNodeId, lhs, rhs, preds0)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n, currNodeId))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = Nil,
                      continuable = false
                    )
                  )
                } yield ()
              }
            } yield ()
          case Synchronized(exp, blk) =>
            m.raiseError("Synronized statement is not suppored by CFG construction.")
          case Throw(exp) =>
            for {
              /*
      
      CFG1 = CFG update { pred: {succ = max}} pred <- preds } union { max : {preds = preds} }
      -----------------------------------------------------------------------------------------------
      CFG, path, preds, continuable, breakNodes, contNodes, caseNodes, throwNodes |- throw exp => CFG1, max1, [], false

               */
              st <- get
              _ <- {
                val cfg0 = st.cfg
                val currNodeId = p
                val preds0 = st.currPreds
                val lhs = HasVarcfgOps.getLVarsFrom(exp)
                val rhs = HasVarcfgOps.getVarsFrom(exp)
                val cfgNode = ThrowNode(currNodeId, lhs, rhs, preds0, Nil)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n, currNodeId))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      continuable = false,
                      currPreds = Nil,
                      throwNodes = st.throwNodes ++ List(currNodeId)
                    )
                  )
                } yield ()
              }
            } yield ()

          case Try(try_blk, List(catch_blk@Catch(params, blk)), Some(finally_blk)) => // there should be only one catch block, multiple catch blocks have been desguared into one.
            for {
              /*
      n = { try_node = tryOf(path), catch_nodes = [ childOf(path, n+1) | n <- (0.. num_catches) ], finally_node = childOf(path, num_catches+1), preds = preds, succs = Nil }
      CFG0 = CFG + { path -> n }
      CFG0, tryOf(path), {path}, false, breakNodes, contNodes, caseNodes, {}, _ |-
        blk1 => CFG1, preds1, continuable1, breakNodes1, contNodes1, caseNodes1, throwNodes1, catchNodes,
      CFG1, childOf(path,1), throwNodes1, false, breakNodes1, contNodes1, caseNodes1, throwNodes, {},  |-
        blk2 => CFG2, max2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2, throwNodes2, catchNodes2
      CFGN, childOf(path, 2), preds1++preds2, false, breakNodes2, contNodes2, caseNodes2, throwNodes2 |-
        blk3 => CFG3, preds3, continuable3, breakNodes3, contNode3, caseNodes3, throwNodes3
      ----------------------------------------------------------------------------------------------------------------
      CFG, path, preds, continuable, breakNodes, contNodes, caseNodes, throwNodes, catchNodes |- try {blk1} catch (x) {blk2} finally {blk3}
       => CFG3, max3, preds3, false, breakNodes3, contNode3, caseNodes3, throwNodes ++ throwNodes3, catchNodes
       // local catch Nodes should not escape
               */
              st <- get
              _ <- {
                val currNodeId = p
                val preds0 = st.currPreds
                val catchVars = HasVarcfgOps.getLVarsFrom(params)
                val n = TryCatchFinallyNode(
                  currNodeId,
                  tryOf(currNodeId),
                  childOf(currNodeId, 1),
                  catchVars,
                  childOf(currNodeId, 2),
                  preds0,
                  List(childOf(tryOf(currNodeId), 0))
                )
                val cfg0 = preds0.foldLeft(st.cfg + (currNodeId -> n))((g, pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n, currNodeId))
                })
                val throwNodes0 = st.throwNodes
                val catchNodes0 = st.catchNodes
                for { // building for try block
                  _ <- put(
                    st.copy(
                      cfg = cfg0,
                      throwNodes = Nil,
                      currPreds = List(currNodeId),
                      continuable = false
                    )
                  )
                  _ <- cfgOps.buildCFG(try_blk, tryOf(currNodeId))
                  st1 <- get
                  _ <- { // building for catch blocks
                    val preds1 = st1.currPreds
                    val throwNodes1 = st1.throwNodes
                    for {
                      _ <- put(
                        st1.copy(throwNodes = throwNodes0, catchNodes = Nil, currPreds = throwNodes1)
                      )
                      _ <- cfgOps.buildCFG(catch_blk, childOf(currNodeId, 1))
                      st2 <- get
                      _ <- { // building for finally blocks if any
                        val catchNodes2 = st2.catchNodes
                        val preds2 = st2.currPreds
                        for {
                          _ <- put(
                            st2.copy(
                              currPreds = preds1 ++ preds2, // preds1 from try, preds2 from catch
                              continuable = false
                            )
                          )
                          _ <- cfgOps.buildCFG(finally_blk, childOf(currNodeId, 2))
                          st3 <- get
                          _ <- put(
                            st3.copy(
                              continuable = false,
                              throwNodes = throwNodes0 ++ st3.throwNodes,
                              catchNodes = catchNodes0
                            )
                          )
                        } yield ()
                      }
                    } yield ()
                  }
                } yield ()
              }
            } yield ()
          case Try(try_blk, List(catch_blk@Catch(params, blk)), None) => buildCFG(Try(try_blk, List(catch_blk), Some(Block(Nil))), p) 
          case Try(try_blk, _, _) => m.raiseError("An error is encountered during the CFG construction. try-catch-finally block should have been desugared to have one catch block and one finally block.")
          case Labeled(id, stmt) => // label shoudl have same path as its containing statement, for the ease of building the labl map
            for {
              /*
            ASSUMPTION, labeled statements in Java must be introduced first then used by continue or break.

            lblMap1 = lblMap + (l -> path)
            CFG1, path, preds, false, lblMap1 |- stmt => CFG2, preds2, continuable2, lblMap2
            ------------------------------------------------------------------------------
            CFG, path, preds, _, lblMap |- l: stmt => CFG2, preds2, continuable2, lblMap2
               */
              st <- get
              _ <- put(
                st.copy(
                  labelMap = st.labelMap + (id -> p),
                  continuable = false
                )
              )
              _ <- buildCFG(stmt, p)
            } yield ()
        }
    }

  given catchCFGInstance: CFGClass[Catch] =
    new CFGClass[Catch] {
      override def buildCFG(
                             a: Catch, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case Catch(params, blk) =>
            for {
              _ <- cfgOps.buildCFG(blk, p)
              st <- get
              _ <- put(
                st.copy(
                  catchNodes = st.catchNodes ++ List(childOf(p, 0))
                )
              )
            } yield ()
        }
    }

  given switchBlockCFGInstance: CFGClass[SwitchBlock] =
    new CFGClass[SwitchBlock] {
      override def buildCFG(
                             a: SwitchBlock, p: ASTPath
                           )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case SwitchBlock(Default, blk_stmts) => {
            /*
            it seems that we can get rid of the caseNodes
            CFG0 = CFG update (path -> DefaultNode(path, [childOf(path,0), ..., childOf(path, n-1), preds, [childOf(path,0)]]))
                      union [ pred -> { succs = succs + [path]}]
            CFG0, childOf(path,0), preds, continuable, breakNodes, contNodes, caseNodes |-
              stmt1 => CFG1, preds1, continuable1, breakNodes1, contNodes1, caseNodes1

            CFG1, childOf(path,1), preds1, continable1, breakNodes1, contNodes1, caseNodes1 |-
              stmt2 => CFG2, preds2, continuable2, breakNodes2, contNodes2, caseNode2
            ...
            ------------------------------------------------------------------------------------------------------------------------------------------------------------------
            CFG, path,preds, continuable, breakNodes, contNodes, caseNodes |-
              default: stmt1; ... ; stmtn; => CFG2, max2, preds2 continuable2, breakNodes, contNodes2, caseNodes2 union (max, default)
            */
            val children_path = (0 to blk_stmts.size-1).map(idx => childOf(p, idx)).toList
            for {
              st <- get
              _ <- {
                val cfgNode = DefaultNode(p, children_path, st.currPreds, Nil) // will update later
                for {
                  _ <- put(st.copy(
                    cfg = st.cfg + (p -> cfgNode),
                    currPreds = List(p),
                    continuable = false
                  ))
                  _ <- blk_stmts.zip(children_path).traverse_(bp => bp match {
                    case (blk, path) => cfgOps.buildCFG(blk, path)
                  })
                } yield ()
              }
            } yield ()
          }
          case SwitchBlock(SwitchCase(e), blk_stmts) => {
            /*
            it seems that we can get rid of the caseNodes
            CFG0 = CFG update (path -> CaseNode(path, [childOf(path,0), ..., childOf(path, n-1), preds, [childOf(path,0)]]))
                      union [ pred -> { succs = succs + [path]}]
            CFG0, childOf(path,0), preds, continuable, breakNodes, contNodes, caseNodes |-
              stmt1 => CFG1, preds1, continuable1, breakNodes1, contNodes1, caseNodes1

            CFG1, childOf(path,1), preds1, continable1, breakNodes1, contNodes1, caseNodes1 |-
              stmt2 => CFG2, preds2, continuable2, breakNodes2, contNodes2, caseNode2
            ...
            ------------------------------------------------------------------------------------------------------------------------------------------------------------------
            CFG, path,preds, continuable, breakNodes, contNodes, caseNodes |-
              case lit: stmt1; ... ; stmtn; => CFG2, max2, preds2 continuable2, breakNodes, contNodes2, caseNodes2 union (max, default)
            */
            val children_path = (0 to blk_stmts.size - 1).map(idx => childOf(p, idx)).toList
            for {
              st <- get
              _ <- {
                val cfgNode = CaseNode(p, children_path, st.currPreds, Nil) // will update later
                for {
                  _ <- put(st.copy(
                    cfg = st.cfg + (p -> cfgNode),
                    currPreds = List(p),
                    continuable = false
                  ))
                  _ <- blk_stmts.zip(children_path).traverse_(bp => bp match {
                    case (blk, path) => cfgOps.buildCFG(blk, path)
                  })
                } yield ()
              }
            } yield ()
          }
        }
    }

  /* not needed, for loop is desugared into while

  given varDeclCFGInstance: CFGClass[ForLocalVars] =
    new CFGClass[ForLocalVars] {
      override def buildCFG(
          a: ForLocalVars, p: ASTPath
      )(using m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {

          case ForLocalVars(modifiers, ty, var_decls) =>
            for {

              /*

          max1 = max + 1
          CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : {ty x = exp[] } }
          --------------------------------------------------------
          CFG, path, preds, false |- ty x = exp[] => CFG1, [], false
               */

              st <- get
              _ <- {
                val currNodeId = p
                val s = LocalVars(modifiers, ty, var_decls)
                val lhs = var_decls.flatMap(HasVarcfgOps.getLVarsFrom(_))
                val rhs = var_decls.flatMap(HasVarcfgOps.getVarsFrom(_))
                val preds0 = st.currPreds
                val cfgNode = Node(List(s), Nil, lhs, rhs, preds0, Nil, Other)
                val cfg0 = st.cfg
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> n.copy(succs =
                    (n.succs ++ List(currNodeId)).toSet.toList
                  ))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currId = max + 1,
                      currPreds = List(currNodeId),
                      continuable = true
                    )
                  )
                } yield ()
              }
            } yield ()
        }
    }
    */
  def internalIdent(s: String): Ident = Ident(s)


  /*
  def updatePreds(cfg:CFG):CFG = cfg.toList.fodlLeft(cfg)( (g,p) => p match {
    case (l,node) => {
      val ss = node.stmts
    }
  })
   */

  /*
  there are situation a phantom node (a label is mentioned in some goto or loop exit, but
  there is no statement, hence, buildCFG will not generate such node. see\
  int f(int x) {
    while (1) { // 0
      if (x < 0) { // 1
        return x; // 2
      } else {
        x--; // 3
      }
    }
    // 4
  }
  note that 4 is phantom as succs of the failure etst of (1) at 0, which is not reachable,
  but we need that empty node (and lambda function to be present) for the target code to be valid
   */

  /*
  def insertPhantoms(cfg: CFG): CFG = {
    // succ lbl and source lbl
    val allSuccsWithPreds: List[(Ident, Ident)] = cfg.toList.flatMap({
      case (lbl, n) => n.succs.map(succ => (succ, lbl))
    })
    // succ lbl -> [source lbl]
    val empty: Map[Ident, List[Ident]] = Map()
    val phanTomSuccWithPreds: Map[Ident, List[Ident]] = allSuccsWithPreds
      .filter({ case (succ_lbl, lbl) => !(cfg.contains(succ_lbl)) })
      .foldLeft(empty)((m, p) =>
        p match {
          case (succ_lbl, lbl) =>
            m.get(succ_lbl) match {
              case None       => m + (succ_lbl -> List(lbl))
              case Some(lbls) => m + (succ_lbl -> (lbls ++ List(lbl)))
            }
        }
      )
    phanTomSuccWithPreds.toList
      .map(p =>
        p match {
          case (lbl, preds) =>
            (lbl, Node(Nil, Nil, Nil, Nil, preds, Nil, Other))
        }
      )
      .foldLeft(cfg)((g, p) =>
        p match { case (lbl, node) => cfg + (lbl -> node) }
      )
  }
  */
  def formalArgsAsDecls(idents: List[Ident], cfg: CFG): CFG = {
    val entryLabel = List(0)
    cfg.get(entryLabel) match {
      case None => cfg
      case Some(n) => cfg + (entryLabel -> appLVars(n, idents))
    }
  }


  trait HasVar[A] {
    def getVarsFrom(a: A): List[Ident]

    def getLVarsFrom(a: A): List[Ident] = List()
  }

  object HasVarcfgOps {
    def getVarsFrom[A](a: A)(using hv: HasVar[A]): List[Ident] =
      hv.getVarsFrom(a)

    def getLVarsFrom[A](a: A)(using hv: HasVar[A]): List[Ident] =
      hv.getLVarsFrom(a)
  }

  given getVarsFromVarDecl: HasVar[VarDecl] =
    new HasVar[VarDecl] {
      override def getVarsFrom(var_decl: VarDecl): List[Ident] =
        var_decl match {
          case VarDecl(var_decl_id, None) => List()
          case VarDecl(var_decl_id, Some(var_init)) =>
            HasVarcfgOps.getVarsFrom(var_init)
        }

      override def getLVarsFrom(var_decl: VarDecl): List[Ident] =
        var_decl match {
          case VarDecl(var_decl_id, var_init) =>
            List(idFromVarDeclId(var_decl_id))
        }
    }

  given getVarsFromVarInit: HasVar[VarInit] =
    new HasVar[VarInit] {
      override def getVarsFrom(var_init: VarInit): List[Ident] =
        var_init match {
          case InitExp(exp) => HasVarcfgOps.getVarsFrom(exp)
          case InitArray(ArrayInit(var_inits)) =>
            var_inits.flatMap(HasVarcfgOps.getVarsFrom(_))
        }

      override def getLVarsFrom(var_init: VarInit): List[Ident] =
        var_init match {
          case InitExp(exp) => HasVarcfgOps.getLVarsFrom(exp)
          case InitArray(ArrayInit(var_inits)) =>
            var_inits.flatMap(HasVarcfgOps.getLVarsFrom(_))
        }
    }

  given getVarsFromExp: HasVar[Exp] =
    new HasVar[Exp] {
      override def getLVarsFrom(exp: Exp): List[Ident] =
        exp match {
          case Lit(lit) => List()
          case ClassLit(ty) => List()
          case This => List()
          case ThisClass(name) => List()
          case InstanceCreation(type_args, type_decl, args, body) => List()
          case QualInstanceCreation(exp, type_args, id, args, body) => List()
          case ArrayCreate(ty, exps, num_dims) => exps.flatMap(getLVarsFrom(_))
          case ArrayCreateInit(ty, size, init) =>
            init match {
              case ArrayInit(var_inits) =>
                var_inits.flatMap(HasVarcfgOps.getLVarsFrom(_))
            }
          case FieldAccess_(access) => HasVarcfgOps.getLVarsFrom(access)
          case MethodInv(methodInv) => HasVarcfgOps.getLVarsFrom(methodInv)
          case ArrayAccess(idx) => HasVarcfgOps.getLVarsFrom(idx)
          case ExpName(name) => List()
          case PostIncrement(exp) => getLVarsFrom(exp)
          case PostDecrement(exp) => getLVarsFrom(exp)
          case PreIncrement(exp) => getLVarsFrom(exp)
          case PreDecrement(exp) => getLVarsFrom(exp)
          case PrePlus(exp) => getLVarsFrom(exp)
          case PreMinus(exp) => getLVarsFrom(exp)
          case PreBitCompl(exp) => getLVarsFrom(exp)
          case PreNot(exp) => getLVarsFrom(exp)
          case Cast(ty, exp) => getLVarsFrom(exp)

          case BinOp(e1, op, e2) => getLVarsFrom(e1) ++ getLVarsFrom(e2)
          case InstanceOf(e, ref_type) => getLVarsFrom(e)
          case Cond(cond, true_exp, false_exp) =>
            getLVarsFrom(cond) ++ getLVarsFrom(true_exp) ++ getLVarsFrom(
              false_exp
            )
          case Assign(lhs, op, rhs) =>
            getLVarsFrom(rhs) ++ HasVarcfgOps.getLVarsFrom(lhs)
          case Lambda(params, body) => {
            val ps = HasVarcfgOps.getVarsFrom(params).toSet
            HasVarcfgOps.getLVarsFrom(body).filterNot(ps)
          }
          case MethodRef(name, id) => List()

        }

      override def getVarsFrom(exp: Exp): List[Ident] =
        exp match {
          case Lit(lit) => List()
          case ClassLit(ty) => List()
          case This => List()
          case ThisClass(name) => List()
          case InstanceCreation(type_args, type_decl, args, body) =>
            args.flatMap(HasVarcfgOps.getVarsFrom(_))
          // note: we can skip the body because any reference to the variables defined the enclosing scope
          //       because those variables must be final or effectively final
          case QualInstanceCreation(exp, type_args, id, args, body) =>
            args.flatMap(HasVarcfgOps.getVarsFrom(_))
          case ArrayCreate(ty, exps, num_dims) => exps.flatMap(getVarsFrom(_))
          case ArrayCreateInit(ty, size, init) =>
            init match {
              case ArrayInit(var_inits) =>
                var_inits.flatMap(HasVarcfgOps.getVarsFrom(_))
            }
          case FieldAccess_(access) => HasVarcfgOps.getVarsFrom(access)
          case MethodInv(methodInv) => HasVarcfgOps.getVarsFrom(methodInv)
          case ArrayAccess(idx) => HasVarcfgOps.getVarsFrom(idx)
          case ExpName(name) => HasVarcfgOps.getVarsFrom(name)
          case PostIncrement(exp) => getVarsFrom(exp)
          case PostDecrement(exp) => getVarsFrom(exp)
          case PreIncrement(exp) => getVarsFrom(exp)
          case PreDecrement(exp) => getVarsFrom(exp)
          case PrePlus(exp) => getVarsFrom(exp)
          case PreMinus(exp) => getVarsFrom(exp)
          case PreBitCompl(exp) => getVarsFrom(exp)
          case PreNot(exp) => getVarsFrom(exp)
          case Cast(ty, exp) => getVarsFrom(exp)
          case BinOp(e1, op, e2) => getVarsFrom(e1) ++ getVarsFrom(e2)
          case InstanceOf(e, ref_type) => getVarsFrom(e)
          case Cond(cond, true_exp, false_exp) =>
            getVarsFrom(cond) ++ getVarsFrom(true_exp) ++ getVarsFrom(false_exp)
          case Assign(lhs, op, rhs) =>
            getVarsFrom(
              rhs
            ) // HasVarcfgOps.getVarsFrom(lhs) ++ getVarsFrom(rhs)
          case Lambda(params, body) => {
            val ps = HasVarcfgOps.getVarsFrom(params).toSet
            HasVarcfgOps.getVarsFrom(body).filterNot(ps)
          }
          case MethodRef(name, id) => List()
        }
    }

  given getVarsFromFieldAccess: HasVar[FieldAccess] =
    new HasVar[FieldAccess] {
      override def getLVarsFrom(field_access: FieldAccess): List[Ident] =
        List() // TODO: check whether it should indeed empty
      override def getVarsFrom(field_access: FieldAccess): List[Ident] =
        field_access match {
          case PrimaryFieldAccess(e, id) => HasVarcfgOps.getVarsFrom(e)
          case SuperFieldAccess(id) => List()
          case ClassFieldAccess(name, id) => List()
        }
    }

  given getVarsFromMethodInvocation: HasVar[MethodInvocation] =
    new HasVar[MethodInvocation] {
      override def getVarsFrom(methodInv: MethodInvocation): List[Ident] =
        methodInv match {
          case MethodCall(name, args) =>
            HasVarcfgOps.getVarsFrom(name) ++ args.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            )
          case PrimaryMethodCall(e, ref_type, id, args) =>
            HasVarcfgOps.getVarsFrom(e) ++ args.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            )
          case SuperMethodCall(ref_types, id, args) =>
            args.flatMap(HasVarcfgOps.getVarsFrom(_))
          case ClassMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarcfgOps.getVarsFrom(_))
          case TypeMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarcfgOps.getVarsFrom(_))
        }

      override def getLVarsFrom(methodInv: MethodInvocation): List[Ident] =
        methodInv match {
          case MethodCall(name, args) =>
            args.flatMap(HasVarcfgOps.getLVarsFrom(_))
          case PrimaryMethodCall(e, ref_type, id, args) =>
            HasVarcfgOps.getLVarsFrom(e) ++ args.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            )
          case SuperMethodCall(ref_types, id, args) =>
            args.flatMap(HasVarcfgOps.getLVarsFrom(_))
          case ClassMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarcfgOps.getLVarsFrom(_))
          case TypeMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarcfgOps.getLVarsFrom(_))
        }
    }

  given getVarsFromArrayIndex: HasVar[ArrayIndex] =
    new HasVar[ArrayIndex] {
      override def getVarsFrom(idx: ArrayIndex): List[Ident] =
        idx match {
          case ArrayIndex(e, es) =>
            HasVarcfgOps.getVarsFrom(e) ++ es.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            )
        }

      override def getLVarsFrom(idx: ArrayIndex): List[Ident] =
        idx match {
          case ArrayIndex(e, es) =>
            HasVarcfgOps.getLVarsFrom(e) ++ es.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            )
        }
    }

  given getVarsFromName: HasVar[Name] =
    new HasVar[Name] {
      override def getVarsFrom(n: Name): List[Ident] =
        n match {
          case Name(id :: Nil) =>
            List(
              id
            ) // TODO: double check, is it safe to assume names are not variable?
          case Name(_) => List()
        }

      override def getLVarsFrom(n: Name): List[Ident] =
        n match {
          case Name(ids) =>
            List() // TODO: double check, is it safe to assume names are not variable?
        }
    }

  given getVarsFromLhs: HasVar[Lhs] =
    new HasVar[Lhs] {
      override def getVarsFrom(lhs: Lhs): List[Ident] =
        lhs match {
          case NameLhs(name) => List()
          case FieldLhs(field_access) => HasVarcfgOps.getVarsFrom(field_access)
          case ArrayLhs(array_idx) => HasVarcfgOps.getVarsFrom(array_idx)
        }

      override def getLVarsFrom(lhs: Lhs): List[Ident] =
        lhs match {
          case NameLhs(name) => HasVarcfgOps.getVarsFrom(name)
          case FieldLhs(field_access) =>
            List() // TODO: double check, is it safe to assume field access has no lhs var
          case ArrayLhs(array_idx) =>
            List() // TODO: double check, is it safe to assume array index has no lhs var
        }
    }

  given getVarsFromLambdaParams: HasVar[LambdaParams] =
    new HasVar[LambdaParams] {
      override def getVarsFrom(ps: LambdaParams): List[Ident] =
        ps match {
          case LambdaSingleParam(id) => List(id)
          case LambdaFormalParams(formal_params) =>
            formal_params.flatMap(HasVarcfgOps.getVarsFrom(_))
          case LambdaInferredParams(ids) => ids
        }
    }

  given getVarsFromLambdaExpression: HasVar[LambdaExpression] =
    new HasVar[LambdaExpression] {
      override def getVarsFrom(lambExp: LambdaExpression): List[Ident] =
        lambExp match {
          case LambdaExpression_(e) => HasVarcfgOps.getVarsFrom(e)
          case LambdaBlock(blk) => HasVarcfgOps.getVarsFrom(blk)
        }

      override def getLVarsFrom(lambExp: LambdaExpression): List[Ident] =
        lambExp match {
          case LambdaExpression_(e) => HasVarcfgOps.getLVarsFrom(e)
          case LambdaBlock(blk) => HasVarcfgOps.getLVarsFrom(blk)
        }

    }

  given getVarsFromBlock: HasVar[Block] =
    new HasVar[Block] {
      override def getVarsFrom(blk: Block): List[Ident] =
        blk match {
          case Block(stmts) => {
            val localVarStmts = stmts.filter(isLocalVarsBlockStmt(_))
            val others = stmts.filter(!isLocalVarsBlockStmt(_))
            val localVars = localVarStmts
              .flatMap(stmt =>
                stmt match {
                  case LocalVars(modifiers, ty, var_decls) =>
                    var_decls.flatMap(HasVarcfgOps.getLVarsFrom(_))
                  case _ => List()
                }
              )
              .toSet
            val otherVars = others.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            ) ++ localVarStmts.flatMap(stmt =>
              stmt match {
                case LocalVars(modifiers, ty, var_decls) =>
                  var_decls.flatMap(HasVarcfgOps.getVarsFrom(_))
                case _ => List()
              }
            )
            otherVars.filterNot(localVars)
          }
        }

      override def getLVarsFrom(blk: Block): List[Ident] =
        blk match {
          case Block(stmts) => {
            val localVarStmts = stmts.filter(isLocalVarsBlockStmt(_))
            val others = stmts.filter(!isLocalVarsBlockStmt(_))
            val localVars = localVarStmts
              .flatMap(stmt =>
                stmt match {
                  case LocalVars(modifiers, ty, var_decls) =>
                    var_decls.flatMap(HasVarcfgOps.getLVarsFrom(_))
                  case _ => List()
                }
              )
              .toSet
            val otherVars = others.flatMap(HasVarcfgOps.getLVarsFrom(_))
            otherVars.filterNot(localVars)
          }
        }
    }

  given getVarsFromBlockStmt: HasVar[BlockStmt] =
    new HasVar[BlockStmt] {
      override def getVarsFrom(blkStmt: BlockStmt): List[Ident] =
        blkStmt match {
          case BlockStmt_(stmt) => HasVarcfgOps.getVarsFrom(stmt)
          case LocalClass(class_decl) => List() // TODO:Fixme
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarcfgOps.getVarsFrom(_))
        }

      override def getLVarsFrom(blkStmt: BlockStmt): List[Ident] =
        blkStmt match {
          case BlockStmt_(stmt) => HasVarcfgOps.getLVarsFrom(stmt)
          case LocalClass(class_decl) => List() // TODO:Fixme
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarcfgOps.getLVarsFrom(_))
        }
    }

  given getVarsFromStmt: HasVar[Stmt] =
    new HasVar[Stmt] {
      override def getVarsFrom(stmt: Stmt): List[Ident] =
        stmt match {
          case StmtBlock(blk) => HasVarcfgOps.getVarsFrom(blk)
          case IfThen(exp, stmt) =>
            HasVarcfgOps.getVarsFrom(exp) ++ getVarsFrom(stmt)
          case IfThenElse(exp, then_stmt, else_stmt) =>
            HasVarcfgOps.getVarsFrom(exp) ++ getVarsFrom(
              then_stmt
            ) ++ getVarsFrom(
              else_stmt
            )
          case While(exp, stmt) =>
            HasVarcfgOps.getVarsFrom(exp) ++ getVarsFrom(stmt)
          case BasicFor(init, loop_cond, post_update, stmt) => {
            val s = init.toList.flatMap(HasVarcfgOps.getLVarsFrom(_)).toSet
            val vs = init.toList.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            ) ++ loop_cond.toList.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            ) ++ post_update.toList.flatMap(x =>
              x.flatMap(y => HasVarcfgOps.getVarsFrom(y))
            ) ++ getVarsFrom(stmt)
            vs.filterNot(s)
          }
          case EnhancedFor(modifiers, ty, id, exp, stmt) => {
            HasVarcfgOps.getVarsFrom(exp) ++ getVarsFrom(stmt)
          }
          case Empty => List()
          case ExpStmt(exp) => HasVarcfgOps.getVarsFrom(exp)
          case Assert(exp, msg) =>
            HasVarcfgOps.getVarsFrom(exp) ++ msg.toList.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            )
          case Switch(exp, blocks) =>
            HasVarcfgOps.getVarsFrom(exp) ++ blocks.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            )
          case Do(stmt, exp) =>
            getVarsFrom(stmt) ++ HasVarcfgOps.getVarsFrom(exp)
          case Break(_) => List()
          case Continue(_) => List()
          case Return(exp) => exp.toList.flatMap(HasVarcfgOps.getVarsFrom(_))
          case Synchronized(exp, blk) =>
            HasVarcfgOps.getVarsFrom(exp) ++ HasVarcfgOps.getVarsFrom(blk)
          case Throw(exp) => HasVarcfgOps.getVarsFrom(exp)
          case Try(try_blk, catches, finally_blk) =>
            HasVarcfgOps.getVarsFrom(try_blk) ++ catches.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            ) ++ finally_blk.toList.flatMap(HasVarcfgOps.getVarsFrom(_))
          case Labeled(id, stmt) => getVarsFrom(stmt)
        }

      override def getLVarsFrom(stmt: Stmt): List[Ident] =
        stmt match {
          case StmtBlock(blk) => HasVarcfgOps.getLVarsFrom(blk)
          case IfThen(exp, stmt) =>
            HasVarcfgOps.getLVarsFrom(exp) ++ getLVarsFrom(stmt)
          case IfThenElse(exp, then_stmt, else_stmt) =>
            HasVarcfgOps.getLVarsFrom(exp) ++ getLVarsFrom(
              then_stmt
            ) ++ getLVarsFrom(else_stmt)
          case While(exp, stmt) =>
            HasVarcfgOps.getLVarsFrom(exp) ++ getLVarsFrom(stmt)
          case BasicFor(init, loop_cond, post_update, stmt) => {
            val s = init.toList.flatMap(HasVarcfgOps.getLVarsFrom(_)).toSet
            val vs = loop_cond.toList.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            ) ++ post_update.toList.flatMap(x =>
              x.flatMap(y => HasVarcfgOps.getLVarsFrom(y))
            ) ++ getLVarsFrom(stmt)
            vs.filterNot(s)
          }
          case EnhancedFor(modifiers, ty, id, exp, stmt) => {
            HasVarcfgOps.getLVarsFrom(exp) ++ getLVarsFrom(stmt).filterNot(
              Set(id)
            )
          }
          case Empty => List()
          case ExpStmt(exp) => HasVarcfgOps.getLVarsFrom(exp)
          case Assert(exp, msg) =>
            HasVarcfgOps.getLVarsFrom(exp) ++ msg.toList.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            )
          case Switch(exp, blocks) =>
            HasVarcfgOps.getLVarsFrom(exp) ++ blocks.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            )
          case Do(stmt, exp) =>
            getLVarsFrom(stmt) ++ HasVarcfgOps.getLVarsFrom(exp)
          case Break(_) => List()
          case Continue(_) => List()
          case Return(exp) => exp.toList.flatMap(HasVarcfgOps.getLVarsFrom(_))
          case Synchronized(exp, blk) =>
            HasVarcfgOps.getLVarsFrom(exp) ++ HasVarcfgOps.getLVarsFrom(blk)
          case Throw(exp) => HasVarcfgOps.getLVarsFrom(exp)
          case Try(try_blk, catches, finally_blk) =>
            HasVarcfgOps.getLVarsFrom(try_blk) ++ catches.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            ) ++ finally_blk.toList.flatMap(HasVarcfgOps.getLVarsFrom(_))
          case Labeled(id, stmt) => getLVarsFrom(stmt)
        }
    }

  given getVarsFromCatch: HasVar[Catch] =
    new HasVar[Catch] {
      override def getVarsFrom(c: Catch): List[Ident] =
        c match {
          case Catch(params, blk) => {
            val ps = HasVarcfgOps.getVarsFrom(params).toSet
            HasVarcfgOps.getVarsFrom(blk).filterNot(ps)
          }
        }

      override def getLVarsFrom(c: Catch): List[Ident] =
        c match {
          case Catch(params, blk) => {
            val ps = HasVarcfgOps.getVarsFrom(params).toSet
            HasVarcfgOps.getLVarsFrom(blk).filterNot(ps)
          }
        }
    }

  given getVarsFromSwitchBlock: HasVar[SwitchBlock] =
    new HasVar[SwitchBlock] {
      override def getVarsFrom(switch_block: SwitchBlock): List[Ident] =
        switch_block match {
          case SwitchBlock(label, blk_stmts) =>
            HasVarcfgOps.getVarsFrom(label) ++ blk_stmts.flatMap(
              HasVarcfgOps.getVarsFrom(_)
            )
        }

      override def getLVarsFrom(switch_block: SwitchBlock): List[Ident] =
        switch_block match {
          case SwitchBlock(label, blk_stmts) =>
            HasVarcfgOps.getLVarsFrom(label) ++ blk_stmts.flatMap(
              HasVarcfgOps.getLVarsFrom(_)
            )
        }
    }

  given getVarsFromSwitchLabel: HasVar[SwitchLabel] =
    new HasVar[SwitchLabel] {
      override def getVarsFrom(switch_label: SwitchLabel): List[Ident] =
        switch_label match {
          case SwitchCase(exp) => HasVarcfgOps.getVarsFrom(exp)
          case Default => List()
        }

      override def getLVarsFrom(switch_label: SwitchLabel): List[Ident] =
        switch_label match {
          case SwitchCase(exp) => HasVarcfgOps.getLVarsFrom(exp)
          case Default => List()
        }
    }

  given getVarsFromFormalParam: HasVar[FormalParam] =
    new HasVar[FormalParam] {
      override def getVarsFrom(fp: FormalParam): List[Ident] =
        fp match {
          case FormalParam(modifiers, ty, has_arity, var_decl_id) =>
            List(idFromVarDeclId(var_decl_id))
        }

      override def getLVarsFrom(fp: FormalParam): List[Ident] =
        fp match {
          case FormalParam(modifiers, ty, has_arity, var_decl_id) =>
            List(idFromVarDeclId(var_decl_id))
        }
    }

  given getVarsFromForInit: HasVar[ForInit] =
    new HasVar[ForInit] {
      override def getVarsFrom(for_init: ForInit): List[Ident] =
        for_init match {
          case ForLocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarcfgOps.getVarsFrom(_))
          case ForInitExps(es) => es.flatMap(HasVarcfgOps.getVarsFrom(_))
        }

      override def getLVarsFrom(for_init: ForInit): List[Ident] =
        for_init match {
          case ForLocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarcfgOps.getLVarsFrom(_))
          case ForInitExps(es) => es.flatMap(HasVarcfgOps.getLVarsFrom(_))
        }
    }

  trait ConvertableToLhs[A] {
    def getLhsFrom(a: A): List[Lhs]
  }

  object ConvertableToLhscfgOps {
    def getLhsFrom[A](a: A)(using hn: ConvertableToLhs[A]): List[Lhs] =
      hn.getLhsFrom(a)
  }

  given expConvertableToLhs: ConvertableToLhs[Exp] =
    new ConvertableToLhs[Exp] {
      // dominating name
      override def getLhsFrom(exp: Exp): List[Lhs] =
        exp match {
          case Lit(lit) => List()
          case ClassLit(ty) => List()
          case This => List()
          case ThisClass(name) => List()
          case InstanceCreation(type_args, type_decl, args, body) => List()
          case QualInstanceCreation(exp, type_args, id, args, body) => List()
          case ArrayCreate(ty, exps, num_dims) => List()
          case ArrayCreateInit(ty, size, init) => List()
          case FieldAccess_(access) => List(FieldLhs(access))
          case MethodInv(methodInv) => List()
          case ArrayAccess(idx) => List(ArrayLhs(idx))
          case ExpName(name) => List(NameLhs(name))
          case PostIncrement(exp) => getLhsFrom(exp)
          case PostDecrement(exp) => getLhsFrom(exp)
          case PreIncrement(exp) => getLhsFrom(exp)
          case PreDecrement(exp) => getLhsFrom(exp)
          case PrePlus(exp) => getLhsFrom(exp)
          case PreMinus(exp) => getLhsFrom(exp)
          case PreBitCompl(exp) => getLhsFrom(exp)
          case PreNot(exp) => getLhsFrom(exp)
          case Cast(ty, exp) => getLhsFrom(exp)
          case BinOp(e1, op, e2) => List()
          case InstanceOf(e, ref_type) => List()
          case Cond(cond, true_exp, false_exp) => List()
          case Assign(lhs, op, rhs) => List(lhs)
          case Lambda(params, body) => List()
          case MethodRef(name, id) => List()
        }
    }

}
