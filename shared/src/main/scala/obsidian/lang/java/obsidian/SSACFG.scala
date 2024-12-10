package obsidian.lang.java.obsidian

import cats.*
import cats.implicits.*


import cats._
import cats.implicits._

import cats.data.StateT
import cats.{ApplicativeError, Functor, MonadError}
import obsidian.lang.java.obsidian.ASTPath.ASTPath
import obsidian.lang.java.obsidian.SSA.{DomTree, Phi, SDomTable}
import obsidian.lang.java.scalangj.Syntax.{Ident, MethodDecl}

import scala.annotation.tailrec

// an implemntation of Cytron's algorithm
object SSACFG {

  type NodeId = ASTPath

  type SSACFG = Map[ASTPath, SSANode]

  /**
    * assumptions made:
    * any variable that is declared in a function/catch node is not assigned to
    */

  sealed trait SSANode

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
                              succs: List[NodeId],
                              rhsVarReplacements: Map[NodeId, Map[Ident, Int]],
                              lhsVarReplacements: Map[NodeId, Map[Ident, Int]]
                            ) extends SSANode

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
                             succs: List[NodeId],
                             rhsVarReplacements: Map[Ident, Int],
                             joins: Map[Ident, (Int, Map[NodeId, Int])]
                           ) extends SSANode


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
                         succs: List[NodeId],
                         rhsVarReplacements: Map[Ident, Int],
                         joins: Map[Ident, (Int, Phi)]
                       ) extends SSANode


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
                        succs: List[NodeId],
                        rhsVarReplacements: Map[Ident, Int],
                        preJoins: Map[Ident, (Int, Map[NodeId, Int])],
                        postJoins: Map[Ident, (Int, Map[NodeId, Int])],
                      ) extends SSANode

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
                                  succs: List[NodeId],
                                  catchJoins: Map[Ident, (Int, Phi)],
                                  finallyJoins: Map[Ident, (Int, Phi)]
                                ) extends SSANode


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
                         preds: List[NodeId],
                         rhsVarReplacements: Map[Ident, Int],
                       ) extends SSANode

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
                        succs: List[NodeId],
                        rhsVarReplacements: Map[Ident, Int],
                      ) extends SSANode

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
                         succs: List[NodeId],
                         rhsVarReplacements: Map[Ident, Int],
                       ) extends SSANode


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
                        succs: List[NodeId],
                      ) extends SSANode

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
                       succs: List[NodeId],
                     ) extends SSANode

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
                        ) extends SSANode


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
                     ) extends SSANode

  def nodeToSsaNode(node: CFG.Node): SSANode = {
    node match {
      case CFG.AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) =>
        AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs, Map(), Map())
      case CFG.IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) =>
        IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs, Map(), Map())
      case CFG.SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>
        SwitchNode(id, caseNodes, lVars, rVars, preds, succs, Map(), Map())
      case CFG.WhileNode(id, bodyNode, lVars, rVars, preds, succs) =>
        WhileNode(id, bodyNode, lVars, rVars, preds, succs, Map(), Map(), Map())
      case CFG.TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs) =>
        TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs, Map(), Map())
      case CFG.ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lVars, rVars, preds, Map())
      case CFG.ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rVars, preds, succs, Map())
      case CFG.AssertNode(id, lVars, rVars, preds, succs) => AssertNode(id, lVars, rVars, preds, succs, Map())
      case CFG.BreakNode(id, preds, succs) => BreakNode(id, preds, succs)
      case CFG.ContNode(id, preds, succs) => ContNode(id, preds, succs)
      case CFG.DefaultNode(id, stmts, preds, succs) => DefaultNode(id, stmts, preds, succs)
      case CFG.CaseNode(id, stmts, preds, succs) => CaseNode(id, stmts, preds, succs)
    }
  }

  def cfgToSsacfg(cfg: CFG.CFG): SSACFG = {
    cfg.map((path, node) => (path, nodeToSsaNode(node)))
  }


  sealed trait SsacfgResult[+A]

  case class SsacfgError(msg: String) extends SsacfgResult[Nothing]

  case class SsacfgOk[A](result: A) extends SsacfgResult[A]

  given cfgResultFunctor: Functor[SsacfgResult] =
    new Functor[SsacfgResult] {
      override def map[A, B](fa: SsacfgResult[A])(f: A => B): SsacfgResult[B] =
        fa match {
          case SsacfgError(s) => SsacfgError(s)
          case SsacfgOk(a) => SsacfgOk(f(a))
        }
    }


  given ssaCfgResultApplicative: ApplicativeError[SsacfgResult, String] =
    new ApplicativeError[SsacfgResult, String] {
      override def ap[A, B](
                             ff: SsacfgResult[A => B]
                           )(fa: SsacfgResult[A]): SsacfgResult[B] =
        ff match {
          case SsacfgOk(f) =>
            fa match {
              case SsacfgOk(a) => SsacfgOk(f(a))
              case SsacfgError(s) => SsacfgError(s)
            }
          case SsacfgError(s) => SsacfgError(s)
        }

      override def pure[A](a: A): SsacfgResult[A] = SsacfgOk(a)

      override def raiseError[A](e: String): SsacfgResult[A] = SsacfgError(e)

      override def handleErrorWith[A](
                                       fa: SsacfgResult[A]
                                     )(f: String => SsacfgResult[A]): SsacfgResult[A] =
        fa match {
          case SsacfgError(s) => f(s)
          case SsacfgOk(a) => SsacfgOk(a)
        }
    }

  given ssaCfgResultMonadError(using
                                      app: ApplicativeError[SsacfgResult, String]
                                     ): MonadError[SsacfgResult, String] =
    new MonadError[SsacfgResult, String] {
      override def raiseError[A](e: String): SsacfgResult[A] = app.raiseError(e)

      override def handleErrorWith[A](fa: SsacfgResult[A])(
        f: String => SsacfgResult[A]
      ): SsacfgResult[A] = app.handleErrorWith(fa)(f)

      override def flatMap[A, B](
                                  fa: SsacfgResult[A]
                                )(f: A => SsacfgResult[B]): SsacfgResult[B] =
        fa match {
          case SsacfgOk(a) => f(a)
          case SsacfgError(s) => SsacfgError(s)
        }

      override def pure[A](a: A): SsacfgResult[A] = app.pure(a)

      @annotation.tailrec
      def tailRecM[A, B](
                          init: A
                        )(fn: A => SsacfgResult[Either[A, B]]): SsacfgResult[B] =
        fn(init) match {
          case SsacfgError(msg) => SsacfgError(msg)
          case SsacfgOk(Right(b)) => SsacfgOk(b)
          case SsacfgOk(Left(a)) => tailRecM(a)(fn)
        }
    }

  case class SsaCfgInfo(info: SSA.StateInfo,
                        ssacfg: SSACFG,
                        idom: SSA.IDomTable,
                        sdom: SSA.SDomTable,
                        domTree: SSA.DomTree,
                        catchNodes: Map[NodeId, NodeId],
                        finallyNodes: Map[NodeId, NodeId])

  type SSACFGState[A] = StateT[SsacfgResult, SsaCfgInfo, A]

  def findCatchNodes(cfg: SSACFG): Map[NodeId, NodeId] = {
    val empty: Map[NodeId, NodeId] = Map()
    cfg.toList.foldLeft(empty) { case (nodes, (id, node)) =>
      node match {
        case n: TryCatchFinallyNode => nodes.updated(n.catchNodes ++ List(0), id)
        case _ => nodes
      }
    }
  }

  def findFinallyNodes(cfg: SSACFG): Map[NodeId, NodeId] = {
    val empty: Map[NodeId, NodeId] = Map()
    cfg.toList.foldLeft(empty) { case (nodes, (id, node)) =>
      node match {
        case n: TryCatchFinallyNode => nodes.updated(n.finallyNode, id)
        case _ => nodes
      }
    }
  }

  def updateVarReplacements(nodeId: NodeId, node: SSANode, info: SSA.StateInfo): SsacfgResult[SSANode] = {
    val rVarReplacements = info.rhsVarReplacements
    val lVarReplacements = info.lhsVarReplacements
    node match {
      case a: AssignmentsNode =>
        SsacfgOk(a.copy(rhsVarReplacements = rVarReplacements.filter( (path, v) => a.stmts contains path),
          lhsVarReplacements = lVarReplacements.filter((path,v) => a.stmts contains path)))
      case n: IfThenElseNode =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else SsacfgOk(n.copy(rhsVarReplacements = rVarReplacements.getOrElse(nodeId, Map())))
      case n: SwitchNode =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else SsacfgOk(n.copy(rhsVarReplacements = rVarReplacements.getOrElse(nodeId, Map())))
      case n: WhileNode =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else SsacfgOk(n.copy(rhsVarReplacements = rVarReplacements.getOrElse(nodeId, Map())))
      case n: ReturnNode =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else SsacfgOk(n.copy(rhsVarReplacements = rVarReplacements.getOrElse(nodeId, Map())))
      case n: ThrowNode =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else SsacfgOk(n.copy(rhsVarReplacements = rVarReplacements.getOrElse(nodeId, Map())))
      case n: AssertNode =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else SsacfgOk(n.copy(rhsVarReplacements = rVarReplacements.getOrElse(nodeId, Map())))
      case _ =>
        if (lVarReplacements contains nodeId) SsacfgError(f"Tried to assign LHS variable to node $nodeId")
        else if (rVarReplacements contains nodeId) SsacfgError(f"Tried to assign RHS variable to node $nodeId")
        else SsacfgOk(node)
    }
  }


  @tailrec
  private def newCommonDominator(preds: Set[NodeId], oldDom: NodeId, sdom: SDomTable, domTree: DomTree): NodeId = {
    val children = domTree.getOrElse(oldDom, Nil)
    children.find(child => preds.forall(sdom(child))) match {
      case Some(value) => newCommonDominator(preds, value, sdom, domTree)
      case None => oldDom
    }
  }

  private def mergeNonTryCatchPostJoinIfMoreThanOne(nodeId: NodeId, ident: Ident, rhs: Phi, dom: NodeId): SSACFGState[(NodeId, Int)] = {
    assert(rhs.nonEmpty, f"join empty:\nnodeId: $nodeId\ndom: $dom\n")
    if (rhs.size == 1) {
      StateT.pure[SsacfgResult, SsaCfgInfo, (NodeId, Int)](rhs.head)
    } else {
      for {
        i <- StateT.apply[SsacfgResult, SsaCfgInfo, Int] { s =>
          val (newStateInfo, i) = SSA.nextVarLabel(ident).run(s.info).value
          SsacfgOk((s.copy(info = newStateInfo), i))
        }
        newNode <- mergeNonTryCatchPostJoin(nodeId, ident, (i, rhs), dom)
      } yield (newNode, i)
    }
  }

  private def mergePostJoin(nodeId: NodeId, ident: Ident, join: (Int, Phi), dom: NodeId): SSACFGState[NodeId] = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      newNode <- if (s.catchNodes contains nodeId) {
        mergeCatchJoin(nodeId, ident, join)
      } else if (s.finallyNodes contains nodeId) {
        mergeFinallyJoin(nodeId, ident, join, dom)
      } else {
        mergeNonTryCatchPostJoin(nodeId, ident, join, dom)
      }
    } yield newNode
  }

  private def mergeNonTryCatchPostJoin(nodeId: NodeId, ident: Ident, join: (Int, Phi), dom: NodeId): SSACFGState[NodeId]  = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      nodeId <- s.ssacfg(dom) match {
        case n: IfThenElseNode =>
          mergeIfThenElseJoin(nodeId, ident, n, dom, join)
        case n: WhileNode =>
          mergeWhileNode(nodeId, ident, n, join, dom)
        case n: SwitchNode =>
          mergeSwitchNode(nodeId, ident, n, join, dom)
        case n =>
          StateT.applyF[SsacfgResult, SsaCfgInfo, NodeId](
            SsacfgError(f"Can't place join on node $nodeId with dominator $dom (node information below)\n$n"))
      }
    } yield nodeId
  }

  private def mergeSwitchNode(nodeId: NodeId, ident: Ident, n: SwitchNode, join: (Int, Phi), dom: NodeId) = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      breakRhs = join._2.filter((key,value) => s.ssacfg(key).isInstanceOf[BreakNode])
      nonBreakRhs = join._2.filter((key,value) => !s.ssacfg(key).isInstanceOf[BreakNode])
      nonBreakLhs <- if (nonBreakRhs.isEmpty) {
        StateT.pure[SsacfgResult, SsaCfgInfo, Option[(NodeId, Int)]](None)
      } else {
        mergeNonTryCatchPostJoinIfMoreThanOne(nodeId, ident, nonBreakRhs,
          newCommonDominator(nonBreakRhs.keySet, dom, s.sdom, s.domTree)).map(Some(_))
      }
      _ <- StateT.modify[SsacfgResult, SsaCfgInfo] { s =>
        val newDom = n.copy(joins = n.joins.updated(ident, (join._1, breakRhs ++ nonBreakLhs)))
        s.copy(ssacfg = s.ssacfg.updated(dom, newDom))
      }
    } yield dom
  }

  private def mergeWhileNode(nodeId: NodeId, ident: Ident, n: WhileNode, join: (Int, Phi), dom: NodeId) = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      breakRhs = join._2.filter((key,v) => s.ssacfg(key).isInstanceOf[BreakNode])
      nonBreakRhs = join._2.filter((key,v) => !s.ssacfg(key).isInstanceOf[BreakNode])
      nonBreakLhs <- if (nonBreakRhs.isEmpty) {
        StateT.pure[SsacfgResult, SsaCfgInfo, Option[(NodeId, Int)]](None)
      } else {
        mergeNonTryCatchPostJoinIfMoreThanOne(nodeId, ident, nonBreakRhs,
          newCommonDominator(nonBreakRhs.keySet, dom, s.sdom, s.domTree)).map(Some(_))
      }
      _ <- StateT.modify[SsacfgResult, SsaCfgInfo] { s =>
        val newDom = n.copy(postJoins = n.postJoins.updated(ident, (join._1, breakRhs ++ nonBreakLhs)))
        s.copy(ssacfg = s.ssacfg.updated(dom, newDom))
      }
    } yield dom
  }

  private def mergeCatchJoin(nodeId: NodeId, ident: Ident, join: (Int, Phi)): SSACFGState[NodeId] = {
    for {
      tryNode <- StateT.inspect[SsacfgResult, SsaCfgInfo, NodeId](s => s.catchNodes(nodeId))
      _ <- StateT.modifyF[SsacfgResult, SsaCfgInfo] { s =>
        s.ssacfg.get(tryNode) match {
          case Some(n: TryCatchFinallyNode) => SsacfgOk(
            s.copy(ssacfg = s.ssacfg.updated(tryNode, n.copy(catchJoins = n.catchJoins.updated(ident, join))))
          )
          case _ => SsacfgError(f"Tried to add a postjoin to corresponding try node " +
            f"$tryNode for catch node $nodeId, but did not find a TryCatchFinallyNode")
        }
      }
    } yield tryNode
  }

  private def mergeFinallyJoin(nodeId: NodeId, ident: Ident, join: (Int, Phi), dom: NodeId): SSACFGState[NodeId] = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      tryNodeId = s.finallyNodes(nodeId)
      tryNode <- s.ssacfg.get(tryNodeId) match {
        case Some(n: TryCatchFinallyNode) =>
          StateT.pure[SsacfgResult, SsaCfgInfo, TryCatchFinallyNode](n)
        case _ =>
          StateT.applyF[SsacfgResult, SsaCfgInfo, TryCatchFinallyNode](SsacfgError(f"Tried to add a postjoin to corresponding try node " +
            f"$tryNodeId for finally node $nodeId, but did not find a TryCatchFinallyNode"))
      }
      catchNode = tryNode.catchNodes
      preds = join._2.keySet
      // find all descendants of the catch node
      catchPreds = preds.filter(_.startsWith(catchNode))
      tryPreds = preds -- catchPreds

      mergeNodeId <- if (catchPreds.isEmpty || tryPreds.isEmpty) {
        mergeNonTryCatchPostJoin(nodeId, ident, join, dom)
      } else {
        // merge both catch and try, then merge
        for {
          catchLhs <- mergeNonTryCatchPostJoinIfMoreThanOne(nodeId, ident, join._2.filter((key,v) => catchPreds contains key),
            newCommonDominator(catchPreds, dom, s.sdom, s.domTree))
          tryLhs <- mergeNonTryCatchPostJoinIfMoreThanOne(nodeId, ident, join._2.filter((key,v) => tryPreds contains key),
            newCommonDominator(tryPreds, dom, s.sdom, s.domTree))
          _ <- StateT.modifyF[SsacfgResult, SsaCfgInfo] { s =>
            s.ssacfg.get(tryNodeId) match {
              case Some(n: TryCatchFinallyNode) => SsacfgOk(
                s.copy(ssacfg = s.ssacfg.updated(tryNodeId,
                  n.copy(finallyJoins = n.finallyJoins.updated(ident, (join._1, Map(catchLhs, tryLhs))))))
              )
              case _ => SsacfgError(f"Tried to add a postjoin to corresponding try node " +
                f"$tryNodeId for catch node $nodeId, but did not find a TryCatchFinallyNode")
            }
          }
        } yield tryNodeId
      }


    } yield mergeNodeId
  }

  private def mergeIfThenElseJoin(nodeId: NodeId, ident: Ident, n: IfThenElseNode, dom: NodeId, join: (Int, Phi)):SSACFGState[NodeId]  = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      thenNodeId = n.thenNode ++ List(0)
      elseNodeId = n.elseNode ++ List(0)
      thenRhs = join._2.filter((key, v)=> s.sdom.getOrElse(thenNodeId, Set()) + thenNodeId contains key)
      elseRhs = join._2.filter((key, v)=> s.sdom.getOrElse(elseNodeId, Set()) + elseNodeId contains key)
      thenLhs <- mergeNonTryCatchPostJoinIfMoreThanOne(
        nodeId, ident,
        join._2.filter((key, v) => thenRhs.keySet contains key),
        newCommonDominator(thenRhs.keySet, dom, s.sdom, s.domTree))
      elseLhs <- mergeNonTryCatchPostJoinIfMoreThanOne(
        nodeId, ident,
        join._2.filter((key, v) => elseRhs.keySet contains key),
        newCommonDominator(elseRhs.keySet, dom, s.sdom, s.domTree))
      _ <- StateT.modify[SsacfgResult, SsaCfgInfo] { s =>
        val newDom = n.copy(joins = n.joins.updated(ident, (join._1, Map(thenLhs, elseLhs))))
        s.copy(ssacfg = s.ssacfg.updated(dom, newDom))
      }
    } yield dom
  }

  def updateJoin(nodeId: NodeId, ident: Ident): SSACFGState[Unit] = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      lhs = s.info.phiLhsVarReplacements(nodeId)(ident)
      rhs = s.info.phiRhs(nodeId)(ident)
      preds = rhs.keySet
      backPreds = preds.intersect(s.sdom.getOrElse(nodeId, Set()))
      postJoinPreds = preds -- backPreds
      hasPreJoin = backPreds.nonEmpty
      hasPostJoin = postJoinPreds.size > 1

      postJoinOption <- if (hasPostJoin) {
        val (newStateInfo, i) = if (hasPreJoin) {
          SSA.nextVarLabel(ident).run(s.info).value
        } else {
          (s.info, lhs)
        }
        StateT.modify[SsacfgResult, SsaCfgInfo](s => s.copy(info = newStateInfo)) >>
          mergePostJoin(nodeId, ident, (i, rhs.filter((key, v) => postJoinPreds contains key)), s.idom(nodeId))
            .map(nodeId => Some((nodeId, i)))
      } else {
        StateT.pure[SsacfgResult, SsaCfgInfo, Option[(NodeId, Int)]](rhs.filterKeys(postJoinPreds).headOption)
      }
      _ <- if (hasPreJoin) {
        for {
          s <- StateT.get[SsacfgResult, SsaCfgInfo]
          _ <- s.ssacfg(nodeId) match {
            case n: WhileNode =>
              for {
                s <- StateT.get[SsacfgResult, SsaCfgInfo]
                continuePreds = backPreds.filter(pred => s.ssacfg(pred).isInstanceOf[ContNode])
                otherPreds = backPreds -- continuePreds
                otherJoinLhsOption <- if (otherPreds.nonEmpty) {
                  mergeNonTryCatchPostJoinIfMoreThanOne(nodeId, ident,
                    rhs.filter((key, v) => otherPreds contains key),
                    newCommonDominator(otherPreds, nodeId, s.sdom, s.domTree)).map(Some(_))
                } else {
                  StateT.pure[SsacfgResult, SsaCfgInfo, Option[(NodeId, Int)]](None)
                }
                _ <- StateT.modify[SsacfgResult, SsaCfgInfo] { s =>
                  s.copy(ssacfg = s.ssacfg.updated(nodeId,
                    n.copy(preJoins = n.preJoins.updated(ident, (lhs, rhs.filter((key, v) => continuePreds contains key) ++ otherJoinLhsOption ++ postJoinOption)))))
                }
              } yield ()
            case n =>
              StateT.applyF[SsacfgResult, SsaCfgInfo, NodeId](SsacfgError(f"Tried to add prejoin to node at $nodeId, but found the node below:\n$n"))
          }
        } yield ()
      } else {
        StateT.pure[SsacfgResult, SsaCfgInfo, Unit](())
      }

    } yield ()
  }


  def buildSSACFG(): SSACFGState[Unit] = {
    for {
      s <- StateT.get[SsacfgResult, SsaCfgInfo]
      catchNodes = findCatchNodes(s.ssacfg)
      _ <- s.ssacfg.toList.traverse_ { case (id, node) =>
        StateT.modifyF[SsacfgResult, SsaCfgInfo] { ssaCfgInfo =>
          val ssacfg = ssaCfgInfo.ssacfg
          updateVarReplacements(id, node, ssaCfgInfo.info).map {
            newNode => ssaCfgInfo.copy(ssacfg = ssacfg.updated(id, newNode))
          }
        }
      }
      _ <- s.info.phiLhsVarReplacements
        .toList
        .flatMap { case (id, phiLhsMap) => phiLhsMap.keySet.map((id, _)) }
        .traverse_ { case (id, ident) => updateJoin(id, ident) }
    } yield ()
  }

  def runSSACFG(methodDecl: MethodDecl, cfg: CFG.CFG, root: NodeId): SsacfgResult[SSACFG] = {
    val sdom = SSA.buildSDom(cfg, root)
    val idom = SSA.buildIDom(sdom)
    val domTree = SSA.buildDomTree(idom)
    val info = SSA.buildSSAStateInfo(methodDecl, cfg, root)
    val ssaCfg = cfgToSsacfg(cfg)
    val catchNodes = findCatchNodes(ssaCfg)
    val finallyNodes = findFinallyNodes(ssaCfg)
    val ssaCfgInfo = SsaCfgInfo(info, ssaCfg, idom, sdom, domTree, catchNodes, finallyNodes)

    buildSSACFG().run(ssaCfgInfo).map { case (s, _) => s.ssacfg }
  }

}
