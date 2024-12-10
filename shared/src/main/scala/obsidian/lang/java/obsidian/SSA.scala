package obsidian.lang.java.obsidian

import cats.*
import cats.implicits.*

import cats.data.State
import obsidian.lang.java.obsidian.CFG.{AssignmentsNode, CFG, HasVarcfgOps, Node, NodeId, getLVars, getRVars, getSuccs}
import obsidian.lang.java.scalangj.Syntax.{Ident, MethodDecl}

import scala.annotation.tailrec

object SSA {

  /**
    * Computes all nodes in a CFG reachable from a source node, excluding specific nodes
    *
    * @param cfg     The control flow graph.
    * @param source  The node to start searching from
    * @param exclude Nodes to exclude
    * @return A list of Nodes
    */
  def reach(cfg: CFG, source: NodeId, exclude: Set[NodeId]): List[NodeId] = {
    if (exclude.contains(source)) {
      List()
    } else {
      cfg.get(source) match {
        case Some(id) => source :: getSuccs(id).flatMap(reach(cfg, _, exclude + source))
        case None => List()
      }
    }
  }

  type SDomTable = Map[NodeId, Set[NodeId]]

  /**
    * Builds a table of nodes that are strictly dominated in a control flow graph.
    *
    * @param cfg  The control flow graph
    * @param root The root node
    * @return A map from NodeId to the NodeIds that it dominates.
    */
  def buildSDom(cfg: CFG, root: NodeId): SDomTable = {
    val ids = cfg.keySet

    // SDom(a) = All Nodes \ [a] \ (all nodes reachable excluding a)
    def sdoms(id: NodeId): Set[NodeId] = ids - id -- reach(cfg, root, Set(id)).toSet

    ids.map(id => id -> sdoms(id)).toMap
  }

  def isSDom(sdom: SDomTable, dominator: NodeId, dominatee: NodeId): Boolean = sdom.get(dominator) match {
    case Some(dominatees) => dominatees.contains(dominatee)
    case None => false
  }

  def isDom(sdom: SDomTable, dominator: NodeId, dominatee: NodeId): Boolean =
    dominator == dominatee || isSDom(sdom, dominator, dominatee)

  type IDomTable = Map[NodeId, NodeId]
  type DomTree = Map[NodeId, List[NodeId]]

  /**
    * From the SDom table, generates the immediate dominator relationships.
    *
    * @param sdom a table generated by buildSDom(...)
    * @return A map showing immediate dominator relationships
    */
  def buildIDom(sdom: SDomTable): IDomTable = { // sort sdom list by number of dominatees
    val sdomSorted = sdom.toList.sortBy(x => x._2.size)

    // for each node in the list, find the next node that dominates it.
    def buildIDom_(s: List[(NodeId, Set[NodeId])]): IDomTable = s match {
      case List() => Map()
      case List(x) => Map()
      case (x, _) :: xs => xs.find(y => y._2.contains(x)) match {
        case Some((dtor, _)) => buildIDom_(xs) + (x -> dtor)
        case None => throw new Exception("Can't find dominator for node " + x)
      }
    }

    buildIDom_(sdomSorted)
  }

  /**
    * Generates immediate dominator relationships from a CFG.
    *
    * @param cfg  the control flow graph
    * @param root the root node
    * @return A map showing immediate dominator relationships
    */
  def buildIDom(cfg: CFG, root: NodeId): IDomTable = {
    val sdom = buildSDom(cfg, root)
    buildIDom(sdom)
  }

  /**
    * Builds the dominance tree from immediate dominator relatioships
    *
    * @param idom A map of immediate dominator relationships
    * @return A map from nodes in the dominance tree to a list of children
    */
  def buildDomTree(idom: IDomTable): DomTree = {
    val initDTree = Map[NodeId, List[NodeId]]() withDefaultValue Nil
    idom.foldLeft(initDTree)((dt, idomPair) =>
      dt + (idomPair._2 -> (idomPair._1 :: dt(idomPair._2))))
  }

  /**
    * Builds dominance tree of a given CFG
    *
    * @param cfg  a control flow graph
    * @param root the root node
    * @return A map from nodes in the dominance tree to a list of children
    */
  def buildDomTree(cfg: CFG, root: NodeId): DomTree = {
    val idom = buildIDom(cfg, root)
    buildDomTree(idom)
  }

  type DFTable = Map[NodeId, Set[NodeId]]

  /**
    * Builds dominance frontier table of a given CFG
    *
    * @param cfg  a control flow graph
    * @param root the root node
    * @return A map from nodes to the corresponding set of dominance frontiers
    */
  def buildDFTable(cfg: CFG, root: NodeId): DFTable = {
    val idom = buildIDom(cfg, root)
    val dtree = buildDomTree(idom)
    buildDFTable(cfg, root, idom, dtree)
  }

  def buildDFTable(cfg: CFG, root: NodeId, idom: IDomTable, dtree: DomTree): DFTable = {
    def postorder(id: NodeId, acc: List[NodeId] = List()): List[NodeId] =
      dtree.getOrElse(id, List()).foldRight(id :: acc)(postorder)

    val initTable = Map[NodeId, Set[NodeId]]()
    val po = postorder(root)

    po.foldLeft(initTable)((table, id) => {
      val locals = getSuccs(cfg(id)).filter(!idom.get(_).contains(id)).toSet
      val dfup = dtree.getOrElse(id, List()).flatMap(table(_)).toSet.filter(!idom.get(_).contains(id))
      table + (id -> (locals ++ dfup))
    })
  }

  @tailrec
  def iterDF(dft: DFTable, s: Set[NodeId], acc: Set[NodeId] = Set()): Set[NodeId] = {
    if (s.isEmpty) {
      acc
    } else {
      val df = s.flatMap(dft.getOrElse(_, Set()))
      // values in acc were handled previously, so we don't check them again.
      iterDF(dft, df -- acc, acc ++ df)
    }
  }

  type VariableLocations = Map[Ident, Set[NodeId]]

  def lhsVarLocations(cfg: CFG): VariableLocations = {
    val lvLocsInit: Map[Ident, Set[NodeId]] = Map()
    //val inits: Map[Ident, Set[NodeId]]
    cfg.foldLeft(lvLocsInit) { (locs, node) =>
      val lvars: List[Ident] = getLVars(node._2) ++ (node._2 match {
        case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => localDecls
        case _ => List()
      })
      //for each node
      lvars.foldLeft(locs) { (inner_locs, lvar) =>
        //add nodeid to the set corresponding to all lvars
        inner_locs + (lvar -> (inner_locs.getOrElse(lvar, Set()) + node._1))
      }
    }
  }

  def phiLocs(dft: DFTable, lhsvars: VariableLocations): Map[Ident, Set[NodeId]] = 
    lhsvars.map( (ident, nodes) => (ident, iterDF(dft, nodes)) )

  // we enumerate different instances of the same variable with an Int.
  // map from predecessor -> corresponding int
  type Phi = Map[NodeId, Int]
  val emptyPhi: Phi = Map()

  case class StateInfo(
                        assignCounts: Map[Ident, Int],
                        phiRhs: Map[NodeId, Map[Ident, Phi]],
                        stacks: Map[Ident, List[Int]],
                        lhsVarReplacements: Map[ASTPath.ASTPath, Map[Ident, Int]],
                        rhsVarReplacements: Map[ASTPath.ASTPath, Map[Ident, Int]],
                        phiLhsVarReplacements: Map[NodeId, Map[Ident, Int]],
                      )

  def phiLocsToPhiRhs(philocs: Map[Ident, Set[NodeId]]): Map[NodeId, Map[Ident, Phi]] = {
    philocs.foldLeft(Map.empty[NodeId, Map[Ident, Phi]]) {
      case (acc, (v, set)) => set.foldLeft(acc) {
        case (innerAcc, id) => innerAcc.updated(id, innerAcc.getOrElse(id, Map()).updated(v, emptyPhi))
      }
    }
  }

  def initState(lhsVarLocs: VariableLocations, phiLocs: Map[Ident, Set[NodeId]]): StateInfo =
    StateInfo(
      lhsVarLocs.map((ident, ns) => (ident, 0)), // do we want these to be default Maps instead?
      phiLocsToPhiRhs(phiLocs),
      lhsVarLocs.map((ident, ns) => (ident, Nil)),
      Map(),
      Map(),
      Map(),
    )

  type Assignment = (ASTPath.ASTPath, List[Ident], List[Ident])

  def getAssignments(node: Node, id: NodeId, methodDecl: MethodDecl): List[Assignment] = node match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) =>
      stmts.map { path =>
        val blockStmt = ASTPath.queryOps.query(methodDecl, path).get
        val lvars = HasVarcfgOps.getLVarsFrom(blockStmt)
        val rvars = HasVarcfgOps.getVarsFrom(blockStmt)
        (path, lvars, rvars)
      }
    case _ => List((id, List(), getRVars(node)))
  }

  def nextVarLabel(v: Ident): State[StateInfo, Int] = {
    for {
      s <- State.get[StateInfo]
      //      _ = println(f"Getting new var label for $v")
      i = s.assignCounts(v)
      newCounts = s.assignCounts.updated(v, i + 1)
      newStacks = s.stacks.updated(v, i :: s.stacks(v))
      _ <- State.set(s.copy(assignCounts = newCounts, stacks = newStacks))
    } yield i
  }

  def getVarLabel(v: Ident): State[StateInfo, Int] = State.inspect(s => s.stacks(v).head)

  def popVarStack(v: Ident): State[StateInfo, Unit] =
    State.modify(s => s.copy(stacks = s.stacks.updated(v, s.stacks(v).tail)))

  def labelRHSVar(v: Ident, path: ASTPath.ASTPath): State[StateInfo, Unit] = {
    for {
      s <- State.get[StateInfo]
      //_ = println(f"Labelling RHS variable $v in node $path")
      _ <- if (s.stacks.contains(v)) {
        for {
          i <- getVarLabel(v)
          rhs = s.rhsVarReplacements
          newrhs = rhs + (path -> (rhs.getOrElse(path, Map()) + (v -> i)))
          _ <- State.set(s.copy(rhsVarReplacements = newrhs))
        } yield ()
      } else {
        println(f"RHS variable $v ignored at path $path")
        State.pure[StateInfo, Unit](())
      }
    } yield ()
  }

  def labelLHSVar(v: Ident, path: ASTPath.ASTPath): State[StateInfo, Unit] = {
    for {
      s <- State.get[StateInfo]
      //_ = println(f"Labelling LHS variable $v in node $path")
      i <- nextVarLabel(v)
      lhs = s.lhsVarReplacements
      newlhs = lhs + (path -> (lhs.getOrElse(path, Map()) + (v -> i)))
      _ <- State.modify[StateInfo](s_ => s_.copy(lhsVarReplacements = newlhs))
    } yield ()
  }

  def labelPhiLHSVar(v: Ident, id: NodeId): State[StateInfo, Unit] = {
    for {
      s <- State.get[StateInfo]
      i <- nextVarLabel(v)
      lhs = s.phiLhsVarReplacements
      newlhs = lhs + (id -> (lhs.getOrElse(id, Map()) + (v -> i)))
      _ <- State.modify[StateInfo](s => s.copy(phiLhsVarReplacements = newlhs))
    } yield ()
  }

  def search(cfg: CFG, nodeId: NodeId, domTree: DomTree, methodDecl: MethodDecl): State[StateInfo, Unit] = {
    val assigns = getAssignments(cfg(nodeId), nodeId, methodDecl)
    for {
      s <- State.get[StateInfo]
      phis = s.phiRhs.getOrElse(nodeId, Map())

      // if lhs variables unassigned, just initialise them first
      s2 <- State.get[StateInfo]
      _ <- s2.assignCounts.filter(pair => pair._2 == 0).keySet.toList.traverse_(ident => labelLHSVar(ident, Nil))

      // update lhs variables of phi
      _ <- phis.keySet.toList.traverse_(v => labelPhiLHSVar(v, nodeId))

      // update rhs, then lhs variables of each assignment
      _ <- assigns.traverse_ { case (path, lhsList, rhsList) =>
        rhsList.traverse_(v => labelRHSVar(v, path)) >> lhsList.traverse_(v => labelLHSVar(v, path))
      }

      // update successor phis
      succs = getSuccs(cfg(nodeId))
      _ <- succs.traverse_ { succ =>
        val succRhs = s2.phiRhs.getOrElse(succ, Map())
        succRhs.toList.traverse_ { case (v, phi) =>
          for {
            s <- State.get[StateInfo]
            i <- getVarLabel(v)
            newPhiRhs = s.phiRhs.updated(succ, s.phiRhs(succ).updated(v, phi.updated(nodeId, i)))
            _ <- State.set(s.copy(phiRhs = newPhiRhs))
          } yield ()
        }
      }

      // search all children
      _ <- domTree(nodeId).traverse_(child => search(cfg, child, domTree, methodDecl))

      // pop stacks of all lhs vars
      _ <- phis.keySet.toList.traverse_(popVarStack)
      _ <- assigns.traverse_ { case (_, lhsOption, _) =>
        lhsOption.traverse_(popVarStack)
      }
    } yield ()
  }

  def buildSSAStateInfo(methodDecl: MethodDecl, cfg: CFG, root: NodeId): StateInfo = {
    val root = List(0)
    val idom = buildIDom(cfg, root)
    val dtree = buildDomTree(idom)
    val dft = buildDFTable(cfg, root, idom, dtree)
    val lvl = lhsVarLocations(cfg)
    search(cfg, root, dtree, methodDecl).run(initState(lvl, phiLocs(dft, lvl))).value._1
  }
}
