package com.github.luzhuomi.obsidian

import cats._
import cats.implicits._
import cats.data.StateT

import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.obsidian.ASTUtils._
import com.github.luzhuomi.obsidian.ASTPath._
// import com.github.luzhuomi.scalangj.Syntax
// import _root_.cats.syntax.contravariant

/*
 Control Flow Graph construction
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
    * @param id node ID
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
    * @param lVars variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
    * @param rVars variables on the rhs of assignments
    * @param preds predecessor ids
    * @param succs successor ids
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
    id:ASTPath,
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
    * @param lVars variables on the lhs of assignments. Though they can be constructed from stmts, we cache them here for convienence.
    * @param rVars variables on the rhs of assignments
    * @param preds predecessor ids
    * @param succs successor ids
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
    * @param tryNode the location of the try node
    * @param catchNodes the locations of the catch blocks
    * @param finallyNode the locations of the finally blocks
    * @param preds predecessor ids
    * @param succs successor ids
    */

  case class TryCatchFinallyNode(
      id: ASTPath,
      tryNode: NodeId,
      catchNodes: List[NodeId],
      finallyNode: Option[NodeId],
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

  // update functions for nodes

  def setSuccs(n:Node, s:List[NodeId]):Node = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, s)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, s)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  SwitchNode(id, caseNodes, lVars, rVars, preds, s) 
    case ReturnNode(id, lVars, rVars, preds) => n
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rVars, preds, s)
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, s)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lVars, rVars, preds, s)
  }

  def setPreds(n:Node, p:List[NodeId]):Node = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => AssignmentsNode(id, stmts,  localDecls, lVars, rVars, p, succs)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lVars, rVars, p, succs)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  SwitchNode(id, caseNodes, lVars, rVars, p, succs)  
    case ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lVars, rVars, p)
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rVars, p, succs)
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, p, succs)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lVars, rVars, p, succs)
  }

  def setLVars(n:Node, lv:List[Ident]):Node = n match {
    case AssignmentsNode(id, stmts, localDecls,  lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lv, rVars, preds, succs)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lv, rVars, preds, succs)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  SwitchNode(id, caseNodes, lv, rVars, preds, succs) 
    case ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lv, rVars, preds)
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lv, rVars, preds, succs)
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lv, rVars, preds, succs)
  }

  def setRVars(n:Node, rv:List[Ident]):Node = n match {
    case AssignmentsNode(id, stmts, localDecls,  lVars, rVars, preds, succs) => AssignmentsNode(id, stmts, localDecls, lVars, rv, preds, succs)
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => IfThenElseNode(id, thenNode, elseNode, lVars, rv, preds, succs)
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  SwitchNode(id, caseNodes, lVars, rv, preds, succs) 
    case ReturnNode(id, lVars, rVars, preds) => ReturnNode(id, lVars, rv, preds)
    case ThrowNode(id, lVars, rVars, preds, succs) => ThrowNode(id, lVars, rv, preds, succs)
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs)
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => WhileNode(id, bodyNode, lVars, rv, preds, succs)
  }

  def getSuccs(n:Node):List[NodeId] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => succs
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => succs
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  succs
    case ReturnNode(id, lVars, rVars, preds) => Nil
    case ThrowNode(id, lVars, rVars, preds, succs) => succs
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => succs
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => succs
  }

  def getPreds(n:Node):List[NodeId] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => preds
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => preds
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  preds
    case ReturnNode(id, lVars, rVars, preds) => preds
    case ThrowNode(id, lVars, rVars, preds, succs) => preds
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => preds
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => preds
  }

  def getLVars(n:Node):List[Ident] = n match {
    case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs) => lVars
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => lVars
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  lVars
    case ReturnNode(id, lVars, rVars, preds) => lVars
    case ThrowNode(id, lVars, rVars, preds, succs) => lVars
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => Nil
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => lVars
  }

  def getRVars(n:Node):List[Ident] = n match {
    case AssignmentsNode(id, stmts, localDecls,  lVars, rVars, preds, succs) => rVars
    case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs) => rVars
    case SwitchNode(id, caseNodes, lVars, rVars, preds, succs) =>  rVars 
    case ReturnNode(id, lVars, rVars, preds) => rVars
    case ThrowNode(id, lVars, rVars, preds, succs) => rVars
    case TryCatchFinallyNode(id, tryNode, catchNodes, finallyNode, preds, succs) => Nil
    case WhileNode(id, bodyNode, lVars, rVars, preds, succs) => rVars
  }

  def appSucc(n:Node, succ:NodeId) = setSuccs(n, (getSuccs(n)++List(succ)).toSet.toList)
  def appPred(n:Node, pred:NodeId) = setPreds(n, (getPreds(n)++List(pred)).toSet.toList)
  def appPreds(n:Node, preds:List[NodeId]) = setPreds(n, (getPreds(n) ++ preds).toSet.toList)
  def appLVars(n:Node, lvs:List[Ident]) = setLVars(n, getLVars(n)++lvs)
  def appRVars(n:Node, rvs:List[Ident]) = setRVars(n, getRVars(n)++rvs)

  def appStmt(n:Node, stmt:ASTPath) = n match {
    case AssignmentsNode(id, stmts, localDecls,  lVars, rVars, preds, succs) => {
      val stmtSet = stmts.toSet
      if (stmtSet.contains(stmt)) {
        n
      } else {
        AssignmentsNode(id, stmts++List(stmt), localDecls,  lVars, rVars, preds, succs)
      }
    } 
    case _ => n
  }

  def appLocalDecls(n:Node, lds:List[Ident]) = n match {
    case AssignmentsNode(id, stmts, localDecls,  lVars, rVars, preds, succs) => { 
      AssignmentsNode(id, stmts, (localDecls++lds).toSet.toList,  lVars, rVars, preds, succs)
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
      contNodes: List[
        NodeId
      ], // it seems some of these are needed to support duff's device, which is only doable in C.
      breakNodes: List[NodeId],
      caseNodes: List[CaseExp],
      formalArgs: List[Ident],
      fallThroughCases: List[Exp],
      throwNodes: List[NodeId],
      catchNodes: List[NodeId],
      labelMap: Map[Ident, Ident] // mapping source code label to CFG labels
  )

  sealed trait CaseExp {
    def getWrapperId(): NodeId
  }
  case class DefaultCase(wrapperId: NodeId, rhs: NodeId) extends CaseExp {
    override def getWrapperId = wrapperId
  }
  case class ExpCase(
      condExp: Exp,
      fallThrough: List[Exp],
      wrapperId: NodeId,
      rhs: NodeId
  ) extends CaseExp {
    override def getWrapperId = wrapperId
  }

  val labPref = "myLabel"
  val initStateInfo = StateInfo(
    rootPath,
    Map[NodeId, Node](),
    List(),
    false,
    List(),
    List(),
    List(),
    List(),
    List(),
    List(),
    List(),
    Map()
  )

  sealed trait CFGResult[+A]
  case class CFGError(msg: String) extends CFGResult[Nothing]
  case class CFGOk[A](result: A) extends CFGResult[A]

  implicit def cfgResultFunctor: Functor[CFGResult] =
    new Functor[CFGResult] {
      override def map[A, B](fa: CFGResult[A])(f: A => B): CFGResult[B] =
        fa match {
          case CFGError(s) => CFGError(s)
          case CFGOk(a)    => CFGOk(f(a))
        }
    }


  implicit def cfgResultApplicative: ApplicativeError[CFGResult, String] =
    new ApplicativeError[CFGResult, String] {
      override def ap[A, B](
          ff: CFGResult[A => B]
      )(fa: CFGResult[A]): CFGResult[B] =
        ff match {
          case CFGOk(f) =>
            fa match {
              case CFGOk(a)    => CFGOk(f(a))
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
          case CFGOk(a)    => CFGOk(a)
        }
    }

  implicit def cfgResultMonadError(implicit
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
          case CFGOk(a)    => f(a)
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
    def buildCFG(a: A, p:ASTPath )(implicit
        m: MonadError[SIState, String]
    ): State[StateInfo, Unit]
  }

  object cfgOps {
    def buildCFG[A](
        a: A, p:ASTPath 
    )(implicit aCFGCl: CFGClass[A]): State[StateInfo, Unit] = {
      aCFGCl.buildCFG(a,p)
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

  def idFromVarDeclId(vdid: VarDeclId): Ident =
    vdid match {
      case VarId(id)         => id
      case VarDeclArray(vid) => idFromVarDeclId(vid)
    }

  implicit def methodCFGInstance: CFGClass[MethodDecl] =
    new CFGClass[MethodDecl] {
      override def buildCFG(
          a: MethodDecl, p:ASTPath
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
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
  implicit def bodyCFGInstance: CFGClass[MethodBody] =
    new CFGClass[MethodBody] {
      override def buildCFG(
          a: MethodBody, p:ASTPath
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case MethodBody(None)      => m.pure(())
          case MethodBody(Some(blk)) => cfgOps.buildCFG(blk, p)
        }
    }

  implicit def blockCFGInstance: CFGClass[Block] =
    new CFGClass[Block] {
      override def buildCFG(
          a: Block, p:ASTPath
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case Block(Nil) => {
            val lhs = Nil
            val rhs = Nil
            val stmts = Nil
            val localDecls = Nil
            val currNodeId = p
            for {
              st         <- get;
              cfg0       <- m.pure(st.cfg);
              preds0     <- m.pure(st.currPreds);
              cfgNode    <- m.pure(AssignmentsNode(currNodeId, stmts, localDecls, lhs, rhs, preds0, Nil));
              cfg1p      <- m.pure(preds0.foldLeft(cfg0)((g, pred) => {
                val n: Node = g(pred)
                g + (pred -> appSucc(n,currNodeId))
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
              _ <- stmts.zip(0 to stmts.size).traverse_(stmt_idx => stmt_idx match {
                case (stmt,idx) => {
                  cfgOps.buildCFG(stmt, childOf(p, idx))
                }
              })
              // TOOD: check whether the following hack is still necessary for Java
              _ <-
                if (stmts.isEmpty) { m.pure(()) }
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
                      cfgOps.buildCFG(empty, childOf(p,stmts.size))
                    }
                    case _ => m.pure(())
                  }
                }
            } yield ()
        }
    }

  implicit def blockStmtCFGInstance: CFGClass[BlockStmt] =
    new CFGClass[BlockStmt] {
      override def buildCFG(
          a: BlockStmt, p:ASTPath
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case LocalClass(_) => m.raiseError("Local Class is not supported.")
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls match {
              case Nil               => m.pure(())
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
                        rvars,
                        lvars,
                        preds0,
                        Nil
                      )
                      val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                        val n: Node = g(pred)
                        val n1 = appSucc(n,currNodeId)
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

  implicit def stmtCFGInstance: CFGClass[Stmt] =
    new CFGClass[Stmt] {
      override def buildCFG(
          a: Stmt, p: ASTPath
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case StmtBlock(blk)                         => cfgOps.buildCFG(blk,p)
          case IfThen(exp, stmt)                      => buildCFG(IfThenElse(exp, stmt, Empty), p)
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
                // val max = st.currId
                val ifElseNodeId = p
                val lhs = HasVarcfgOps.getLVarsFrom(exp)
                val rhs = HasVarcfgOps.getVarsFrom(exp)
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val ifElseNode = IfThenElseNode(ifElseNodeId, thenOf(p), elseOf(p), lhs, rhs, preds0, Nil)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  val n1 = appSucc(n,ifElseNodeId)
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
            CFG0, path++[0], {path}, false, {}, {}, {} |- case1 => CFG1, preds1, continuable1, breakNodes1, contNodes1, caseNodes1
            CFG1, path++[1], {path} u preds1, false, breakNodes1, contNodes1, caseNodes1 |- case2 => CFG2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2
            CFG2, path++[2], {path} u preds2, false, breakNodes2, contNodes2, caseNodes2 |- case3 => CFG3, preds3, continuable3, breakNodes3, contNodes3, caseNodes3
            ...
            CFGn-1, path++[n-1], {path} u predsn-1, false, breakNodesn-1, contNodem-1, caseNodesn-1 |- casen => CFGn, predsn, continuable3, breakNodesn, contNodesn, caseNodesn

            ---------------------------------------------------------------------------------------------------------------------------------------------------------
            CFG, path, preds, continuable, breakNodes, contNodes, caseNodes |- swtch exp { case1, ..., casen } => CFGn, predsn u breaknodesn, false,  breaknodes, contNodesn, caseNodesn 
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
                val caseNodes0 = st.caseNodes
                val childNodeIds = (0 to blocks.size).map(idx=>childOf(p,idx)).toList
                val switchNode = SwitchNode(currNodeId, childNodeIds, lhs, rhs, preds0, childNodeIds)
                val cfg1p = preds0.foldLeft(cfg0)((g,pred) => {
                  val n = g(pred)
                  g + (pred -> appSucc(n,currNodeId))
                })
                val cfg1 = cfg1p + (currNodeId -> switchNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      currPreds = List(currNodeId),
                      fallThroughCases = Nil,
                      continuable = false,
                      breakNodes = Nil,
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
                          currPreds = preds1 ++ breakNodes1,
                          breakNodes = breakNodes0,
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

          CFG1 = CFG update { pred: {succ = p} | pred <- preds } union { path: whilenode(childOF(path), lhs, rhs, preds, childOf(path))}
          CFG1, childOF(path), {path}, _, {}, {} |- stmt |- CFG2,  preds2, continuable2, contNodes2, breakNodes2
          CFG3 = CFG3 update { nodeid: { succ = {path} | nodeid <- contNodes2++preds2 } } update { path : { preds = preds ++ contNodes2 ++ preds2 } }
          -------------------------------------------------------------------------------------------------------------------------------
          CFG, path, preds, _, contNodes, breakNodes |- while (exp) { stmt } =>  CFG3, {path} ++ breakNodes2, false, breakNodes ,contNOdes
             */
            for {
              st <- get
              _ <- {
                
                val currNodeId = p
                val lhs = HasVarcfgOps.getVarsFrom(exp)
                val rhs = HasVarcfgOps.getLVarsFrom(exp)
                
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val contNodes0 = st.contNodes
                val breakNodes0 = st.breakNodes
                val childNodeId = childOf(currNodeId,0)
                val cfgNode =
                  WhileNode(currNodeId, childNodeId, lhs, rhs, preds0, List(childNodeId))
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
                      breakNodes = Nil
                    )
                  )
                  _ <- buildCFG(stmt, childOf(p,0))
                  st1 <- get
                  _ <- {
                    val preds2 = st1.currPreds
                    val contNodes2 = st1.contNodes
                    val cfg2 = st1.cfg
                    val cfg2p = (preds2++contNodes2).foldLeft(cfg2)((g, pred) => {
                      val n = g(pred)
                      g + (pred -> appSucc(n, currNodeId))
                    })
                    val breakNodes2 = st1.breakNodes
                    val currNode = cfg2p(currNodeId)
                    val cfg2pp = cfg2p + (currNodeId -> 
                      appPreds(cfg2p(currNodeId), preds2 ++ contNodes2))
                    val cfg3 = contNodes2.foldLeft(cfg2pp)(
                      (g, l) => { // update the break and cont immediately
                        val n = g(l)
                        g + (l -> appSucc(n,currNodeId))
                      }
                    )
                    val cfg3p = cfg3
                    for {
                      _ <- put(
                        st1.copy(
                          cfg = cfg3p,
                          currPreds = List(currNodeId) ++ breakNodes2,
                          continuable = false,
                          contNodes = contNodes0,
                          breakNodes = breakNodes0
                        )
                      )
                    } yield ()
                  }
                } yield ()
              }
            } yield ()
          /*
      CFG, max, preds, continuable |- stmt => CFG1, max1, preds1, false
      CFG1, max1, preds1, false, contNodes, breakNodes |- while (exp) { stmt } => CFG2, max2, preds2, continuable, contNodes', breakNodes'
      --------------------------------------------------------------------------------------
      CFG, max, preds, continuable, contNodes, breakNodes |- do  { stmt } while (exp) => CFG2, max2, {max}, false, contNodes', breakNodes'
           */
          // Desguaring should be done separately in a desugarer
          /*
          case Do(stmt, exp) =>
            for {
              _ <- buildCFG(stmt)
              _ <- buildCFG(While(exp, stmt))
            } yield ()
          */
          /*
      CFG, max, preds, continuable |- init => CFG1, max1, preds1, continuable1
      CFG1, max1, preds1, continuable1 |- while (exp2) { stmt; exp3 } => CFG2, max2, preds2, continuabl2
      ---------------------------------------------------------------------------------------
      CFG, max, preds, true |- for (init; exp2; exp3) { stmt }  => CFG2, max', preds', continuable
           */
          case BasicFor(init, exp2, exp3, stmt) =>
            for {
              _ <- init match {
                case None => m.pure(())
                case Some(flv @ ForLocalVars(modifiers, ty, var_decls)) =>
                  cfgOps.buildCFG(flv)
                // var_decls.traverse_(vd => cfgOps.buildCFG(vd))
                case Some(ForInitExps(es)) =>
                  es.traverse_(e => buildCFG(ExpStmt(e)))
              }
              _ <- {
                val exp2p = exp2 match {
                  case None    => Lit(IntLit(1))
                  case Some(e) => e
                }
                val stmtp = exp3 match {
                  case None     => stmt
                  case Some(es) => appStmt(stmt, es.map(ExpStmt(_)))
                }
                for {
                  _ <- buildCFG(While(exp2p, stmtp))
                } yield ()
              }
            } yield ()
          /*
      Enhanced For with primitive type arrays

      CFG, max, preds, continuable |- for (int i = 0; i < exp.length; i++) { t x = exp3[i]; stmt } =>
      CFG', max', preds', continuable
      ---------------------------------------------------------------------------------------
      CFG, max, preds, true |- for (t x: exp) { stmt }  => CFG', max', preds', continuable
           */

          case EnhancedFor(modifiers, ty @ PrimType_(_), id, exp, stmt) =>
            for {
              st <- get
              _ <- {
                val max = st.currId
                val i = Ident(s"idx_loop${labPref}${max}")
                val var_decls = List(
                  VarDecl(VarId(i), Some(InitExp(Lit(IntLit(0)))))
                )
                val init = Some(ForLocalVars(Nil, PrimType_(IntT), var_decls))
                val exp2 = Some(
                  BinOp(
                    ExpName(Name(i :: Nil)),
                    LThan,
                    dotField(exp, Ident("length"))
                  )ppr
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
                for {
                  _ <- buildCFG(
                    BasicFor(
                      init,
                      exp2,
                      exp3,
                      prpDecl(modifiers, ty, var_decls2, stmt)
                    )
                  )
                } yield ()
              }
            } yield ()

          /*
      Enhanced For with object type arrays

      CFG, max, preds, continuable |- java.util.Iterator<T> l = exp.iterator() => CFG1, max1, preds1, continuable1
      CFG1, max1, preds1, continuable1 |- while (l.hasNext()) { T x = l.next(); stmt } => CFG', max', preds', continuable
      ---------------------------------------------------------------------------------------
      CFG, max, preds, true |- for (T x: exp) { stmt }  => CFG', max', preds', continuable
           */
          case EnhancedFor(modifiers, ty @ RefType_(t), id, exp, stmt) =>
            for {
              st <- get
              _ <- {
                val max = st.currId
                val l = Ident(s"itr_loop${labPref}${max}")
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
                val while_stmt = While(
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
                val blk = Block(List(iteratorDecl, BlockStmt_(while_stmt)))
                for {
                  _ <- cfgOps.buildCFG(blk)
                } yield ()
              }
            } yield ()

          case Empty => m.pure(())

          /*
      CFG1 = CFG update { pred : {stmts = stmts ++ [x = exp], lVars = lVars ++ [x] } }
      x \not in {v | v \in lVars pred, pred \in preds }
      --------------------------------------------------------
      CFG, max, preds, true |-  x = exp => CFG1, max, [] , true

      max1 = max + 1
      CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : {x = exp} }
      --------------------------------------------------------
      CFG, max, preds, false |- x = exp => CFG1, max1, [], false
           */
          case s @ ExpStmt(Assign(lhs, EqualA, rhs)) => {
            val xs = HasVarcfgOps.getLVarsFrom(lhs).toSet
            val ys = HasVarcfgOps.getVarsFrom(rhs)
            for {
              st <- get
              _ <- {
                val preds0 = st.currPreds
                val max = st.currId
                val cfg0 = st.cfg
                val is_reassigned = preds0.foldLeft(false)((b, pred) => {
                  val n = cfg0(pred)
                  n.lVars.exists(v => xs(v))
                })
                if ((st.continuable) && (!is_reassigned)) {
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(
                      stmts = n.stmts ++ List(BlockStmt_(s)),
                      lVars = n.lVars ++ xs.toList,
                      rVars = n.rVars ++ ys
                    ))
                  })
                  for {
                    _ <- put(st.copy(cfg = cfg1, continuable = true))
                  } yield ()
                } else {
                  val currNodeId = internalIdent(s"${labPref}${max}")
                  val max1 = max + 1
                  val cfgNode = Node(
                    List(BlockStmt_(s)),
                    xs.toList,
                    ys,
                    Nil,
                    preds0,
                    Nil,
                    AssignmentNode
                  )
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
                        currId = max1,
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
          case ExpStmt(Assign(lhs, aop, rhs)) => {
            val rlhs = lhsToRhs(lhs)
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
            val stmt = ExpStmt(Assign(lhs, EqualA, BinOp(rlhs, op, rhs)))
            for {
              _ <- buildCFG(stmt)
            } yield ()
          }

          case ExpStmt(PostIncrement(exp)) =>
            ConvertableToLhscfgOps.getLhsFrom(exp) match {
              case Nil =>
                m.raiseError(
                  s"BuildCFG Failed: Fail to extract lhs from exp ${exp}"
                )
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, AddA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>
                m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")
            }
          case ExpStmt(PostDecrement(exp)) =>
            ConvertableToLhscfgOps.getLhsFrom(exp) match {
              case Nil =>
                m.raiseError(
                  s"BuildCFG Failed: Fail to extract lhs from exp ${exp}"
                )
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, SubA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>
                m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")
            }
          case ExpStmt(PreIncrement(exp)) =>
            ConvertableToLhscfgOps.getLhsFrom(exp) match {
              case Nil =>
                m.raiseError(
                  s"BuildCFG Failed: Fail to extract lhs from exp ${exp}"
                )
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, AddA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>
                m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")
            }
          case ExpStmt(PreDecrement(exp)) =>
            ConvertableToLhscfgOps.getLhsFrom(exp) match {
              case Nil =>
                m.raiseError(
                  s"BuildCFG Failed: Fail to extract lhs from exp ${exp}"
                )
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, SubA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>
                m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")
            }
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
                    g + (pred -> n.copy(
                      stmts = n.stmts ++ List(s),
                      lVars = n.lVars ++ lhs,
                      rVars = n.rVars ++ rhs
                    ))
                  })
                  put(st.copy(cfg = cfg1, continuable = true))
                } else {
                  val max = st.currId
                  val currNodeId = internalIdent(s"${labPref}${max}")
                  val max1 = max + 1
                  val cfgNode =
                    Node(List(s), Nil, lhs, rhs, preds0, Nil, AssignmentNode)
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(succs = n.succs ++ List(currNodeId)))
                  })
                  val cfg1 = cfg1p + (currNodeId -> cfgNode)
                  put(
                    st.copy(
                      cfg = cfg1,
                      currId = max1,
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
          case s @ Assert(exp, msg) =>
            for {
              st <- get
              _ <- {
                val es = List(exp) ++ msg.toList
                val lhs = es.flatMap(HasVarcfgOps.getLVarsFrom(_))
                val rhs = es.flatMap(HasVarcfgOps.getVarsFrom(_))
                val max = st.currId
                val currNodeId = internalIdent(s"${labPref}${max}")
                val max1 = max + 1
                val preds0 = st.currPreds
                val cfg0 = st.cfg
                val cfgNode = Node(
                  List(BlockStmt_(s)),
                  lhs,
                  rhs,
                  Nil,
                  preds0,
                  Nil,
                  AssertNode
                )
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
                      currId = max1,
                      currPreds = List(currNodeId),
                      continuable = false
                    )
                  )
                } yield ()
              }
            } yield ()

          case Break(Some(id)) =>
            m.raiseError(
              "break with label is not supported."
            ) // this seems to require callCC?
          case Break(None) =>
            for {
              st <- get
              _ <-
                if (st.continuable) {
                  /*
                CFG1 = CFG update { pred: {stmts = stmts ++ [break] } | pred <- preds }
                -----------------------------------------------------------------------------------------------------------------------------
                CFG, max, preds, true, contNodes, breakNodes |- break =>  CFG1, max, {}, false, contNodes, breakNode union preds
                   */
                  val cfg0 = st.cfg
                  val preds0 = st.currPreds
                  val s = BlockStmt_(Break(None))
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(stmts = n.stmts ++ List(s)))
                  })
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        currPreds = Nil,
                        continuable = false,
                        breakNodes = st.breakNodes ++ preds0
                      )
                    )
                  } yield ()
                } else {
                  val max = st.currId
                  val currNodeId = internalIdent(s"${labPref}${max}")
                  val max1 = max + 1
                  val cfg0 = st.cfg
                  val preds0 = st.currPreds
                  val s = BlockStmt_(Break(None))
                  val cfgNode =
                    Node(List(s), Nil, Nil, Nil, preds0, Nil, BreakNode)
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
                        currId = max1,
                        currPreds = Nil,
                        continuable = false,
                        breakNodes = st.breakNodes ++ List(currNodeId)
                      )
                    )
                  } yield ()
                }
            } yield ()

          case Continue(Some(id)) =>
            for {
              st <- get
              _ <- st.labelMap.get(id) match {
                case None =>
                  m.raiseError(
                    s"BuildCFG failed, continue with a label ${id} that is not in the label map."
                  )
                case Some(lp) if (st.continuable) => {
                  /*
            l' = labelMap(l)
            CFG1 = CFG update { pred: {stmts = stmts ++ [continue l], succs = succs ++ [l'] } | pred <- preds }
            -----------------------------------------------------------------------------------------------------------------------------
            CFG, max, preds, true, contNode, breaknodes, labllMap |- continue l => CFG1, max, {}, false, contNode union preds, breakNodes, labelMap
                   */
                  val cfg0 = st.cfg
                  val s = BlockStmt_(Continue(Some(id)))
                  val preds0 = st.currPreds
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(
                      stmts = n.stmts ++ List(s),
                      succs = n.succs ++ List(lp)
                    ))
                  })
                  val cfg1 = cfg1p.get(lp) match {
                    case None => cfg1p
                    case Some(n) =>
                      cfg1p + (lp -> n.copy(preds =
                        (n.preds ++ preds0).toSet.toList
                      ))
                  }
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        currPreds = Nil,
                        continuable = false,
                        contNodes = st.contNodes ++ preds0
                      )
                    )
                  } yield ()
                }
                case Some(lp) => {
                  /*
            l' = labelMap(l)
            max1 = max + 1
            CFG1 = CFG update { pred: {uccs = succs ++ [max] } | pred <- preds } union { max : { stmts = [continue l], succs = [l'] }}
            -----------------------------------------------------------------------------------------------------------------------------
            CFG, max, preds, false, contNode, breaknodes, labllMap |- continue l => CFG1, max1, {}, false, contNode uinion {max}, breakNodes, labelMap
                   */
                  val cfg0 = st.cfg
                  val s = BlockStmt_(Continue(Some(id)))
                  val max = st.currId
                  val max1 = max + 1
                  val preds0 = st.currPreds
                  val currNodeId = internalIdent(s"${labPref}${max}")
                  val cfgNode =
                    Node(List(s), Nil, Nil, Nil, preds0, List(lp), ContNode)
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(succs = n.succs ++ List(currNodeId)))
                  })
                  val cfg1 = cfg1p + (currNodeId -> cfgNode)
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        currId = max1,
                        continuable = false,
                        contNodes = st.contNodes ++ List(currNodeId)
                      )
                    )
                  } yield ()
                }
              }
            } yield ()

          case Continue(None) =>
            for {
              st <- get
              _ <-
                if (st.continuable) {
                  /*
                CFG1 = CFG update { pred: {stmts = stmts ++ [continue] } | pred <- preds }
                -----------------------------------------------------------------------------------------------------------------------------
                CFG, max, preds, true, contNode, breakNodes |- continue =>  CFG1, max, {}, false, contNode union preds, breakNodes
                   */
                  val cfg0 = st.cfg
                  val preds0 = st.currPreds
                  val s = BlockStmt_(Continue(None))
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(stmts = n.stmts ++ List(s)))
                  })
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        currPreds = Nil,
                        continuable = false,
                        contNodes = st.contNodes ++ preds0
                      )
                    )
                  } yield ()
                } else {
                  /*
                max1 = max + 1
                CFG1 = CFG update { pred: {succ = max} | pred <- preds } union { max : { stmts = [ continue ], preds = preds, succ = [] } }
                -----------------------------------------------------------------------------------------------------------------------------
                CFG, max, preds, false, contNode, breakNodes |- continue =>  CFG1, max1, {max}, false, contNode union { max }, breakNodes

                check the preds {max} should be {}
                   */
                  val max = st.currId
                  val currNodeId = internalIdent(s"${labPref}${max}")
                  val max1 = max + 1
                  val cfg0 = st.cfg
                  val preds0 = st.currPreds
                  val s = BlockStmt_(Continue(None))
                  val cfgNode =
                    Node(List(s), Nil, Nil, Nil, preds0, Nil, ContNode)
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
                        currId = max1,
                        currPreds = Nil,
                        continuable = false,
                        contNodes = st.contNodes ++ List(currNodeId)
                      )
                    )
                  } yield ()
                }
            } yield ()

          /*
      CFG1 = CFG update { pred : {stmts = stmts ++ [ return exp ] } }
      --------------------------------------------------------
      CFG, max, preds, true |- return exp  => CFG1, max, [] , false

      max1 = max + 1
      CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : {stmts = return exp } }
      --------------------------------------------------------
      CFG, max, preds, false |- return exp => CFG1, max, [], false
           */
          case Return(o_exp) =>
            for {
              st <- get
              _ <-
                if (st.continuable) {
                  val cfg0 = st.cfg
                  val lhs = o_exp.toList.flatMap(HasVarcfgOps.getLVarsFrom(_))
                  val rhs = o_exp.toList.flatMap(HasVarcfgOps.getVarsFrom(_))
                  val preds0 = st.currPreds
                  val s = BlockStmt_(Return(o_exp))
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(
                      stmts = n.stmts ++ List(s),
                      lVars = n.lVars ++ lhs,
                      rVars = n.rVars ++ rhs
                    ))
                  })
                  for {
                    _ <- put(
                      st.copy(cfg = cfg1, currPreds = Nil, continuable = false)
                    )
                  } yield ()
                } else {
                  val max = st.currId
                  val currNodeId = internalIdent(s"${labPref}${max}")
                  val lhs = o_exp.toList.flatMap(HasVarcfgOps.getLVarsFrom(_))
                  val rhs = o_exp.toList.flatMap(HasVarcfgOps.getVarsFrom(_))
                  val max1 = max + 1
                  val cfg0 = st.cfg
                  val preds0 = st.currPreds
                  val s = BlockStmt_(Return(o_exp))
                  val cfgNode =
                    Node(List(s), lhs, rhs, Nil, preds0, Nil, ReturnNode)
                  val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(succs =
                      (n.succs ++ List(currNodeId).toSet.toList)
                    ))
                  })
                  val cfg1 = cfg1p + (currNodeId -> cfgNode)
                  for {
                    _ <- put(
                      st.copy(
                        cfg = cfg1,
                        currId = max1,
                        currPreds = Nil,
                        continuable = false
                      )
                    )
                  } yield ()
                }
            } yield ()
          case Synchronized(exp, blk) =>
            m.raiseError("Synronized statement is not suppored.")
          case Throw(exp) =>
            for {
              /*
      max1 = max + 1
      CFG1 = CFG update { pred: {succ = max}} pred <- preds } union { max : {throw exp} }
      -----------------------------------------------------------------------------------------------
      CFG, max, preds, continuable breakNodes, contNodes, caseNodes, throwNodes |- throw exp => CFG1, max1, [], false

               */
              st <- get
              _ <- {
                val max = st.currId
                val max1 = max + 1
                val cfg0 = st.cfg
                val currNodeId = internalIdent(s"${labPref}${max}")
                val preds0 = st.currPreds
                val s = BlockStmt_(Throw(exp))
                val lhs = HasVarcfgOps.getLVarsFrom(exp)
                val rhs = HasVarcfgOps.getVarsFrom(exp)
                val cfgNode =
                  Node(List(s), lhs, rhs, Nil, preds0, Nil, ThrowNode)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  g + (pred -> n.copy(succs =
                    (n.succs ++ List(currNodeId).toSet.toList)
                  ))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                for {
                  _ <- put(
                    st.copy(
                      cfg = cfg1,
                      continuable = false,
                      currId = max1,
                      currPreds = Nil,
                      throwNodes = st.throwNodes ++ List(currNodeId)
                    )
                  )
                } yield ()
              }
            } yield ()

          case Try(try_blk, catches, finally_blk) =>
            for {
              /*
      max0 = max + 1
      n = { stmts = {try ... catch ... finally}, try_node = max0, catch_nodes = [catchNodes2], finally_node = max2 }
      CFG0 = CFG + { max -> n }
      CFG0, max0, {max}, false, breakNodes, contNodes, caseNodes, {}, _ |-
        blk1 => CFG1, max1, preds1, continuable1, breakNodes1, contNodes1, caseNodes1, throwNodes1, catchNodes,
      CFG1, max1, throwNodes1, false, breakNodes1, contNodes1, caseNodes1, throwNodes, {},  |-
        blk2 => CFG2, max2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2, throwNodes2, catchNodes2
      CFG2, max2, preds1++preds2, false, breakNodes2, contNodes2, caseNodes2, throwNodes2 |-
        blk3 => CFG3, max3, preds3, continuable3, breakNodes3, contNode3, caseNodes3, throwNodes3
      ----------------------------------------------------------------------------------------------------------------
      CFG, max, preds, continuable, breakNodes, contNodes, caseNodes, throwNodes, catchNodes |- try {blk1} catch (x) {blk2} finally {blk3}
       => CFG3, max3, preds3, false, breakNodes3, contNode3, caseNodes3, throwNodes ++ throwNodes3, catchNodes
       // local catch Nodes should not escape

               */
              st <- get
              _ <- {
                val max0 = st.currId
                val max1 = max0 + 1
                val preds0 = st.currPreds
                val s = BlockStmt_(Try(try_blk, catches, finally_blk))
                val l0 = internalIdent(s"${labPref}${max0}")
                val l1 = internalIdent(s"${labPref}${max1}")
                val n = Node(
                  List(s),
                  Nil,
                  Nil,
                  Nil,
                  preds0,
                  Nil,
                  TryCatchNode(l1, Nil, None)
                ) // will be updated later

                val cfg0 = st.cfg + (l0 -> n)
                val throwNodes0 = st.throwNodes
                val catchNodes0 = st.catchNodes
                for { // building for try block
                  _ <- put(
                    st.copy(
                      cfg = cfg0,
                      throwNodes = Nil,
                      currId = max1,
                      currPreds = List(l0),
                      continuable = false
                    )
                  )
                  _ <- cfgOps.buildCFG(try_blk)
                  st1 <- get
                  _ <- { // building for catch blocks
                    val preds1 = st1.currPreds
                    val throwNodes1 = st1.throwNodes
                    for {
                      _ <- put(
                        st1.copy(throwNodes = throwNodes0, catchNodes = Nil)
                      )
                      _ <- catches.traverse_(c =>
                        for {
                          st1p <- get
                          _ <- put(
                            st1p.copy(
                              continuable = false,
                              currPreds = throwNodes1
                            )
                          )
                          _ <- cfgOps.buildCFG(c)
                        } yield ()
                      )
                      st2 <- get
                      _ <- { // building for finally blocks if any
                        val catchNodes2 = st2.catchNodes
                        val preds2 = st2.currPreds
                        finally_blk match {
                          case Some(blk) => {
                            val max2 = st2.currId
                            val l3 = internalIdent(s"${labPref}${max2}")
                            for {
                              _ <- put(
                                st2.copy(
                                  currPreds = preds1 ++ preds2,
                                  continuable = false
                                )
                              )
                              _ <- cfgOps.buildCFG(blk)
                              st3 <- get
                              _ <- { // update the node for the try / catch stmt, to
                                val cfg3 = st3.cfg
                                val n3 = cfg3(l0)
                                val cfg4 = cfg3 + (l0 -> n3.copy(nodeType =
                                  TryCatchNode(l1, catchNodes2, Some(l3))
                                ))
                                for {
                                  _ <- put(
                                    st3.copy(
                                      cfg = cfg4,
                                      continuable = false,
                                      catchNodes = catchNodes0
                                    )
                                  )
                                } yield ()
                              }
                            } yield ()
                          }
                          case None => {
                            val cfg3 = st2.cfg
                            val n3 = cfg3(l0)
                            val cfg4 = cfg3 + (l0 -> n3.copy(nodeType =
                              TryCatchNode(l1, catchNodes2, None)
                            ))
                            for {
                              _ <- put(
                                st2.copy(
                                  cfg = cfg4,
                                  currPreds = preds1 ++ preds2,
                                  continuable = false,
                                  catchNodes = catchNodes0
                                )
                              )
                            } yield ()
                          }
                        }
                      }
                    } yield ()
                  }
                } yield ()
              }
            } yield ()

          case Labeled(id, stmt) =>
            for {
              /*
            ASSUMPTION, labeled statements in Java must be introduced first then used by continue or break.

            lblMap1 = lblMap + (l -> max)
            CFG1, max, preds, false, lblMap1 |- stmt => CFG2, max2, preds, continuable, lblMap2
            ------------------------------------------------------------------------------
            CFG, max, preds, _, lblMap |- l: stmt => CFG2, max2, preds, continuable, lblMap2
               */
              st <- get
              _ <- put(
                st.copy(
                  labelMap = st.labelMap + (id -> internalIdent(
                    s"${labPref}${st.currId}"
                  )),
                  continuable = false
                )
              )
              _ <- buildCFG(stmt)
            } yield ()
        }
    }

  /**
    * Helper functoin for buildCFG for the case of switch
    */
  def caseExpsToIfNodes(exp: Exp, ces: List[CaseExp])(implicit
      m: MonadError[SIState, String]
  ): State[StateInfo, List[NodeId]] =
    ces match {
      case Nil => m.pure(Nil)
      case (DefaultCase(wrapNodeId, rhsNodeId) :: _) =>
        for {
          st <- get
          _ <- {
            val preds0 = st.currPreds
            val cfg0 = st.cfg
            val stmts =
              List(
                BlockStmt_(Continue(Some(rhsNodeId)))
              ) // instead of Goto L, we put continue L
            val cfgNode = Node(
              stmts,
              Nil,
              Nil,
              Nil,
              preds0,
              List(rhsNodeId),
              AssignmentNode
            )
            val rhsNode = cfg0(rhsNodeId)
            val cfg0p = cfg0 + (rhsNodeId -> rhsNode.copy(preds =
              (rhsNode.preds ++ List(wrapNodeId))
            ))
            val cfg0pp = preds0.foldLeft(cfg0p)((g, pred) => {
              val n = g(pred)
              val np = n.copy(succs = (n.succs ++ List(wrapNodeId)))
              g + (pred -> np)
            })
            val cfg1 = cfg0pp + (wrapNodeId -> cfgNode)
            for {
              _ <- put(
                st.copy(
                  cfg = cfg1,
                  currPreds = List(wrapNodeId),
                  continuable = false
                )
              )
            } yield ()
          }
        } yield Nil
      case (ExpCase(e, es, wrapNodeId, rhsNodeId) :: next :: ps) =>
        for {
          st <- get
          ns <- {
            val preds0 = st.currPreds
            val cfg0 = st.cfg
            val nextNodeId = next.getWrapperId
            val lhs = HasVarcfgOps.getLVarsFrom(exp)
            val rhs = HasVarcfgOps.getVarsFrom(exp)
            val lhsp = (e :: es).flatMap(HasVarcfgOps.getLVarsFrom(_))
            val rhsp = (e :: es).flatMap(HasVarcfgOps.getVarsFrom(_))
            val cond = es.foldLeft(eeq(exp, e))((a, e) => eor(eeq(exp, e), a))
            val stmts = List(
              BlockStmt_(
                IfThenElse(
                  cond,
                  Continue(Some(rhsNodeId)),
                  Continue(Some(nextNodeId))
                )
              )
            )
            val cfgNode = Node(
              stmts,
              (lhs ++ lhsp).toSet.toList,
              (rhs ++ rhsp).toSet.toList,
              Nil,
              preds0,
              List(rhsNodeId, nextNodeId),
              AssignmentNode
            )
            val n = cfg0(rhsNodeId)
            val cfg0p =
              cfg0 + (rhsNodeId -> n.copy(preds = n.preds ++ List(wrapNodeId)))
            val cfg0pp = preds0.foldLeft(cfg0p)((g, pred) => {
              val n = g(pred)
              g + (pred -> n.copy(succs =
                (n.succs ++ List(wrapNodeId)).toSet.toList
              ))
            })
            val cfg1 = cfg0pp + (wrapNodeId -> cfgNode)
            for {
              _ <- put(
                st.copy(
                  cfg = cfg1,
                  currPreds = List(wrapNodeId),
                  continuable = false
                )
              )
              ns <- caseExpsToIfNodes(exp, next :: ps)
            } yield ns
          }
        } yield ns
      case (ExpCase(e, es, wrapNodeId, rhsNodeId) :: Nil) =>
        for {
          st <- get
          ns <- {
            val preds0 = st.currPreds
            val cfg0 = st.cfg
            val max = st.currId
            val lhs = HasVarcfgOps.getLVarsFrom(exp)
            val rhs = HasVarcfgOps.getVarsFrom(exp)
            val lhsp = (e :: es).flatMap(HasVarcfgOps.getLVarsFrom(_))
            val rhsp = (e :: es).flatMap(HasVarcfgOps.getVarsFrom(_))
            val cond = es.foldLeft(eeq(exp, e))((a, e) => eor(eeq(exp, e), a))
            val stmts = List(
              BlockStmt_(IfThen(cond, Continue(Some(rhsNodeId))))
            )
            val cfgNode = Node(
              stmts,
              (lhs ++ lhsp).toSet.toList,
              (rhs ++ rhsp).toSet.toList,
              Nil,
              preds0,
              List(rhsNodeId),
              AssignmentNode
            )
            val n = cfg0(rhsNodeId)
            val cfg0p =
              cfg0 + (rhsNodeId -> n.copy(preds = n.preds ++ List(wrapNodeId)))
            val cfg0pp = preds0.foldLeft(cfg0p)((g, pred) => {
              val n = g(pred)
              g + (pred -> n.copy(succs =
                (n.succs ++ List(wrapNodeId)).toSet.toList
              ))
            })
            val cfg1 = cfg0pp + (wrapNodeId -> cfgNode)
            for {
              _ <- put(
                st.copy(
                  cfg = cfg1,
                  currPreds = List(wrapNodeId),
                  continuable = false
                )
              )
            } yield List(wrapNodeId)
          }
        } yield ns
    }

  implicit def catchCFGInstance: CFGClass[Catch] =
    new CFGClass[Catch] {
      override def buildCFG(
          a: Catch
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case Catch(params, blk) =>
            for {
              st <- get
              _ <- {
                val max = st.currId
                val l = internalIdent(s"${labPref}${max}")
                val lhs = HasVarcfgOps.getLVarsFrom((params))
                for {
                  _ <- cfgOps.buildCFG(blk)
                  st2 <- get
                  _ <- {
                    val cfg2 = st2.cfg
                    val n = cfg2(l)
                    val np = n.copy(localDecls = n.localDecls ++ lhs)
                    val cfg3 = cfg2 + (l -> np)
                    for {
                      _ <- put(
                        st2.copy(
                          cfg = cfg3,
                          catchNodes = st.catchNodes ++ List(l)
                        )
                      )
                    } yield ()
                  }
                } yield ()
              }
            } yield ()
        }
    }

  implicit def switchBlockCFGInstance: CFGClass[SwitchBlock] =
    new CFGClass[SwitchBlock] {
      override def buildCFG(
          a: SwitchBlock
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case SwitchBlock(Default, blks_stmts) =>
            for {
              /*
          CFG, max, preds, continuable, breakNodes, contNodes, caseNodes |-
            stmt => CFG2, max2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2
          ------------------------------------------------------------------------------------------------------------------------------------------------------------------
          CFG,max,preds, continuable, breakNodes, contNodes, caseNodes |-
            default: stmt => CFG2, max2, preds2 continuable2, breakNodes, contNodes2, caseNodes2 union (max, default)
               */
              st <- get
              _ <- {
                val max = st.currId
                val wrapNodeId = internalIdent(s"${labPref}${max}")
                val rhsNodeId = internalIdent(s"${labPref}${max + 1}")
                for {
                  _ <- put(st.copy(currId = max + 1, fallThroughCases = Nil))
                  _ <- blks_stmts.traverse_(cfgOps.buildCFG(_))
                  st1 <- get
                  _ <- put(
                    st1.copy(caseNodes =
                      st1.caseNodes ++ List(DefaultCase(wrapNodeId, rhsNodeId))
                    )
                  )
                } yield ()
              }
            } yield ()
          case SwitchBlock(SwitchCase(e), blk_stmts) =>
            for {
              /*
          CFG, max, preds, continuable, breakNodes, contNodes, caseNodes |-
            stmt => CFG2, max2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2
          ------------------------------------------------------------------------------------------------------------------------------------------------------------------
          CFG,max,preds, continuable, breakNodes, contNodes, caseNodes |-
            case e: stmt => CFG2, max2, preds2 continuable2, breakNodes, contNodes2, caseNodes2 union (max, e)
               */
              st <- get
              _ <- {
                val max = st.currId
                val fallThrough = st.fallThroughCases
                val wrapNodeId = internalIdent(s"${labPref}${max}")
                val rhsNodeId = internalIdent(s"${labPref}${max + 1}")
                for {
                  _ <- put(st.copy(currId = max + 1, fallThroughCases = Nil))
                  _ <- blk_stmts.traverse_(cfgOps.buildCFG(_))
                  st1 <- get
                  _ <- put(
                    st1.copy(caseNodes =
                      st1.caseNodes ++ List(
                        ExpCase(e, fallThrough, wrapNodeId, rhsNodeId)
                      )
                    )
                  )
                } yield ()
              }
            } yield ()
        }
    }

  implicit def varDeclCFGInstance: CFGClass[ForLocalVars] =
    new CFGClass[ForLocalVars] {
      override def buildCFG(
          a: ForLocalVars
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {

          case ForLocalVars(modifiers, ty, var_decls) =>
            for {

              /*

          max1 = max + 1
          CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : {ty x = exp[] } }
          --------------------------------------------------------
          CFG, max, preds, false |- ty x = exp[] => CFG1, max1, [], false
               */

              st <- get
              _ <- {
                val max = st.currId
                val currNodeId = internalIdent(s"${labPref}${max}")
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

  def formalArgsAsDecls(idents: List[Ident], cfg: CFG): CFG = {
    val entryLabel = internalIdent(s"${labPref}0")
    cfg.get(entryLabel) match {
      case None    => cfg
      case Some(n) => cfg + (entryLabel -> n.copy(lVars = idents ++ n.lVars))
    }
  }

  trait HasVar[A] {
    def getVarsFrom(a: A): List[Ident]
    def getLVarsFrom(a: A): List[Ident] = List()
  }

  object HasVarcfgOps {
    def getVarsFrom[A](a: A)(implicit hv: HasVar[A]): List[Ident] =
      hv.getVarsFrom(a)
    def getLVarsFrom[A](a: A)(implicit hv: HasVar[A]): List[Ident] =
      hv.getLVarsFrom(a)
  }

  implicit def getVarsFromVarDecl: HasVar[VarDecl] =
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

  implicit def getVarsFromVarInit: HasVar[VarInit] =
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

  implicit def getVarsFromExp: HasVar[Exp] =
    new HasVar[Exp] {
      override def getLVarsFrom(exp: Exp): List[Ident] =
        exp match {
          case Lit(lit)                                             => List()
          case ClassLit(ty)                                         => List()
          case This                                                 => List()
          case ThisClass(name)                                      => List()
          case InstanceCreation(type_args, type_decl, args, body)   => List()
          case QualInstanceCreation(exp, type_args, id, args, body) => List()
          case ArrayCreate(ty, exps, num_dims)                      => exps.flatMap(getLVarsFrom(_))
          case ArrayCreateInit(ty, size, init) =>
            init match {
              case ArrayInit(var_inits) =>
                var_inits.flatMap(HasVarcfgOps.getLVarsFrom(_))
            }
          case FieldAccess_(access) => HasVarcfgOps.getLVarsFrom(access)
          case MethodInv(methodInv) => HasVarcfgOps.getLVarsFrom(methodInv)
          case ArrayAccess(idx)     => HasVarcfgOps.getLVarsFrom(idx)
          case ExpName(name)        => List()
          case PostIncrement(exp)   => getLVarsFrom(exp)
          case PostDecrement(exp)   => getLVarsFrom(exp)
          case PreIncrement(exp)    => getLVarsFrom(exp)
          case PreDecrement(exp)    => getLVarsFrom(exp)
          case PrePlus(exp)         => getLVarsFrom(exp)
          case PreMinus(exp)        => getLVarsFrom(exp)
          case PreBitCompl(exp)     => getLVarsFrom(exp)
          case PreNot(exp)          => getLVarsFrom(exp)
          case Cast(ty, exp)        => getLVarsFrom(exp)

          case BinOp(e1, op, e2)       => getLVarsFrom(e1) ++ getLVarsFrom(e2)
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
          case Lit(lit)        => List()
          case ClassLit(ty)    => List()
          case This            => List()
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
          case FieldAccess_(access)    => HasVarcfgOps.getVarsFrom(access)
          case MethodInv(methodInv)    => HasVarcfgOps.getVarsFrom(methodInv)
          case ArrayAccess(idx)        => HasVarcfgOps.getVarsFrom(idx)
          case ExpName(name)           => HasVarcfgOps.getVarsFrom(name)
          case PostIncrement(exp)      => getVarsFrom(exp)
          case PostDecrement(exp)      => getVarsFrom(exp)
          case PreIncrement(exp)       => getVarsFrom(exp)
          case PreDecrement(exp)       => getVarsFrom(exp)
          case PrePlus(exp)            => getVarsFrom(exp)
          case PreMinus(exp)           => getVarsFrom(exp)
          case PreBitCompl(exp)        => getVarsFrom(exp)
          case PreNot(exp)             => getVarsFrom(exp)
          case Cast(ty, exp)           => getVarsFrom(exp)
          case BinOp(e1, op, e2)       => getVarsFrom(e1) ++ getVarsFrom(e2)
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

  implicit def getVarsFromFieldAccess: HasVar[FieldAccess] =
    new HasVar[FieldAccess] {
      override def getLVarsFrom(field_access: FieldAccess): List[Ident] =
        List() // TODO: check whether it should indeed empty
      override def getVarsFrom(field_access: FieldAccess): List[Ident] =
        field_access match {
          case PrimaryFieldAccess(e, id)  => HasVarcfgOps.getVarsFrom(e)
          case SuperFieldAccess(id)       => List()
          case ClassFieldAccess(name, id) => List()
        }
    }

  implicit def getVarsFromMethodInvocation: HasVar[MethodInvocation] =
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

  implicit def getVarsFromArrayIndex: HasVar[ArrayIndex] =
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

  implicit def getVarsFromName: HasVar[Name] =
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

  implicit def getVarsFromLhs: HasVar[Lhs] =
    new HasVar[Lhs] {
      override def getVarsFrom(lhs: Lhs): List[Ident] =
        lhs match {
          case NameLhs(name)          => List()
          case FieldLhs(field_access) => HasVarcfgOps.getVarsFrom(field_access)
          case ArrayLhs(array_idx)    => HasVarcfgOps.getVarsFrom(array_idx)
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

  implicit def getVarsFromLambdaParams: HasVar[LambdaParams] =
    new HasVar[LambdaParams] {
      override def getVarsFrom(ps: LambdaParams): List[Ident] =
        ps match {
          case LambdaSingleParam(id) => List(id)
          case LambdaFormalParams(formal_params) =>
            formal_params.flatMap(HasVarcfgOps.getVarsFrom(_))
          case LambdaInferredParams(ids) => ids
        }
    }

  implicit def getVarsFromLambdaExpression: HasVar[LambdaExpression] =
    new HasVar[LambdaExpression] {
      override def getVarsFrom(lambExp: LambdaExpression): List[Ident] =
        lambExp match {
          case LambdaExpression_(e) => HasVarcfgOps.getVarsFrom(e)
          case LambdaBlock(blk)     => HasVarcfgOps.getVarsFrom(blk)
        }
      override def getLVarsFrom(lambExp: LambdaExpression): List[Ident] =
        lambExp match {
          case LambdaExpression_(e) => HasVarcfgOps.getLVarsFrom(e)
          case LambdaBlock(blk)     => HasVarcfgOps.getLVarsFrom(blk)
        }

    }

  implicit def getVarsFromBlock: HasVar[Block] =
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

  implicit def getVarsFromBlockStmt: HasVar[BlockStmt] =
    new HasVar[BlockStmt] {
      override def getVarsFrom(blkStmt: BlockStmt): List[Ident] =
        blkStmt match {
          case BlockStmt_(stmt)       => HasVarcfgOps.getVarsFrom(stmt)
          case LocalClass(class_decl) => List() // TODO:Fixme
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarcfgOps.getVarsFrom(_))
        }
      override def getLVarsFrom(blkStmt: BlockStmt): List[Ident] =
        blkStmt match {
          case BlockStmt_(stmt)       => HasVarcfgOps.getLVarsFrom(stmt)
          case LocalClass(class_decl) => List() // TODO:Fixme
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarcfgOps.getLVarsFrom(_))
        }
    }

  implicit def getVarsFromStmt: HasVar[Stmt] =
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
          case Empty        => List()
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
          case Break(_)    => List()
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
          case Empty        => List()
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
          case Break(_)    => List()
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

  implicit def getVarsFromCatch: HasVar[Catch] =
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

  implicit def getVarsFromSwitchBlock: HasVar[SwitchBlock] =
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

  implicit def getVarsFromSwitchLabel: HasVar[SwitchLabel] =
    new HasVar[SwitchLabel] {
      override def getVarsFrom(switch_label: SwitchLabel): List[Ident] =
        switch_label match {
          case SwitchCase(exp) => HasVarcfgOps.getVarsFrom(exp)
          case Default         => List()
        }
      override def getLVarsFrom(switch_label: SwitchLabel): List[Ident] =
        switch_label match {
          case SwitchCase(exp) => HasVarcfgOps.getLVarsFrom(exp)
          case Default         => List()
        }
    }

  implicit def getVarsFromFormalParam: HasVar[FormalParam] =
    new HasVar[FormalParam] {
      override def getVarsFrom(fp: FormalParam): List[Ident] =
        fp match {
          case FormalParam(modifiers, ty, has_arity, var_decl_id) =>
            List(idFromVarDeclId(var_decl_id))
        }
    }

  implicit def getVarsFromForInit: HasVar[ForInit] =
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
    def getLhsFrom[A](a: A)(implicit hn: ConvertableToLhs[A]): List[Lhs] =
      hn.getLhsFrom(a)
  }

  implicit def expConvertableToLhs: ConvertableToLhs[Exp] =
    new ConvertableToLhs[Exp] {
      // dominating name
      override def getLhsFrom(exp: Exp): List[Lhs] =
        exp match {
          case Lit(lit)                                             => List()
          case ClassLit(ty)                                         => List()
          case This                                                 => List()
          case ThisClass(name)                                      => List()
          case InstanceCreation(type_args, type_decl, args, body)   => List()
          case QualInstanceCreation(exp, type_args, id, args, body) => List()
          case ArrayCreate(ty, exps, num_dims)                      => List()
          case ArrayCreateInit(ty, size, init)                      => List()
          case FieldAccess_(access)                                 => List(FieldLhs(access))
          case MethodInv(methodInv)                                 => List()
          case ArrayAccess(idx)                                     => List(ArrayLhs(idx))
          case ExpName(name)                                        => List(NameLhs(name))
          case PostIncrement(exp)                                   => getLhsFrom(exp)
          case PostDecrement(exp)                                   => getLhsFrom(exp)
          case PreIncrement(exp)                                    => getLhsFrom(exp)
          case PreDecrement(exp)                                    => getLhsFrom(exp)
          case PrePlus(exp)                                         => getLhsFrom(exp)
          case PreMinus(exp)                                        => getLhsFrom(exp)
          case PreBitCompl(exp)                                     => getLhsFrom(exp)
          case PreNot(exp)                                          => getLhsFrom(exp)
          case Cast(ty, exp)                                        => getLhsFrom(exp)
          case BinOp(e1, op, e2)                                    => List()
          case InstanceOf(e, ref_type)                              => List()
          case Cond(cond, true_exp, false_exp)                      => List()
          case Assign(lhs, op, rhs)                                 => List(lhs)
          case Lambda(params, body)                                 => List()
          case MethodRef(name, id)                                  => List()
        }
    }

}
