package com.github.luzhuomi.obsidian

import cats._
import cats.implicits._
import cats.data.StateT

import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.obsidian.ASTUtils._
import com.github.luzhuomi.scalangj.Syntax
import _root_.cats.syntax.contravariant

/*
 Control Flow Graph construction
 */


/* 
object CFG {

  type NodeId = Ident

  type CFG = Map[NodeId, Node]

  /**
   * redesigning the CFG data type. Unlike the C CFG, which has only a single Node type
   * the Java CFG should have a proper algebraic data type node. Some node has no statement
   * e.g. If-else, while, try catch finally.
   * 
  */
  
  sealed trait Node
  

  /**
    * A CFG node contains a sequence of assignment statments
    *
    * @param stmts list of statements
    * @param lVars variables appearing on the lhs of assignments
    * @param rVars variables appearing on the rhs of the assignments 
    * @param localDecls locally declared variables
    * @param preds predecessor node ids
    * @param succs successor node ids
    */
  case class AssignmentNode(
    stmts: List[BlockStmt], 
    lVars: List[Ident], 
    rVars: List[Ident], 
    localDecls: List[Ident],
    preds: List[NodeId],
    succs: List[NodeId]
  )
  /**
    * A CFG node contains an if-else statement
    *
    * @param condExp condintional expression
    * @param thenNode then node id
    * @param elseNode else node id
    * @param preds predecessor ids
    */
  case class IfElseNode(
    rVars: List[Ident],
    condExp: Option[Exp],
    thenNode: NodeId,
    elseNode: Option[NodeId],
    preds: List[NodeId]
  ) {
    val succs = this.thenNode :: this.elseNode.toList
  }

  case class LoopNode (
    condExp: Option[Exp],
    bodyNode: NodeId, 
    preds: List[NodeId],
    succs: List[NodeId]
  )

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
    0,
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

  /*

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
   */

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

  trait CFGClass[A] {
    def buildCFG(a: A)(implicit
        m: MonadError[SIState, String]
    ): State[StateInfo, Unit]
  }

  object ops {
    def buildCFG[A](
        a: A
    )(implicit aCFGCl: CFGClass[A]): State[StateInfo, Unit] = {
      aCFGCl.buildCFG(a)
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
          a: MethodDecl
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
              _ <- ops.buildCFG(body);
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
          a: MethodBody
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case MethodBody(None)      => m.pure(())
          case MethodBody(Some(blk)) => ops.buildCFG(blk)
        }
    }

  /*
  max1 = max + 1
  l0' = max
  CFG1 = CFG update { pred : { succ = {max} } } union { l0' : { stmts = { if (exp == e1) { goto l1; } else { goto l1'; } }}, succs = { l1,l1'}, preds = preds }  update { l1: { preds += l0' } }
                                                union { l1' : { stmts = { if (exp == e2) { goto l2; } else { goto l2'; } }}, succs = { l2,l2'}, preds = {l0'} }  update { l2: { preds += l1' } }
                                                union { l2' : { stmts = { if (exp == e3) { goto l3; } else { goto l3'; } }}, succs = { l3,l3'}, preds = {l1'} }  update { l3: { preds += l2' } }
                                                ...
                                                union { ln-1' : { stmts = { if (exp == en) { goto ln; } else { goto l_default; }}, succs = { ln, l_default }, preds = {ln-2'} } update { ln- : { preds += ln-1' }} update { l_default : { preds += ln-1' } }

  CFG1, max1, {}, false, {}, contNodes, {} |- stmt1,..., stmtn+1 => CFG2, max2, preds2, continable2, breakNodes, contNodes2, {(l1,l1',e1),...,(l_default, _)}
  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  CFG, max, preds, continuable, breakNodes, contNodes, caseNodes |- switch exp { stmt1,...,stmtn }   => CFG2, max2, preds2 union breakNodes2 , false, breakNodes, contNodes2, caseNodes
   */
  implicit def blockCFGInstance: CFGClass[Block] =
    new CFGClass[Block] {
      override def buildCFG(
          a: Block
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case Block(Nil) => {
            val lhs = Nil
            val rhs = Nil
            for {
              st <- get;
              max <- m.pure(st.currId);
              currNodeId <- m.pure(internalIdent(s"${labPref}${max}"));
              max1 <- m.pure(max + 1);
              cfg0 <- m.pure(st.cfg);
              preds0 <- m.pure(st.currPreds);
              cfgNode <-
                m.pure(Node(Nil, lhs, rhs, Nil, preds0, Nil, EmptyNode));
              cfg1p <- m.pure(preds0.foldLeft(cfg0)((g, pred) => {
                val n: Node = g(pred)
                g + (pred -> n.copy(succs = n.succs ++ List(currNodeId)))
              }));
              cfg1 <- m.pure(cfg1p + (currNodeId -> cfgNode));
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
          case Block(stmts) =>
            for {
              _ <- stmts.traverse_(stmt => ops.buildCFG(stmt))
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
                      ops.buildCFG(empty)
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
          a: BlockStmt
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case LocalClass(_) => m.raiseError("Local Class is not supported.")
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls match {
              case Nil               => m.pure(())
              case (var_decl :: Nil) =>
                /*
        CFG1 = CFG update { pred : {stmts = stmts ++ [ty x = exp[]], lVars = lVars ++ [x] } }
        --------------------------------------------------------
        CFG, max, preds, true |- ty x = exp[] => CFG1, max, [] , false

        max1 = max + 1
        CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : {ty x = exp[] } }
        --------------------------------------------------------
        CFG, max, preds, false |- ty x = exp[] => CFG1, max1, [], false
                 */
                for {
                  st <- get;
                  _ <-
                    if (st.continuable) {
                      val cfg0 = st.cfg
                      val preds0 = st.currPreds
                      val s = a
                      val lvars = HasVarOps.getLVarsFrom(var_decl)
                      val rvars = HasVarOps.getVarsFrom(var_decl)
                      val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                        val n: Node = g(pred)
                        val n1 = n.copy(
                          stmts = n.stmts ++ List(s),
                          localDecls = n.localDecls ++ lvars,
                          lVars = n.lVars ++ lvars,
                          rVars = n.rVars ++ rvars
                        )
                        g + (pred -> n1)
                      })
                      for {
                        _ <- put(st.copy(cfg = cfg1))
                      } yield ()
                    } else {
                      val max = st.currId
                      val currNodeId = internalIdent(s"${labPref}${max}")
                      val max1 = max + 1
                      val cfg0 = st.cfg
                      val preds0 = st.currPreds
                      val s = a
                      val lvars = HasVarOps.getLVarsFrom(var_decl)
                      val rvars = HasVarOps.getVarsFrom(var_decl)
                      val cfgNode = Node(
                        List(s),
                        lvars,
                        rvars,
                        lvars,
                        preds0,
                        Nil,
                        AssignmentNode
                      )
                      val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                        val n: Node = g(pred)
                        val n1 = n.copy(succs =
                          (n.succs ++ List(currNodeId)).toSet.toList
                        )
                        g + (pred -> n1)
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
                } yield ()
              case (var_decl :: rest) => for {
                _ <- buildCFG(LocalVars(modifiers, ty, var_decl::Nil))
                _ <- buildCFG(LocalVars(modifiers, ty, rest))
              } yield ()
            }
          case BlockStmt_(stmt) => ops.buildCFG(stmt)
        }
    }

  implicit def stmtCFGInstance: CFGClass[Stmt] =
    new CFGClass[Stmt] {
      override def buildCFG(
          a: Stmt
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] =
        a match {
          case StmtBlock(blk)                         => ops.buildCFG(blk)
          case IfThen(exp, stmt)                      => buildCFG(IfThenElse(exp, stmt, Empty))
          case IfThenElse(exp, true_stmt, false_stmt) =>
            /*
      max1 = max + 1
      CFG1 = CFG update { pred : {succ = max} |  pred <- preds } union { max : { stmts =  [ if exp { continue max1 } else { continue max2 } ], succ = [], preds = preds} }
      CFG1, max1, {max}, false |-n trueStmt => CFG2, max2, preds1, _
      CFG2, max2, {max}, false |-n falseStmt => CFG3, max3, preds2, _
      -------------------------------------------------------------------------------------------------------------
      CFG, max, preds, _ |- if exp { trueStmt } else { falseStmt }  => CFG3, max3, preds1 U preds2, false

      In Java, we replace goto max1 and goto max2 with max1:Empty and max2:Empty
             */
            for {
              st <- get;
              _ <- {
                val max = st.currId
                val currNodeId = internalIdent(s"${labPref}${max}")
                val lhs = HasVarOps.getLVarsFrom(exp)
                val rhs = HasVarOps.getVarsFrom(exp)
                val max1 = max + 1
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                /* Note: we don't have max2 until we have CFG1
             CFG1, max1, {max}, false |-n trueStmt => CFG, max2, preds1, _
             we can given an empty statement to the new CFG node in CFG1 first and update it
             after we have max2
                 */
                val cfgNode = Node(Nil, lhs, rhs, Nil, preds0, Nil, IfElseNode)
                val cfg1p = preds0.foldLeft(cfg0)((g, pred) => {
                  val n = g(pred)
                  val n1 =
                    n.copy(succs = (n.succs ++ List(currNodeId)).toSet.toList)
                  g + (pred -> n1)
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
                  );
                  _ <- buildCFG(true_stmt);
                  st1 <- get;
                  _ <- {
                    val max2 = st1.currId
                    val preds1 = st1.currPreds
                    val s = BlockStmt_(
                      IfThenElse(
                        exp,
                        Continue(Some(internalIdent(s"${labPref}${max1}"))),
                        Continue(Some(internalIdent(s"${labPref}${max2}")))
                      )
                    )
                    val cfg2 = st1.cfg
                    val n = cfg2(currNodeId)
                    val cfg2p = cfg2 + (currNodeId -> n.copy(stmts = List(s)))
                    for {
                      _ <- put(
                        st1.copy(
                          cfg = cfg2p,
                          currId = max2,
                          currPreds = List(currNodeId),
                          continuable = false
                        )
                      )
                      _ <- buildCFG(false_stmt)
                      st2 <- get
                      _ <- {
                        val max3 = st2.currId
                        val preds2 = st2.currPreds
                        val cfg3 = st2.cfg
                        for {
                          _ <- put(
                            st2.copy(
                              cfg = cfg3,
                              currId = max3,
                              currPreds = preds1 ++ preds2,
                              continuable = false
                            )
                          )
                        } yield ()
                      }
                    } yield ()
                  }
                } yield ()
              }
            } yield ()
          case Switch(exp, blocks) =>
            for {
              st <- get
              _ <- {
                val max = st.currId
                val currNodeId = internalIdent(s"${labPref}${max}")
                val lhs = HasVarOps.getLVarsFrom(exp)
                val rhs = HasVarOps.getVarsFrom(exp)
                val max1 = max + 1
                val l0 = max
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val contNodes0 = st.contNodes
                val breakNodes0 = st.breakNodes
                val caseNodes0 = st.caseNodes
                for {
                  _ <- put(
                    st.copy(
                      currPreds = Nil,
                      fallThroughCases = Nil,
                      continuable = false,
                      breakNodes = Nil,
                      caseNodes = Nil
                    )
                  )
                  _ <- blocks.traverse_((b: SwitchBlock) => ops.buildCFG(b))
                  st1 <- get
                  _ <- {
                    val preds1 = st1.breakNodes
                    val breakNodes1 = st1.breakNodes
                    val caseNodes1 = st1.caseNodes
                    for {
                      _ <- put(st1.copy(currPreds = preds0))
                      mb_preds <- caseExpsToIfNodes(exp, caseNodes1)
                      st2 <- get
                      _ <- put(
                        st2.copy(
                          currPreds = preds1 ++ breakNodes1 ++ mb_preds,
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
            for {
              st <- get
              _ <- {
                val max = st.currId
                val currNodeId = internalIdent(s"${labPref}${max}")
                val lhs = HasVarOps.getVarsFrom(exp)
                val rhs = HasVarOps.getLVarsFrom(exp)
                val max1 = max + 1
                val cfg0 = st.cfg
                val preds0 = st.currPreds
                val contNodes0 = st.contNodes
                val breakNodes0 = st.breakNodes
                val cfgNode =
                  Node(Nil, lhs, rhs, Nil, preds0, Nil, LoopNode(Nil, Nil))
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
                      continuable = false,
                      breakNodes = Nil
                    )
                  )
                  _ <- buildCFG(stmt)
                  st1 <- get
                  _ <- {
                    val max2 = st1.currId
                    val preds1 = st1.currPreds
                    /* s         = AST.CBlockStmt $ AST.CIf exp
                      (AST.CGoto (internalIdent (labPref ++ show max1)) nodeInfo)
                      (Just (AST.CGoto (internalIdent (labPref ++ show max2)) nodeInfo)) nodeInfo -- problem, the following statement is not neccessarily max2, i.e. the next available num. the next available num can be used in a sibling block, e.g. the current loop is in then branch, the next available int is usined in the else branch for instance
              int search(int a[], int size, int x) {
              if (a) { // 0
                int i;  // 1
                for (i=0 /* // 2 */; i< size; i++) // 3
                  {
                  // 4
                  if (a[i] == x)
                    {
                      return i; // 5
                    }
                  // 6
                  // 7 i++
                      }
                  }
                  // 8 implicit else
                  // 9
                return -1;
                }
                3 will have if ... { goto 4 } else { goto 8 } which is an else from the outer if statement 0.
                the correct translation should be  if ... { goto 4 } else { goto 9 }
                     */
                    val s = BlockStmt_(
                      IfThen(
                        exp,
                        Continue(Some(internalIdent(s"${labPref}${max1}")))
                      )
                    )
                    val cfg2 = st1.cfg
                    val cfg2p = preds1.foldLeft(cfg2)((g, pred) => {
                      val n = g(pred)
                      g + (pred -> n.copy(succs =
                        (n.succs ++ List(currNodeId)).toSet.toList
                      ))
                    })
                    val breakNodes2 = st1.breakNodes
                    val contNodes2 = st1.contNodes
                    val currNode = cfg2p(currNodeId)
                    val cfg2pp = cfg2p + (currNodeId -> currNode.copy(
                      stmts = List(s),
                      preds =
                        (currNode.preds ++ preds1 ++ contNodes2).toSet.toList,
                      succs = currNode.succs.toSet.toList,
                      nodeType = LoopNode(breakNodes2, contNodes2)
                    ))
                    val cfg3 = contNodes2.foldLeft(cfg2pp)(
                      (g, l) => { // update the break and cont immediately
                        val n = g(l)
                        g + (l -> n.copy(succs =
                          (n.succs ++ List(currNodeId)).toSet.toList
                        ))
                      }
                    )
                    /*
              val cfg3p  = breakNodes2.foldLeft(cfg3) ( (g,l) => {
                val n = g(l)
                g + ( l -> n.copy(succs = (n.succs ++ List(intdent(s"${labPref}${max2}")).toSet.toList)))
              })
                     */
                    val cfg3p = cfg3
                    for {
                      _ <- put(
                        st1.copy(
                          cfg = cfg3p,
                          currId = max2,
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
          case Do(stmt, exp) =>
            for {
              _ <- buildCFG(stmt)
              _ <- buildCFG(While(exp, stmt))
            } yield ()

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
                case Some(flv@ForLocalVars(modifiers, ty, var_decls)) =>
                  ops.buildCFG(flv)
                  // var_decls.traverse_(vd => ops.buildCFG(vd))
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
                  _ <- ops.buildCFG(blk)
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
            val xs = HasVarOps.getLVarsFrom(lhs).toSet
            val ys = HasVarOps.getVarsFrom(rhs)
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

          case ExpStmt(PostIncrement(exp)) => ConvertableToLhsOps.getLhsFrom(exp) match {
              case Nil => m.raiseError(s"BuildCFG Failed: Fail to extract lhs from exp ${exp}")
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, AddA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>  m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")  
          }
          case ExpStmt(PostDecrement(exp)) => ConvertableToLhsOps.getLhsFrom(exp) match {
              case Nil => m.raiseError(s"BuildCFG Failed: Fail to extract lhs from exp ${exp}")
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, SubA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>  m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")  
          }
          case ExpStmt(PreIncrement(exp)) => ConvertableToLhsOps.getLhsFrom(exp) match {
              case Nil => m.raiseError(s"BuildCFG Failed: Fail to extract lhs from exp ${exp}")
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, AddA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>  m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")  
          }
          case ExpStmt(PreDecrement(exp)) => ConvertableToLhsOps.getLhsFrom(exp) match {
              case Nil => m.raiseError(s"BuildCFG Failed: Fail to extract lhs from exp ${exp}")
              case List(lhs) => {
                val stmt = ExpStmt(Assign(lhs, SubA, Lit(IntLit(1))))
                for {
                  _ <- buildCFG(ExpStmt(exp))
                  _ <- buildCFG(stmt)
                } yield ()
              }
              case _ =>  m.raiseError(s"BuildCFG Failed: too many lhs from exp ${exp}")  
          }
          case ExpStmt(e) => for {
            st <- get
            _  <- {
              val cfg0 = st.cfg
              val lhs = HasVarOps.getLVarsFrom(e)
              val rhs = HasVarOps.getVarsFrom(e)
              val preds0 = st.currPreds
              val s = BlockStmt_(ExpStmt(e))
              if (st.continuable) {
                val cfg1 = preds0.foldLeft(cfg0) ((g,pred) => {
                  val n = g(pred)
                  g + (pred -> n.copy(stmts = n.stmts ++ List(s), lVars = n.lVars ++ lhs, rVars = n.rVars ++ rhs))
                })
                put(st.copy(cfg=cfg1, continuable=true))
              } else {
                val max = st.currId
                val currNodeId = internalIdent(s"${labPref}${max}")
                val max1 = max + 1 
                val cfgNode = Node(List(s), Nil, lhs, rhs, preds0, Nil, AssignmentNode)
                val cfg1p = preds0.foldLeft(cfg0)((g,pred) => {
                  val n = g(pred)
                  g + (pred -> n.copy(succs = n.succs ++ List(currNodeId) ))
                })
                val cfg1 = cfg1p + (currNodeId -> cfgNode)
                put(st.copy(cfg=cfg1, currId=max1, currPreds=List(currNodeId), continuable=true))
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
                val lhs = es.flatMap(HasVarOps.getLVarsFrom(_))
                val rhs = es.flatMap(HasVarOps.getVarsFrom(_))
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
                  val cfg1 = preds0.foldLeft(cfg0)((g, pred) => {
                    val n = g(pred)
                    g + (pred -> n.copy(
                      stmts = n.stmts ++ List(s),
                      succs = n.succs ++ List(lp)
                    ))
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
                  val lhs = o_exp.toList.flatMap(HasVarOps.getLVarsFrom(_))
                  val rhs = o_exp.toList.flatMap(HasVarOps.getVarsFrom(_))
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
                  val lhs = o_exp.toList.flatMap(HasVarOps.getLVarsFrom(_))
                  val rhs = o_exp.toList.flatMap(HasVarOps.getVarsFrom(_))
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
                val lhs = HasVarOps.getLVarsFrom(exp)
                val rhs = HasVarOps.getVarsFrom(exp)
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
                  _ <- ops.buildCFG(try_blk)
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
                          _ <- ops.buildCFG(c)
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
                              _ <- ops.buildCFG(blk)
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
            val lhs = HasVarOps.getLVarsFrom(exp)
            val rhs = HasVarOps.getVarsFrom(exp)
            val lhsp = (e :: es).flatMap(HasVarOps.getLVarsFrom(_))
            val rhsp = (e :: es).flatMap(HasVarOps.getVarsFrom(_))
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
            val lhs = HasVarOps.getLVarsFrom(exp)
            val rhs = HasVarOps.getVarsFrom(exp)
            val lhsp = (e :: es).flatMap(HasVarOps.getLVarsFrom(_))
            val rhsp = (e :: es).flatMap(HasVarOps.getVarsFrom(_))
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
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] = a match {
        case Catch(params, blk) => for {
          st <- get
          _  <- {
            val max = st.currId
            val l = internalIdent(s"${labPref}${max}")
            val lhs = HasVarOps.getLVarsFrom((params))
            for {
              _ <- ops.buildCFG(blk)
              st2 <- get
              _ <- {
                val cfg2 = st2.cfg
                val n = cfg2(l)
                val np = n.copy(localDecls = n.localDecls ++ lhs)
                val cfg3 = cfg2 + (l -> np)
                for {
                  _ <- put(st2.copy(cfg = cfg3, catchNodes = st.catchNodes ++ List(l)))
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
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] = a match {
        case SwitchBlock(Default, blks_stmts) => for {
          /*
          CFG, max, preds, continuable, breakNodes, contNodes, caseNodes |- 
            stmt => CFG2, max2, preds2, continuable2, breakNodes2, contNodes2, caseNodes2 
          ------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
          CFG,max,preds, continuable, breakNodes, contNodes, caseNodes |- 
            default: stmt => CFG2, max2, preds2 continuable2, breakNodes, contNodes2, caseNodes2 union (max, default) 
          */
          st <- get 
          _  <- { 
            val max = st.currId
            val wrapNodeId = internalIdent(s"${labPref}${max}")
            val rhsNodeId  = internalIdent(s"${labPref}${max+1}")
            for {
              _ <- put(st.copy(currId = max + 1, fallThroughCases = Nil))
              _ <- blks_stmts.traverse_(ops.buildCFG(_))
              st1 <- get
              _ <- put(st1.copy(caseNodes=st1.caseNodes ++ List(DefaultCase(wrapNodeId,rhsNodeId))))
            } yield ()
          }
        } yield () 
        case SwitchBlock(SwitchCase(e), blk_stmts) => for {
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
            val rhsNodeId  = internalIdent(s"${labPref}${max+1}")
            for {
              _ <- put(st.copy(currId=max + 1, fallThroughCases=Nil))
              _ <- blk_stmts.traverse_(ops.buildCFG(_))
              st1 <- get
              _ <- put(st1.copy(caseNodes=st1.caseNodes++List(ExpCase(e, fallThrough, wrapNodeId, rhsNodeId))))
            } yield ()
          }
        } yield ()
      }
    }

  implicit def varDeclCFGInstance: CFGClass[ForLocalVars] =
    new CFGClass[ForLocalVars] {
      override def buildCFG(
          a: ForLocalVars
      )(implicit m: MonadError[SIState, String]): State[StateInfo, Unit] = a match {

        case ForLocalVars(modifiers, ty, var_decls) => for {

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
            val lhs = var_decls.flatMap(HasVarOps.getLVarsFrom(_))
            val rhs = var_decls.flatMap(HasVarOps.getVarsFrom(_))
            val preds0 = st.currPreds
            val cfgNode = Node(List(s), Nil, lhs, rhs, preds0, Nil, Other)
            val cfg0 = st.cfg
            val cfg1p = preds0.foldLeft(cfg0) ((g,pred) => {
              val n = g(pred)
              g + (pred -> n.copy(succs = (n.succs ++ List(currNodeId)).toSet.toList ))
            })
            val cfg1 = cfg1p + (currNodeId -> cfgNode)
            for {
              _ <-put(st.copy(cfg=cfg1, currId=max+1, currPreds=List(currNodeId), continuable=true))
            } yield ()
          }
        } yield ()
      }
    }

  def internalIdent(s: String): Ident = Ident(s)
  def formalArgsAsDecls(idents: List[Ident], cfg: CFG): CFG = cfg // TODO:fixme

  trait HasVar[A] {
    def getVarsFrom(a: A): List[Ident]
    def getLVarsFrom(a: A): List[Ident] = List()
  }

  object HasVarOps {
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
            HasVarOps.getVarsFrom(var_init)
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
          case InitExp(exp) => HasVarOps.getVarsFrom(exp)
          case InitArray(ArrayInit(var_inits)) =>
            var_inits.flatMap(HasVarOps.getVarsFrom(_))
        }
      override def getLVarsFrom(var_init: VarInit): List[Ident] =
        var_init match {
          case InitExp(exp) => HasVarOps.getLVarsFrom(exp)
          case InitArray(ArrayInit(var_inits)) =>
            var_inits.flatMap(HasVarOps.getLVarsFrom(_))
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
                var_inits.flatMap(HasVarOps.getLVarsFrom(_))
            }
          case FieldAccess_(access) => HasVarOps.getLVarsFrom(access)
          case MethodInv(methodInv) => HasVarOps.getLVarsFrom(methodInv)
          case ArrayAccess(idx)     => HasVarOps.getLVarsFrom(idx)
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
            getLVarsFrom(rhs) ++ HasVarOps.getLVarsFrom(lhs)
          case Lambda(params, body) => {
            val ps = HasVarOps.getVarsFrom(params).toSet
            HasVarOps.getLVarsFrom(body).filterNot(ps)
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
            args.flatMap(HasVarOps.getVarsFrom(_))
          // note: we can skip the body because any reference to the variables defined the enclosing scope
          //       because those variables must be final or effectively final
          case QualInstanceCreation(exp, type_args, id, args, body) =>
            args.flatMap(HasVarOps.getVarsFrom(_))
          case ArrayCreate(ty, exps, num_dims) => exps.flatMap(getVarsFrom(_))
          case ArrayCreateInit(ty, size, init) =>
            init match {
              case ArrayInit(var_inits) =>
                var_inits.flatMap(HasVarOps.getVarsFrom(_))
            }
          case FieldAccess_(access)    => HasVarOps.getVarsFrom(access)
          case MethodInv(methodInv)    => HasVarOps.getVarsFrom(methodInv)
          case ArrayAccess(idx)        => HasVarOps.getVarsFrom(idx)
          case ExpName(name)           => HasVarOps.getVarsFrom(name)
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
            getVarsFrom(rhs) // HasVarOps.getVarsFrom(lhs) ++ getVarsFrom(rhs)
          case Lambda(params, body) => {
            val ps = HasVarOps.getVarsFrom(params).toSet
            HasVarOps.getVarsFrom(body).filterNot(ps)
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
          case PrimaryFieldAccess(e, id)  => HasVarOps.getVarsFrom(e)
          case SuperFieldAccess(id)       => List()
          case ClassFieldAccess(name, id) => List()
        }
    }

  implicit def getVarsFromMethodInvocation: HasVar[MethodInvocation] =
    new HasVar[MethodInvocation] {
      override def getVarsFrom(methodInv: MethodInvocation): List[Ident] =
        methodInv match {
          case MethodCall(name, args) =>
            HasVarOps.getVarsFrom(name) ++ args.flatMap(
              HasVarOps.getVarsFrom(_)
            )
          case PrimaryMethodCall(e, ref_type, id, args) =>
            HasVarOps.getVarsFrom(e) ++ args.flatMap(HasVarOps.getVarsFrom(_))
          case SuperMethodCall(ref_types, id, args) =>
            args.flatMap(HasVarOps.getVarsFrom(_))
          case ClassMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarOps.getVarsFrom(_))
          case TypeMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarOps.getVarsFrom(_))
        }
      override def getLVarsFrom(methodInv: MethodInvocation): List[Ident] =
        methodInv match {
          case MethodCall(name, args) => args.flatMap(HasVarOps.getLVarsFrom(_))
          case PrimaryMethodCall(e, ref_type, id, args) =>
            HasVarOps.getLVarsFrom(e) ++ args.flatMap(HasVarOps.getLVarsFrom(_))
          case SuperMethodCall(ref_types, id, args) =>
            args.flatMap(HasVarOps.getLVarsFrom(_))
          case ClassMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarOps.getLVarsFrom(_))
          case TypeMethodCall(name, ref_types, id, args) =>
            args.flatMap(HasVarOps.getLVarsFrom(_))
        }
    }

  implicit def getVarsFromArrayIndex: HasVar[ArrayIndex] =
    new HasVar[ArrayIndex] {
      override def getVarsFrom(idx: ArrayIndex): List[Ident] =
        idx match {
          case ArrayIndex(e, es) =>
            HasVarOps.getVarsFrom(e) ++ es.flatMap(HasVarOps.getVarsFrom(_))
        }
      override def getLVarsFrom(idx: ArrayIndex): List[Ident] =
        idx match {
          case ArrayIndex(e, es) =>
            HasVarOps.getLVarsFrom(e) ++ es.flatMap(HasVarOps.getLVarsFrom(_))
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
          case FieldLhs(field_access) => HasVarOps.getVarsFrom(field_access)
          case ArrayLhs(array_idx)    => HasVarOps.getVarsFrom(array_idx)
        }
      override def getLVarsFrom(lhs: Lhs): List[Ident] =
        lhs match {
          case NameLhs(name) => HasVarOps.getVarsFrom(name)
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
            formal_params.flatMap(HasVarOps.getVarsFrom(_))
          case LambdaInferredParams(ids) => ids
        }
    }

  implicit def getVarsFromLambdaExpression: HasVar[LambdaExpression] =
    new HasVar[LambdaExpression] {
      override def getVarsFrom(lambExp: LambdaExpression): List[Ident] =
        lambExp match {
          case LambdaExpression_(e) => HasVarOps.getVarsFrom(e)
          case LambdaBlock(blk)     => HasVarOps.getVarsFrom(blk)
        }
      override def getLVarsFrom(lambExp: LambdaExpression): List[Ident] =
        lambExp match {
          case LambdaExpression_(e) => HasVarOps.getLVarsFrom(e)
          case LambdaBlock(blk)     => HasVarOps.getLVarsFrom(blk)
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
                    var_decls.flatMap(HasVarOps.getLVarsFrom(_))
                  case _ => List()
                }
              )
              .toSet
            val otherVars = others.flatMap(
              HasVarOps.getVarsFrom(_)
            ) ++ localVarStmts.flatMap(stmt =>
              stmt match {
                case LocalVars(modifiers, ty, var_decls) =>
                  var_decls.flatMap(HasVarOps.getVarsFrom(_))
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
                    var_decls.flatMap(HasVarOps.getLVarsFrom(_))
                  case _ => List()
                }
              )
              .toSet
            val otherVars = others.flatMap(HasVarOps.getLVarsFrom(_))
            otherVars.filterNot(localVars)
          }
        }
    }

  implicit def getVarsFromBlockStmt: HasVar[BlockStmt] =
    new HasVar[BlockStmt] {
      override def getVarsFrom(blkStmt: BlockStmt): List[Ident] =
        blkStmt match {
          case BlockStmt_(stmt)       => HasVarOps.getVarsFrom(stmt)
          case LocalClass(class_decl) => List() // TODO:Fixme
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarOps.getVarsFrom(_))
        }
      override def getLVarsFrom(blkStmt: BlockStmt): List[Ident] =
        blkStmt match {
          case BlockStmt_(stmt)       => HasVarOps.getLVarsFrom(stmt)
          case LocalClass(class_decl) => List() // TODO:Fixme
          case LocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarOps.getLVarsFrom(_))
        }
    }

  implicit def getVarsFromStmt: HasVar[Stmt] =
    new HasVar[Stmt] {
      override def getVarsFrom(stmt: Stmt): List[Ident] =
        stmt match {
          case StmtBlock(blk) => HasVarOps.getVarsFrom(blk)
          case IfThen(exp, stmt) =>
            HasVarOps.getVarsFrom(exp) ++ getVarsFrom(stmt)
          case IfThenElse(exp, then_stmt, else_stmt) =>
            HasVarOps.getVarsFrom(exp) ++ getVarsFrom(then_stmt) ++ getVarsFrom(
              else_stmt
            )
          case While(exp, stmt) =>
            HasVarOps.getVarsFrom(exp) ++ getVarsFrom(stmt)
          case BasicFor(init, loop_cond, post_update, stmt) => {
            val s = init.toList.flatMap(HasVarOps.getLVarsFrom(_)).toSet
            val vs = init.toList.flatMap(
              HasVarOps.getVarsFrom(_)
            ) ++ loop_cond.toList.flatMap(
              HasVarOps.getVarsFrom(_)
            ) ++ post_update.toList.flatMap(x =>
              x.flatMap(y => HasVarOps.getVarsFrom(y))
            ) ++ getVarsFrom(stmt)
            vs.filterNot(s)
          }
          case EnhancedFor(modifiers, ty, id, exp, stmt) => {
            HasVarOps.getVarsFrom(exp) ++ getVarsFrom(stmt)
          }
          case Empty        => List()
          case ExpStmt(exp) => HasVarOps.getVarsFrom(exp)
          case Assert(exp, msg) =>
            HasVarOps.getVarsFrom(exp) ++ msg.toList.flatMap(
              HasVarOps.getVarsFrom(_)
            )
          case Switch(exp, blocks) =>
            HasVarOps.getVarsFrom(exp) ++ blocks.flatMap(
              HasVarOps.getVarsFrom(_)
            )
          case Do(stmt, exp) => getVarsFrom(stmt) ++ HasVarOps.getVarsFrom(exp)
          case Break(_)      => List()
          case Continue(_)   => List()
          case Return(exp)   => exp.toList.flatMap(HasVarOps.getVarsFrom(_))
          case Synchronized(exp, blk) =>
            HasVarOps.getVarsFrom(exp) ++ HasVarOps.getVarsFrom(blk)
          case Throw(exp) => HasVarOps.getVarsFrom(exp)
          case Try(try_blk, catches, finally_blk) =>
            HasVarOps.getVarsFrom(try_blk) ++ catches.flatMap(
              HasVarOps.getVarsFrom(_)
            ) ++ finally_blk.toList.flatMap(HasVarOps.getVarsFrom(_))
          case Labeled(id, stmt) => getVarsFrom(stmt)
        }
      override def getLVarsFrom(stmt: Stmt): List[Ident] =
        stmt match {
          case StmtBlock(blk) => HasVarOps.getLVarsFrom(blk)
          case IfThen(exp, stmt) =>
            HasVarOps.getLVarsFrom(exp) ++ getLVarsFrom(stmt)
          case IfThenElse(exp, then_stmt, else_stmt) =>
            HasVarOps.getLVarsFrom(exp) ++ getLVarsFrom(
              then_stmt
            ) ++ getLVarsFrom(else_stmt)
          case While(exp, stmt) =>
            HasVarOps.getLVarsFrom(exp) ++ getLVarsFrom(stmt)
          case BasicFor(init, loop_cond, post_update, stmt) => {
            val s = init.toList.flatMap(HasVarOps.getLVarsFrom(_)).toSet
            val vs = loop_cond.toList.flatMap(
              HasVarOps.getLVarsFrom(_)
            ) ++ post_update.toList.flatMap(x =>
              x.flatMap(y => HasVarOps.getLVarsFrom(y))
            ) ++ getLVarsFrom(stmt)
            vs.filterNot(s)
          }
          case EnhancedFor(modifiers, ty, id, exp, stmt) => {
            HasVarOps.getLVarsFrom(exp) ++ getLVarsFrom(stmt).filterNot(Set(id))
          }
          case Empty        => List()
          case ExpStmt(exp) => HasVarOps.getLVarsFrom(exp)
          case Assert(exp, msg) =>
            HasVarOps.getLVarsFrom(exp) ++ msg.toList.flatMap(
              HasVarOps.getLVarsFrom(_)
            )
          case Switch(exp, blocks) =>
            HasVarOps.getLVarsFrom(exp) ++ blocks.flatMap(
              HasVarOps.getLVarsFrom(_)
            )
          case Do(stmt, exp) =>
            getLVarsFrom(stmt) ++ HasVarOps.getLVarsFrom(exp)
          case Break(_)    => List()
          case Continue(_) => List()
          case Return(exp) => exp.toList.flatMap(HasVarOps.getLVarsFrom(_))
          case Synchronized(exp, blk) =>
            HasVarOps.getLVarsFrom(exp) ++ HasVarOps.getLVarsFrom(blk)
          case Throw(exp) => HasVarOps.getLVarsFrom(exp)
          case Try(try_blk, catches, finally_blk) =>
            HasVarOps.getLVarsFrom(try_blk) ++ catches.flatMap(
              HasVarOps.getLVarsFrom(_)
            ) ++ finally_blk.toList.flatMap(HasVarOps.getLVarsFrom(_))
          case Labeled(id, stmt) => getLVarsFrom(stmt)
        }
    }

  implicit def getVarsFromCatch: HasVar[Catch] =
    new HasVar[Catch] {
      override def getVarsFrom(c: Catch): List[Ident] =
        c match {
          case Catch(params, blk) => {
            val ps = HasVarOps.getVarsFrom(params).toSet
            HasVarOps.getVarsFrom(blk).filterNot(ps)
          }
        }
      override def getLVarsFrom(c: Catch): List[Ident] =
        c match {
          case Catch(params, blk) => {
            val ps = HasVarOps.getVarsFrom(params).toSet
            HasVarOps.getLVarsFrom(blk).filterNot(ps)
          }
        }
    }

  implicit def getVarsFromSwitchBlock: HasVar[SwitchBlock] =
    new HasVar[SwitchBlock] {
      override def getVarsFrom(switch_block: SwitchBlock): List[Ident] =
        switch_block match {
          case SwitchBlock(label, blk_stmts) =>
            HasVarOps.getVarsFrom(label) ++ blk_stmts.flatMap(
              HasVarOps.getVarsFrom(_)
            )
        }
      override def getLVarsFrom(switch_block: SwitchBlock): List[Ident] =
        switch_block match {
          case SwitchBlock(label, blk_stmts) =>
            HasVarOps.getLVarsFrom(label) ++ blk_stmts.flatMap(
              HasVarOps.getLVarsFrom(_)
            )
        }
    }

  implicit def getVarsFromSwitchLabel: HasVar[SwitchLabel] =
    new HasVar[SwitchLabel] {
      override def getVarsFrom(switch_label: SwitchLabel): List[Ident] =
        switch_label match {
          case SwitchCase(exp) => HasVarOps.getVarsFrom(exp)
          case Default         => List()
        }
      override def getLVarsFrom(switch_label: SwitchLabel): List[Ident] =
        switch_label match {
          case SwitchCase(exp) => HasVarOps.getLVarsFrom(exp)
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
            var_decls.flatMap(HasVarOps.getVarsFrom(_))
          case ForInitExps(es) => es.flatMap(HasVarOps.getVarsFrom(_))
        }
      override def getLVarsFrom(for_init: ForInit): List[Ident] =
        for_init match {
          case ForLocalVars(modifiers, ty, var_decls) =>
            var_decls.flatMap(HasVarOps.getLVarsFrom(_))
          case ForInitExps(es) => es.flatMap(HasVarOps.getLVarsFrom(_))
        }
    }

  trait ConvertableToLhs[A] {
    def getLhsFrom(a: A): List[Lhs]
  }

  object ConvertableToLhsOps {
    def getLhsFrom[A](a: A)(implicit hn: ConvertableToLhs[A]): List[Lhs] =
      hn.getLhsFrom(a)
  }

  implicit def expConvertableToLhs: ConvertableToLhs[Exp] =
    new ConvertableToLhs[Exp] {
      // dominating name
      override def getLhsFrom(exp: Exp): List[Lhs] =
        exp match {
          case Lit(lit)        => List()
          case ClassLit(ty)    => List()
          case This            => List()
          case ThisClass(name) => List()
          case InstanceCreation(type_args, type_decl, args, body) => List()
          case QualInstanceCreation(exp, type_args, id, args, body) => List()
          case ArrayCreate(ty, exps, num_dims) => List()
          case ArrayCreateInit(ty, size, init) => List()
          case FieldAccess_(access)    => List(FieldLhs(access))
          case MethodInv(methodInv)    => List()
          case ArrayAccess(idx)        => List(ArrayLhs(idx))
          case ExpName(name)           => List(NameLhs(name))
          case PostIncrement(exp)      => getLhsFrom(exp)
          case PostDecrement(exp)      => getLhsFrom(exp)
          case PreIncrement(exp)       => getLhsFrom(exp)
          case PreDecrement(exp)       => getLhsFrom(exp)
          case PrePlus(exp)            => getLhsFrom(exp)
          case PreMinus(exp)           => getLhsFrom(exp)
          case PreBitCompl(exp)        => getLhsFrom(exp)
          case PreNot(exp)             => getLhsFrom(exp)
          case Cast(ty, exp)           => getLhsFrom(exp)
          case BinOp(e1, op, e2)       => List()
          case InstanceOf(e, ref_type) => List()
          case Cond(cond, true_exp, false_exp) => List()
          case Assign(lhs, op, rhs) => List(lhs)
          case Lambda(params, body) => List() 
          case MethodRef(name, id) => List()
        }
      }
    
}
*/