package obsidian.lang.java


import cats.kernel.Semilattice
import cats._
import cats.implicits._
import cats.data.StateT
import com.github.luzhuomi.scalangj.Syntax._
import obsidian.lang.java.ASTPath._
import scala.collection.immutable
import obsidian.lang.java.ASTUtils._


object MinSSAL {
  case class SSAMethodDecl(
      modifiers: List[Modifier],
      type_params: List[TypeParam],
      ty: Option[Type],
      id: Ident,
      formal_params: List[FormalParam],
      ex_types: List[ExceptionType],
      exp: Option[Exp],
      body: SSAMethodBody
  )

  case class SSAMethodBody(
      blocks: List[SSABlock]
  )

  case class SSABlock(
      label: Label,
      stmts: List[SSAStmt]
  )

  type Label = TCtx 

    sealed trait SSAStmt 
  // to handle nested decl, not in the paper
  case class SSAVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl]) extends SSAStmt

  case class SSAAssert(exp:Exp, msg:Option[Exp]) extends SSAStmt

  case class SSAAssignments(stmts: List[Stmt]) extends SSAStmt

  case class SSAExps(stmts: List[Stmt]) extends SSAStmt
  
  case class SSAReturn(oexp:Option[Exp]) extends SSAStmt

  case class SSAThrow(exp:Exp) extends SSAStmt

  case class SSABreak(tlbl:Label) extends SSAStmt

  case class SSAContinue(tlbl:Label) extends SSAStmt

  // it defers from the paper here, we don't keep a method invocation as a seperate case
  //      it is combined with the assignments and exps cases, as there might be multiple
  //      function calls within a single statement. 
  //      we keep track of the function call and its context in the state
  // case class SSAMethodInvocation(methodInv:MethodInvocation) extends SSAStmt

  case object SSAEmpty extends SSAStmt

  /**
    * 
    *
    * @param tryStmts: try blocks
    * @param phiCatch: Phis before the catch block
    * @param params: parameters for the catch block
    * @param catchStmts: catch blocks
    * @param phiFinally: Phis after the catch block
    */
  case class SSATry(
    tryStmts:List[SSABlock], 
    phiCatch: List[Phi],
    param: FormalParam,
    catchStmts:List[SSABlock],
    phiFinally: List[Phi] 
  ) extends SSAStmt

  
  /**
    * 
    *
    * @param phiEntr: Phis at the entry of the while stmt
    * @param exp: boolean expression
    * @param stmts
    * @param phiExit: Phis at the exit of the while stmt
    */
  case class SSAWhile(
    phiEntr: List[Phi],
    exp: Exp,
    stmts:List[SSABlock],
    phiExit: List[Phi]
  ) extends SSAStmt


  /**
    * 
    * @param exp boolean expression
    * @param thenStmts then blocks
    * @param elseStmts else blocks
    * @param phiExit: Phis at the exit of the while stmt
    */
  case class SSAIf(
    exp:Exp,
    thenStmts:List[SSABlock],
    elseStmts:List[SSABlock],
    phiExit: List[Phi]
  ) extends SSAStmt


  // type Label = ASTPath 

  /**
    * A phi assignment
    *
    * @param srcVar 
    * @param renVar
    * @param rhs
    */
  case class Phi(
    srcVar:Name,
    renVar:Name, 
    rhs:Map[Label, Name]
  )


  /**
    * Source language Context
    * ctx ::= Box | ctx; | ctx; \overline{s} | s; ctx | if e {ctx} else {\overline{s}} | 
    *    if e {\overline{s}} else {ctx}  |  while e {ctx} | try {ctx} catch (T x) (\overline{s}) | 
    *     try {\overline{s}} catch (T x) {ctx}  
    */
  sealed trait SCtx 
  
  case object SBox extends SCtx

  case class SLast(ctx: SCtx) extends SCtx 

  case class SHead(ctx: SCtx) extends SCtx

  case class STail(ctx: SCtx) extends SCtx
  
  case class SThen(ctx: SCtx) extends SCtx

  case class SElse(ctx: SCtx) extends SCtx

  case class SWhile(ctx: SCtx) extends SCtx
  
  case class STry(ctx: SCtx) extends SCtx
  
  case class SCatch(ctx: SCtx) extends SCtx


  def putSCtx(outter:SCtx, inner:SCtx): SCtx = outter match {
    case SBox => inner 
    case SLast(o) => SLast(putSCtx(o, inner))
    case SHead(o) => SHead(putSCtx(o, inner))
    case STail(o) => STail(putSCtx(o, inner))
    case SThen(o) => SThen(putSCtx(o, inner))
    case SElse(o) => SElse(putSCtx(o, inner))
    case SWhile(o) => SWhile(putSCtx(o, inner))
    case STry(o) => STry(putSCtx(o, inner))
    case SCatch(o) => SCatch(putSCtx(o, inner))
  }

  /**
    * Target language context (SSA)
    * CTX ::= Box | CTX; | CTX; \overline{B} | B; CTX | if E {CTX} else {\overline{B}} | 
    *     if E {\overline{B}} else {CTX} join {\overline{\phi}}  | 
    *     if E {CTX} else {\overline{B}} join {\overline{\phi}}  |
    *     if E {\overline{B}} else {\overline{B}} join {BBox}    | 
    *     join {BBox} while E { \overline{B}} join {\overline{\phi}} |  
    *     join {\overline{\phi}} while E { CTX } join {\overline{\phi}} |
    *     join {\overline{\phi}} while E { \overline{B}} join {BBox} | 
    *     try {Ctx} join {\overline{\phi}} catch (T x) {\overline{B}} join {\overline{\phi}} |
    *     try {\overline{B}} join {BBox} catch (T x) {\overline{B}} join {\overline{\phi}} |
    *     try {\overline{B}} join {\overline{\phi}} catch (T x) {CTX} join {\overline{\phi}} | 
    *     try {\overline{B}} join {\overline{\phi}} catch (T x) {\overline{B}} join {BBox}}
    */
  sealed trait TCtx 

  case object TBox extends TCtx

  case class TLast(ctx:TCtx) extends TCtx

  case class THead(ctx:TCtx) extends TCtx

  case class TTail(ctx:TCtx) extends TCtx

  case class TThen(ctx:TCtx) extends TCtx

  case class TElse(ctx:TCtx) extends TCtx

  case object TIfPostPhi extends TCtx

  case class TWhilePrePhi(b:Int) extends TCtx

  case class TWhile(ctx:TCtx) extends TCtx

  case object TWhilePostPhi extends TCtx

  case class TTry(ctx:TCtx) extends TCtx

  case object TTryPeriPhi extends TCtx

  case class TCatch(ctx:TCtx) extends TCtx

  case object TTryPostPhi extends TCtx 

  

  def putTCtx(outter:TCtx, inner:TCtx): TCtx = outter match {
    case TBox => inner 
    case TLast(o) => TLast(putTCtx(o, inner))
    case THead(o) => THead(putTCtx(o, inner))
    case TTail(o) => TTail(putTCtx(o, inner))
    case TThen(o) => TThen(putTCtx(o, inner))
    case TElse(o) => TElse(putTCtx(o, inner))
    case TWhile(o) => TWhile(putTCtx(o, inner))
    case TTry(o) => TTry(putTCtx(o, inner))
    case TCatch(o) => TCatch(putTCtx(o, inner))
    case _ => outter
  }


  // variable mapping 
  // old and wrong
  // type VarMap = Map[Name, Map[SCtx, (TCtx, Name)]]
  type VarMap = Map[Name, Map[TCtx, (SCtx, Name)]]
  
  def unionVarMap(vm1:VarMap, vm2:VarMap):VarMap = vm2.toList.foldLeft(vm1)( (vm, kv) => kv match {
    case (name, m) => vm.get(name) match {
      case None => vm + (name -> m)
      case Some(m2) => vm + (name -> (m ++ m2))
    }
  })

  def listToVarMap(l:List[(Name, (TCtx, (SCtx, Name)))]):VarMap = l.foldLeft(Map():VarMap)( (vm, kv) => kv match {
    case (name, (tctx, (sctx, tname))) => vm.get(name) match {
      case None => vm + (name -> Map(tctx -> (sctx, tname)))
      case Some(m2) => m2.get(tctx) match {
        case None => vm + (name ->  (m2 + (tctx -> (sctx, tname))))
        case Some((sctx1, name1)) => vm // duplicate?
      }
    }
  })


  /**
  * A state object for the conversion function
  *
  * @param varMap - the variable mapping
  * @param exitCtx - the exit context from the last block
  * @param eenv - the list of contexts that throw exception
  * @param benv - the list of contexts that contains a break statement 
  * @param cenv - the list of contexts that contains a continue statement
  * @param nestedDecls - the list of nested declared variables
  * @param methodInvs - the list of method invocations
  */
  case class State(
    varMap: VarMap, 
    exitCtx: TCtx,
    aenv: AEnv, // all non phi context so far
    eenv: EEnv, 
    benv: BEnv, // break context, breakee context
    cenv: CEnv, // continue context, continuee context
    nestedDecls: List[(TCtx, Ident, Type, List[Modifier])],
    methodInvs: List[(TCtx, MethodInvocation)],
    srcLabelEnv: Map[Ident, SCtx]
  )

  val initState:State = State(Map(), TBox, List(), List(), List(), List(), List(), List(), Map())

  def eenvFromState(st:State):EEnv = st match {
    case State(_, _, aenv, eenv, _, _, _, _,_ ) => eenv
  }

  def benvFromState(st:State):BEnv = st match {
    case State(_, _, aenv, _, benv, _, _, _, _) => benv
  }

  def cenvFromState(st:State):CEnv = st match {
    case State(_, _, aenv, _, _, cenv, _, _, _) => cenv
  }

  def eCtxFromState(st:State):TCtx = st match {
    case State(_,ectx, aenv, _, _, _, _, _, _) => ectx
  }

  def srcLabelEnvFromState(st:State):Map[Ident,SCtx] = st match {
    case State(_,ectx, aenv, _, _, _, _, _, srcLblEnv) => srcLblEnv
  }

  type AEnv = List[TCtx]
  type EEnv = List[TCtx] 
  type BEnv = List[(TCtx, Option[TCtx])] // when it is None, it means out of the current lexical scope, do we need to keep track of the list of "negative" ctx constructors? 
  type CEnv = List[(TCtx, Option[TCtx])] 


  type ErrorM = String


  sealed trait SSAResult[+A]

  case class SSAError(msg:ErrorM) extends SSAResult[Nothing]
  
  case class SSAOk[A](result:A) extends SSAResult[A]

  implicit def ssaResultFunctor: Functor[SSAResult] =
    new Functor[SSAResult] {
      override def map[A, B](fa: SSAResult[A])(f: A => B): SSAResult[B] =
        fa match {
          case SSAError(s) => SSAError(s)
          case SSAOk(a) => SSAOk(f(a))
        }
    }

  implicit def ssaResultApplicative: ApplicativeError[SSAResult, ErrorM] = 
    new ApplicativeError[SSAResult, ErrorM] {
      override def ap[A, B](ff: SSAResult[A => B])(fa: SSAResult[A]): SSAResult[B] =
        ff match {
          case SSAOk(f) =>
            fa match {
              case SSAOk(a) => SSAOk(f(a))
              case SSAError(s) => SSAError(s)
            }
          case SSAError(s) => SSAError(s)
        }

      override def pure[A](a: A): SSAResult[A] = SSAOk(a)

      override def raiseError[A](e: ErrorM): SSAResult[A] = SSAError(e)

      override def handleErrorWith[A](fa: SSAResult[A])(f: ErrorM => SSAResult[A]): SSAResult[A] =
        fa match {
          case SSAError(s) => f(s)
          case SSAOk(a) => SSAOk(a)
        }
    }

  implicit def ssaResultMonadError(implicit app:ApplicativeError[SSAResult, ErrorM]):MonadError[SSAResult, ErrorM] = {
    new MonadError[SSAResult, ErrorM] {
      override def raiseError[A](e: ErrorM): SSAResult[A] = app.raiseError(e)

      override def handleErrorWith[A](fa: SSAResult[A])(f: ErrorM => SSAResult[A]): SSAResult[A] = app.handleErrorWith(fa)(f)

      override def flatMap[A, B](fa: SSAResult[A])(f: A => SSAResult[B]): SSAResult[B] =
        fa match {
          case SSAOk(a) => f(a)
          case SSAError(s) => SSAError(s)
        }

      override def pure[A](a: A): SSAResult[A] = app.pure(a)

      @annotation.tailrec
      def tailRecM[A, B](init: A)(fn: A => SSAResult[Either[A, B]]): SSAResult[B] =
        fn(init) match {
          case SSAError(msg) => SSAError(msg)
          case SSAOk(Right(b)) => SSAOk(b)
          case SSAOk(Left(a)) => tailRecM(a)(fn)
        }
    }
  }

  type SState[S,A] = StateT[SSAResult, S, A]
  type SSAState[A] = SState[State, A]


  def get:SState[State, State] = StateT { state => SSAOk((state, state))} 

  def put(st:State):SState[State, Unit] = StateT { _ => SSAOk((st,()))} 

  /**
    * setECtx - setting the exiting context in the state
    *
    * @param tctx
    * @param m
    * @return
    */
  def setECtx(tctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(vm, eCtx, aenv, eenv, benv, cenv, nestedDecls, methInvs, srcLabelEnv) => State(vm, tctx, aenv, eenv, benv, cenv, nestedDecls, methInvs, srcLabelEnv)
    })
    _   <- put(st1)
  } yield ()


  /**
    * setVM - set the given VarMap in the state
    *
    * @param vm
    * @param m
    * @return
    */
  def setVM(vm:VarMap)(implicit m:MonadError[SSAState, ErrorM]): SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(_,eCtx, aenv, eenv, benv, cenv, nestedDecls, methInvs, srcLabelEnv) => State(vm, eCtx, aenv, eenv, benv, cenv, nestedDecls, methInvs,srcLabelEnv)
    })
    _   <- put(st1)
  } yield ()
  
  def removeVarFromVM(v:Name)(implicit m:MonadError[SSAState, ErrorM]): SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(vm ,eCtx, aenv, eenv, benv, cenv, nestedDecls, methInvs, srcLabelEnv) => State(vm - v, eCtx, aenv, eenv, benv, cenv, nestedDecls, methInvs,srcLabelEnv)
    })
    _ <- put(st1)
  } yield ()

  /**
    * addNestedVarDecls - add an entry to the nested var decls in the state
    *
    * @param tctx
    * @param id
    * @param ty
    * @param mods
    * @param m
    * @return
    */
  def addNestedVarDecls(tctx:TCtx, id:Ident, ty:Type, mods:List[Modifier])(implicit m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1  <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val nDecls1 = (nDecls.toSet + ((tctx, id, ty, mods))).toList
        State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls1, methInvs,srcLblEnv)
      }
    })
    _  <- put(st1)
  } yield ()

  /**
    * addMethodInv - add an entry of the method invocatoin in the state
    *
    * @param tctx
    * @param methinv
    * @param m
    * @return
    */
  def addMethodInv(tctx:TCtx, methinv:MethodInvocation)(implicit m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val methInvs1 = (methInvs.toSet + ((tctx, methinv))).toList
        State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs1, srcLblEnv)
      }
    })
    _  <- put(st1)
  } yield ()

  def addSrcLabel(label:Ident, ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val srcLblEnv1 = srcLblEnv + (label -> ctx) 
        State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv1)
      }
    })
  } yield ()


  /**
    * addAEnv - add an context to the list of all program context env
    *
    * @param tctx 
    * @param m
    * @return
    */
  def addAEnv(tctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val aenv1 = (aenv.toSet + tctx).toList
        State(varMap, eCtx, aenv1, eenv, benv, cenv, nDecls, methInvs, srcLblEnv)
      }
    })
    _  <- put(st1)
  } yield ()


  /**
    * addEEv - add the given context to the list of throwing context in the state
    *
    * @param tctx
    * @param m
    * @return
  */
  

  def addEEnv(tctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val eenv1 = (eenv.toSet + tctx).toList
        State(varMap, eCtx, aenv, eenv1, benv, cenv, nDecls, methInvs, srcLblEnv)
      }
    })
    _  <- put(st1)
  } yield ()

  /**
    * addBEnv - and a pair of target contexts into the break environment
    *
    * @param bctx - context where the break statement is 
    * @param tctx - context where the while/switch statement that the break statement is targeting at
    * @param m
    * @return
    */

  def addBEnv(bctx:TCtx, tctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val benv1 = (benv.toSet + ((bctx,Some(tctx)))).toList
        State(varMap, eCtx, aenv, eenv, benv1, cenv, nDecls, methInvs, srcLblEnv)
      }
    })
    _  <- put(st1)
  } yield ()

  /**
    * addCEnv - and a pair of target contexts into the continue environment
    *
    * @param bctx - context where the continue statement is 
    * @param tctx - context where the while statement that the continue statement is targeting at
    * @param m
    * @return
    */


  def addCEnv(bctx:TCtx, tctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => {
        val cenv1 = (cenv.toSet + ((bctx,Some(tctx)))).toList
        State(varMap, eCtx, aenv, eenv, benv, cenv1, nDecls, methInvs, srcLblEnv)
      }
    })
    _  <- put(st1)
  } yield ()




  def mergeState(st1:State, st2:State, st3:State):State = {
    val st12 = mergeState(st1, st2)
    mergeState(st12, st3)  
  }
  

  /** 
   * mergeState - merge two states by taking the vm and ectx from st1,
   * and union the eenv and nDecls
   * */
  def mergeState(st1:State, st2:State):State = (st1, st2) match {
    case (State(vm1, eCtx1, aenv1, eenv1, benv1, cenv1, nDecls1, methInvs1, srcLblEnv1), State(vm2, eCtx2, aenv2, eenv2, benv2, cenv2, nDecls2, methInvs2, srcLblEnv2)) => 
      State(unionVarMap(vm1, vm2), eCtx1, (aenv1++aenv2).toSet.toList, (eenv1++eenv2).toSet.toList, (benv1++benv2).toSet.toList, (cenv1++cenv2).toSet.toList, (nDecls1 ++ nDecls2).toSet.toList, (methInvs1 ++ methInvs2).toSet.toList, (srcLblEnv1 ++ srcLblEnv2))
  }

  

  def extendAllVarsWithContextAndLabel(sctx:SCtx, tctx:TCtx, lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1 <- st match {
      case State(vm0, eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv) => for {
        entries <- vm0.keySet.toList.traverse(v => for {
          v_lbl <- mkName(v, lbl)
        } yield (v, tctx, sctx, v_lbl ))
      } yield State(entries.foldLeft(vm0)((vm, ent) => ent match {
        case (v, tctx, sctx, v_lbl) => vm.get(v) match {
          case None => vm
          case Some(m) => vm + (v -> (m + (tctx -> (sctx, v_lbl))))
        }
      }), eCtx, aenv, eenv, benv, cenv, nDecls, methInvs, srcLblEnv)
    }
    _ <- put(st1)
  } yield ()



  /**
  * mkName - create a new name from an existing name and a label.
  *
  * @param n
  * @param lbl
  * @param m
  * @return
  */
  def mkName(n:Name, lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Name] = n match 
    {
      case Name(Nil) => m.raiseError("SSA construction failed, mkName is applied to an empty name.")
      case Name(ids) => {
        val pre = ids.init
        val x   = ids.last
        val s   = lblToStr(lbl)
        val y   = appIdStr(x, s)
        m.pure(Name(pre++List(y)))
      }
    }

  def lblToStr(lbl:Label):String = lbl.toString()


  implicit val eqTCtx:Eq[TCtx] = new Eq[TCtx]{
    override def eqv(x: TCtx, y: TCtx): Boolean = (x,y) match {
      case (TBox, TBox) => true
      case (TLast(ctx1), TLast(ctx2)) => eqv(ctx1, ctx2)
      case (THead(ctx1), THead(ctx2)) => eqv(ctx1, ctx2)
      case (TTail(ctx1), TTail(ctx2)) => eqv(ctx1, ctx2)
      case (TThen(ctx1), TThen(ctx2)) => eqv(ctx1, ctx2)
      case (TElse(ctx1), TElse(ctx2)) => eqv(ctx1, ctx2)
      case (TIfPostPhi, TIfPostPhi) => true
      case (TWhilePrePhi(b1), TWhilePrePhi(b2)) => b1 == b2
      case (TWhile(ctx1), TWhile(ctx2)) => eqv(ctx1, ctx2)
      case (TWhilePostPhi, TWhilePostPhi) => true
      case (TTry(ctx1), TTry(ctx2)) => eqv(ctx1, ctx2)
      case (TTryPeriPhi, TTryPeriPhi) => true
      case (TCatch(ctx1), TCatch(ctx2)) => eqv(ctx1, ctx2)
      case (TTryPostPhi, TTryPostPhi) => true
      case (_,_) => false 
    }
  }


  // ****************************** implementing isLast start *********************************************************************
  // check whether a context is the last of a sequence, w.r.t. to the list all program contexts in the same immediate lexical scope

  def isLast(tctx:TCtx, aenv:AEnv):Boolean = follow(tctx,aenv) match {
    case None => true 
    case Some(_) => false
  }


  def ifElseEnv(aenv:AEnv):Boolean = aenv match {
    case (TThen(_)) :: tl => true 
    case (TElse(_)) :: tl => true
    case (TIfPostPhi :: tl ) => true
    case _ => false
  }

  def whileEnv(aenv:AEnv):Boolean = aenv match {
    case (TWhile(_)) :: tl => true
    case TWhilePostPhi :: tl => true 
    case TWhilePrePhi(_) :: tl => true
    case _ => false 
  }

  def tryEnv(aenv:AEnv):Boolean = aenv match {
    case TTry(_) :: tl => true
    case TTryPeriPhi :: tl => true
    case TTryPostPhi :: tl => true
    case TCatch(_) :: tl => true 
    case _ => false
  }

  def seqEnv(aenv:AEnv):Boolean = aenv match {
    case THead(_) :: tl => true
    case TTail(_) :: tl => true 
    case _ => false
  }

  def lastEnv(aenv:AEnv):Boolean = aenv match {
    case TLast(_) :: tl => true
    case _ => false
  }


  // isLast(c) == true iff follow(c) == None
  /** follow - get the following program context 
   *
   * follow is only called when tctx is not in eenv ++ dom(benv) ++ dom(cenv)
   */
  
  def follow(tctx:TCtx, aenv:AEnv):Option[TCtx] = tctx match {
    case TBox if ifElseEnv(aenv) => Some(TIfPostPhi)
    case TBox if whileEnv(aenv) => Some(TWhilePostPhi)
    case TBox if tryEnv(aenv) => Some(TTryPostPhi)
    case TBox if seqEnv(aenv) => Some(TTail(TBox))
    case TBox if aenv.length > 0 => Some(TLast(TBox))
    case TBox => None
    case TLast(c) => {
      val daenv = appDec(unTLast, aenv)         
      follow(c,daenv) match {
        case Some(n) => Some(TLast(n))
        case None => None
      }
    }
    case THead(c) => Some(TTail(TBox)) // fast-forward to the tail without stepping through c
    case TTail(c) => {
      val daenv = appDec(unTTail, aenv)
      follow(c, daenv) match {
        case Some(n) => Some(TTail(n))
        case None => None
      }
    }
    case TThen(c) => {
      val daenv = appDec(unTThen, aenv)
      follow(c, daenv) match  {
        case Some(n) => Some(TThen(n))
        case None => Some(TIfPostPhi)
      }
    }
    case TElse(c) => {
      val daenv = appDec(unTElse, aenv)
      follow(c, daenv) match {
        case Some(n) => Some(TElse(n))
        case None => Some(TIfPostPhi)
      }
    }
    case TIfPostPhi => None
    case TTry(c) => {
      val daenv = appDec(unTTry, aenv)
      follow(c, daenv) match {
        case Some(n) => Some(TTry(n))
        case None => Some(TTryPostPhi)
      }
    }
    case TTryPeriPhi => follow(TCatch(TBox), aenv)
    
    case TCatch(c) => {
      val daenv = appDec(unTCatch, aenv)
      follow(c, daenv) match {
        case Some(n) => Some(TCatch(n))
        case None => Some(TTryPostPhi)
      }
    }
    case TTryPostPhi => None
    case TWhile(c) => {
      val daenv = appDec(unTCatch, aenv) 
      follow(c, daenv) match {
        case Some(n) => Some(TWhile(n))
        case None => Some(TWhilePostPhi) // there is no statement between TWhilePrePhi and TWhilePostPhi 
      }
    }
    case TWhilePrePhi(_) => Some(TWhilePostPhi)
    case TWhilePostPhi => None
    
  } 


  // list of extractors


  def unTHead(tctx:TCtx):Option[TCtx] = tctx match {
    case THead(c) => Some(c)
    case _        => None
  }

  def unTLast(tctx:TCtx):Option[TCtx] = tctx match {
    case TLast(c) => Some(c) 
    case _        => None
  }

  def unTTail(tctx:TCtx):Option[TCtx] = tctx match {
    case TTail(c) => Some(c)
    case _        => None
  }

  def unTThen(tctx:TCtx):Option[TCtx] = tctx match {
    case TThen(c) => Some(c)
    case _        => None
  } 
   
  def unTElse(tctx:TCtx):Option[TCtx] = tctx match {
    case TElse(c) => Some(c)
    case _        => None
  }

  def unTWhile(tctx:TCtx):Option[TCtx] = tctx match {
    case TWhile(c) => Some(c)
    case _         => None
  }

  def unTTry(tctx:TCtx):Option[TCtx] = tctx match {
    case TTry(c) => Some(c) 
    case _       => None
  }

  def unTCatch(tctx:TCtx):Option[TCtx] = tctx match {
    case TCatch(c) => Some(c)
    case _         => None
  }

  def appDec(dec:TCtx => Option[TCtx], ts:List[TCtx]):List[TCtx] = ts.map(dec(_)).filter( x => !x.isEmpty).flatMap({
    case Some(c) => List(c)
    case None => Nil
  })

  def appDec2(dec:TCtx => Option[TCtx], ts:BEnv):BEnv = ts.flatMap({ 
    case (c1,Some(c2)) => (dec(c1), dec(c2)) match {
      case (Some(c3), Some(c4)) => List((c3,Some(c4)))
      case (Some(c3), None) => List((c3,None))
      case (None, _) => Nil
    }
    case (c1, None) => dec(c1) match {
      case Some(c3) => List((c3, None))
      case None => Nil
    }
  })

  // ****************************** implementing isLast end *********************************************************************



  // ****************************** implementing partial order begin ************************************************************
  // return the domain of a mapping

  def dom[A,B](m:List[(A,B)]):List[A] = m.map{
    case (a,b) => a
  }


  implicit def partialOrderTCtx(aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv):PartialOrder[TCtx] = new PartialOrder[TCtx]{
    override def partialCompare(x: TCtx, y: TCtx): Double = 
    { 
      (x,y) match {
        case (_,_) if (eqTCtx.eqv(x,y)) => 0.0
        // CtxOrdHole
        case (TBox, _) => -1.0 
        case (_, TBox) => 1.0

        // CtxOrdInd  specialized for Last
        case (TLast(ctx1), TLast(ctx2)) => {
          val daenv = appDec(unTLast, aenv)
          val deenv = appDec(unTLast, eenv)
          val dbenv = appDec2(unTLast, benv) 
          val dcenv = appDec2(unTLast, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        }

        // CtxOrdInd specialized for Head
        case (THead(ctx1), THead(ctx2)) => {
          val daenv = appDec(unTHead, aenv)
          val deenv = appDec(unTHead, eenv)
          val dbenv = appDec2(unTHead, benv) 
          val dcenv = appDec2(unTHead, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        }

        // CtxOrdSeq
        case (THead(_), TTail(_)) if !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => -1.0

        // CtxOrdInd specialized for TTail
        case (TTail(ctx1), TTail(ctx2)) =>  {
          val daenv = appDec(unTTail, aenv)
          val deenv = appDec(unTTail, eenv)
          val dbenv = appDec2(unTTail, benv) 
          val dcenv = appDec2(unTTail, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        }

        // CtxOrdSeq - dual 
        case (TTail(_), THead(_))  => -partialCompare(y,x) 

        // CtxOrdInd specialized for TThen
        case (TThen(ctx1), TThen(ctx2)) => {
          val daenv = appDec(unTThen, aenv)
          val deenv = appDec(unTThen, eenv)
          val dbenv = appDec2(unTThen, benv) 
          val dcenv = appDec2(unTThen, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        }
        
        // CtxOrdThen 
        case (TThen(c), TIfPostPhi) if isLast(c, appDec(unTThen, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => -1.0
        case (TThen(c), TIfPostPhi) if isLast(c, appDec(unTThen, aenv)) && ((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => Double.NaN
        // if not last, we need to apply the transtivity
        case (TThen(c), TIfPostPhi) if !isLast(c, appDec(unTThen, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => follow(c, appDec(unTThen, aenv)) match {
          case Some(n) => partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(TThen(n), TIfPostPhi)
          case None  => Double.NaN
        }
        case (TThen(c), TIfPostPhi) if !isLast(c, appDec(unTThen, aenv)) && ((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => Double.NaN

        // CtxOrdInd specialized for TElse
        case (TElse(ctx1), TElse(ctx2)) => {
          val daenv = appDec(unTElse, aenv)
          val deenv = appDec(unTElse, eenv)
          val dbenv = appDec2(unTElse, benv) 
          val dcenv = appDec2(unTElse, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        }
        
        // CtxOrdElse 
        case (TElse(c), TIfPostPhi) if isLast(c, appDec(unTElse, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => -1.0
        case (TElse(c), TIfPostPhi) if isLast(c, appDec(unTElse, aenv)) && ((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => Double.NaN
        // if not last, we need to apply the transtivity until we find the last
        case (TElse(c), TIfPostPhi) if !isLast(c, appDec(unTElse, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => follow(c, appDec(unTElse, aenv)) match {
          case Some(n) => partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(TElse(n), TIfPostPhi)
          case None  => Double.NaN
        }
        case (TElse(c), TIfPostPhi) if !isLast(c, appDec(unTElse, aenv)) && ((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => Double.NaN

        // CtxOrdThen - dual 
        case (TIfPostPhi, TThen(c)) => -partialCompare(y,x) 
        // CtxOrdElse - dual 
        case (TIfPostPhi, TElse(c)) => -partialCompare(y,x) 

        // CtxOrdWhileEntry1 
        case (TWhilePrePhi(0), TWhile(_)) => -1.0 // _ or Box? todo: check!!

        // CtxOrdWhileExit2
        case (TWhilePrePhi(_), TWhilePostPhi) => -1.0

        // CtxOrdWhileEntry2
        case (TWhile(c), TWhilePrePhi(1)) if isLast(c, appDec(unTWhile, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => -1.0
        case (TWhile(c), TWhilePrePhi(1)) if isLast(c, appDec(unTWhile, aenv)) && (eenv.contains(x)) => Double.NaN
        case (TWhile(c), TWhilePrePhi(1)) if isLast(c, appDec(unTWhile, aenv)) && (dom(cenv.filter({case ((_,ctx)) => ctx == Some(TBox)})).contains(x)) => -1.0 // CtxOrdWhileEntry3
        case (TWhile(c), TWhilePrePhi(1)) if isLast(c, appDec(unTWhile, aenv)) => Double.NaN   
        // if not last, we need to apply the transtivity
        case (TWhile(c), TWhilePrePhi(1)) if !isLast(c, appDec(unTWhile, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => follow(c, appDec(unTWhile, aenv)) match {
          case Some(n) => partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(TWhile(n), TWhilePrePhi(1))
          case None  => Double.NaN
        }
        case (TWhile(c), TWhilePrePhi(1)) if !isLast(c, appDec(unTWhile, aenv)) && (eenv.contains(x)) => Double.NaN // is this possible?
        case (TWhile(c), TWhilePrePhi(1)) if !isLast(c, appDec(unTWhile, aenv)) && (dom(cenv.filter({case ((_,ctx)) => ctx == Some(TBox)})).contains(x)) => -1.0 // CtxOrdWhileEntry3, is this possible?
        case (TWhile(c), TWhilePrePhi(1)) if !isLast(c, appDec(unTWhile, aenv)) => Double.NaN // is this possible?


        // CtxOrdInd specialized for TWhile
        case (TWhile(ctx1), TWhile(ctx2)) => {
          val daenv = appDec(unTWhile, aenv)
          val deenv = appDec(unTWhile, eenv)
          val dbenv = appDec2(unTWhile, benv) 
          val dcenv = appDec2(unTWhile, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        }



        case (TWhile(c), TWhilePostPhi) if eenv.contains(x) => Double.NaN 
        case (TWhile(c), TWhilePostPhi) if dom((benv ++ cenv).filter({case ((_,ctx)) => ctx != Some(TBox)})).contains(x) => Double.NaN 
        case (TWhile(c), TWhilePostPhi) => -1.0 // (CtxOrdWhileExit1) and (CtxOrdWhileEntry2) (CtxOrdWhileEntry3) with transivitity 
        // CtxOrdWhileExit2 - dual
        case (TWhilePostPhi, TWhilePrePhi(_)) => -partialCompare(y,x) 
        case (TWhilePostPhi, TWhile(c)) => -partialCompare(y,x) 



        // CtxOrdInd specialized for TTry
        case (TTry(ctx1), TTry(ctx2)) => {
          val daenv = appDec(unTTry, aenv)
          val deenv = appDec(unTTry, eenv)
          val dbenv = appDec2(unTTry, benv) 
          val dcenv = appDec2(unTTry, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        } 
        case (TTry(c), TTryPeriPhi) if eenv.contains(x) => -1.0 // (CtxOrdTry1)

        // apply transtivity until we can fire (CtxOrdTry1) or fail at the last 
        case (TTry(c), TTryPeriPhi) if !isLast(c, appDec(unTTry, aenv)) && !((dom(benv) ++ dom(cenv)).contains(x)) => follow(c, appDec(unTTry, aenv)) match {
          case Some(n) => partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(TTry(n), TTryPeriPhi)
          case None  => Double.NaN
        }
        // c must be the last
        case (TTry(c), TTryPeriPhi) => Double.NaN

        case (TTry(c1), TCatch(c2)) if eenv.contains(x) => -1.0 // (CtxOrdTry1) and (CtxOrdCatch1) with transitivity
        // apply transtivity until we can fire previous case or fail at the last 
        case (TTry(c1), TCatch(c2)) if !isLast(c1, appDec(unTTry, aenv)) && !((dom(benv) ++ dom(cenv)).contains(x)) => follow(c1, appDec(unTTry, aenv)) match {
          case Some(n1) => partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(TTry(n1), TCatch(c2))
          case None => Double.NaN
        }
        // c1 must be the last
        case (TTry(c1), TCatch(c2)) => Double.NaN 

        // (CtxOrdTry2), we don't check x is contained in eenv, because even if it is in eenv, we apply transtivity to get the same result
        case (TTry(c), TTryPostPhi) if !dom(benv ++ cenv).contains(x) => -1.0
        case (TTry(c), TTryPostPhi) => Double.NaN

        // dual of the above
        case (TTryPeriPhi, TTry(_)) => -partialCompare(y,x)

        // (CtxOrdCatch1) 
        case (TTryPeriPhi, TCatch(_)) => -1.0
        // (CtxOrdCatch1) and transivitiy, no throw in the catch block
        case (TTryPeriPhi, TTryPostPhi) => partialCompare(TCatch(TBox), TTryPostPhi) // we still need to step through the catch block to ensure no break or continue

        // dual of the above
        case (TCatch(_), TTry(_)) => -partialCompare(y,x)
        case (TCatch(_), TTryPeriPhi) => -partialCompare(y,x)

        // CtxOrdInd specialized for TCatch

        case (TCatch(ctx1), TCatch(ctx2)) => {
          val daenv = appDec(unTCatch, aenv)
          val deenv = appDec(unTCatch, eenv)
          val dbenv = appDec2(unTCatch, benv) 
          val dcenv = appDec2(unTCatch, cenv)
          partialOrderTCtx(daenv, deenv, dbenv, dcenv).partialCompare(ctx1,ctx2)
        } 
        case (TCatch(c), TTryPostPhi) if isLast(c, appDec(unTCatch, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => -1.0 // (CtxOrdCatch2)
        // apply transtivity until we can fire previous case or fail at the last 
        case (TCatch(c), TTryPostPhi) if !isLast(c, appDec(unTCatch, aenv)) && !((eenv ++ dom(benv) ++ dom(cenv)).contains(x)) => follow(c, appDec(unTCatch, aenv)) match {
          case Some(n) => partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(TCatch(n), TTryPostPhi)
          case None => Double.NaN
        }
        case (TCatch(c), TTryPostPhi) => Double.NaN
        // dual of the above
        case (TTryPostPhi, TTry(_)) => -partialCompare(y,x)
        case (TTryPostPhi, TTryPeriPhi) => -partialCompare(y,x)
        case (TTryPostPhi, TCatch(_)) => -partialCompare(y,x)

        case _ => Double.NaN
      }
    }
  }



  // ****************************** implementing partial order end ************************************************************


  /**
  * combine - return the lub from the set
  *
  * @param cs
  * @return
  */
  def combine[A](cs:List[(TCtx,A)], aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv) :List[(TCtx,A)] = cs match {
    case Nil => Nil
    case x::Nil => x::Nil
    case (x::xs) => {
      val ys = xs.filter(  y => !(partialOrderTCtx(aenv,eenv,benv, cenv).partialCompare(y._1,x._1) == -1.0))
      if (ys.exists( y => partialOrderTCtx(aenv,eenv,benv,cenv).partialCompare(x._1,y._1) == -1.0))
      { 
        combine(ys, aenv, eenv, benv, cenv) 
      }
      else {
        x::combine(ys, aenv, eenv, benv, cenv)
      }
      
    }
  } 

  def Rlt(aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv, ctx:TCtx, vm:VarMap, x:Name):List[(TCtx,Name)] = R(aenv, eenv, benv, cenv, ctx, vm, x, 
    {
      case ((tctx1, tctx2))  => (partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(tctx1, tctx2) == -1.0)
    }
  ) 

  def Rleq(aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv, ctx:TCtx, vm:VarMap, x:Name):List[(TCtx,Name)] = R(aenv, eenv, benv, cenv, ctx, vm, x, 
    {
      case ((tctx1, tctx2))  => { 
        val pot = partialOrderTCtx(aenv, eenv, benv, cenv)
        ((pot.partialCompare(tctx1, tctx2) == -1.0) || (pot.partialCompare(tctx1, tctx2) == 0.0))
      }
    })

  def RleqBounded(aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv, lctx:TCtx, uctx:TCtx, vm:VarMap, x:Name):List[(TCtx,Name)] = RBounded(aenv, eenv, benv, cenv, lctx, uctx, vm, x,
    {
      case ((tctx1, tctx2))  => (partialOrderTCtx(aenv, eenv, benv, cenv).partialCompare(tctx1, tctx2) == -1.0)
    }, 
    {
      case ((tctx1, tctx2))  => { 
        val pot = partialOrderTCtx(aenv, eenv, benv, cenv)
        ((pot.partialCompare(tctx1, tctx2) == -1.0) || (pot.partialCompare(tctx1, tctx2) == 0.0))
      }
    })
  /**
    * Compute the name from the lub of all the reachable context until ctx
    *  it defers from the paper, which takes in a default value, we return None in case the set of contexts an empty set. The defaulting should be handled at the call site.
    *
    * @param eenv - exception throwing program contexts
    * @param benv - break statement contexts
    * @param cenv - continue statement contexts
    * @param ctx
    * @param vm
    * @param x
    * @param cmp - modifier to switch between leq or lt
    * @return - return the name of the variable that is the most recent dominator of x
    */
  def R(aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv, ctx:TCtx, vm:VarMap, x:Name, cmp:(TCtx,TCtx) => Boolean):List[(TCtx,Name)] = vm.get(x) match { // perhaps we should report the error properly
    case None => Nil
    case Some(trs) => {
      val tcvs = for { 
        (tctx, (sctx, tx)) <- trs.toList
        if (cmp(tctx, ctx))
      } yield (tctx, tx)

      /* not using the builtin semilattice class
      // partial function, but lub should be in the set.
      def comb(px:(TCtx,Name), py:(TCtx,Name)):(TCtx, Name) = (px, py) match {
        case ((cx, vx), (cy, vy)) if (semilatticeTCtx(aenv, eenv, benv, cenv).combine(cx, cy) == cx) => (cx, vx)
        case ((cx, vx), (cy, vy)) if (semilatticeTCtx(aenv, eenv, benv, cenv).combine(cx, cy) == cy) => (cy, vy)
        
      }

      tcvs match {
        case Nil => None
        case (tcv::tcvs) => tcvs.foldLeft(tcv)((x,y) => comb(x,y)) match {
          case (_, vx) => Some(vx)
        }
      }      
      */
      combine(tcvs, aenv,eenv, benv,cenv) 
    }
  }

  // a variant of the R, with lower bound and upper bound, instead of just having an upper bound
  def RBounded(aenv:AEnv, eenv:EEnv, benv:BEnv, cenv:CEnv, lctx:TCtx, uctx:TCtx, vm:VarMap, x:Name, lcmp:(TCtx,TCtx) => Boolean, ucmp:(TCtx,TCtx) => Boolean):List[(TCtx,Name)] = vm.get(x) match {
    case None => Nil
    case Some(trs) => {
      val tcvs = for {
        (tctx, (sctx, tx)) <- trs.toList
        if ((lcmp(lctx, tctx)) && (ucmp(tctx, uctx)))
      } yield (tctx, tx)
      combine(tcvs, aenv,eenv, benv,cenv) 
    }
  } 



}
