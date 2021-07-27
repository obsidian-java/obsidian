package com.github.luzhuomi.obsidian


import cats.kernel.Semilattice
import cats._
import cats.implicits._
import cats.data.StateT
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.obsidian.ASTPath._
import scala.collection.immutable
import com.github.luzhuomi.obsidian.ASTUtils._

// Kenny's version of SSA
/**
  * Assumption: nested declaration contains no repetitive variable name
  *   source program is desguared, flattened and label.
  */

object SSAKL {
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
    stmt: SSAStmt
  )

  sealed trait SSAStmt 
  // to handle nested decl, not in the paper
  case class SSAVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl]) extends SSAStmt

  case class SSAAssert(exp:Exp, msg:Option[Exp]) extends SSAStmt

  case class SSAAssignments(stmts: List[Stmt]) extends SSAStmt

  case class SSAExps(stmts: List[Stmt]) extends SSAStmt
  
  case class SSAReturn(oexp:Option[Exp]) extends SSAStmt

  case class SSAThrow(exp:Exp) extends SSAStmt

  case class SSAMethodInvocation(stmt:Stmt) extends SSAStmt

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
    * @param stmt
    * @param phiEntr: Phis at the entry of the while stmt
    * @param phiExit: Phis at the exit of the while stmt
    */
  case class SSAWhile(
    stmt:Stmt,
    phiEntr: List[Phi],
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

  case object TWhilePrePhi extends TCtx

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



  type VarMap = Map[Name, Map[SCtx, (TCtx, Name)]]
  
  def unionVarMap(vm1:VarMap, vm2:VarMap):VarMap = vm2.toList.foldLeft(vm1)( (vm, kv) => kv match {
    case (name, m) => vm.get(name) match {
      case None => vm + (name -> m)
      case Some(m2) => vm + (name -> (m ++ m2))
    }
  })
  

  /**
    * A state object for the conversion function
    *
    * @param varMap - the variable mapping
    * @param exitCtx - the exit context from the last block
    * @param throwCtxs - the list of contexts that throw exception
    * @param nestedDecls - the list of nested declared variables
    */
  case class State(
    varMap: VarMap, 
    exitCtx: TCtx,
    throwCtxs: List[TCtx], 
    nestedDecls: List[(TCtx, Ident, Type, List[Modifier])]
  )

  def thsFromState(st:State):List[TCtx] = st match {
    case State(_, _, ths, _) => ths
  }

  def eCtxFromState(st:State):TCtx = st match {
    case State(_,ectx, _, _) => ectx
  }

  sealed trait Ann

  case object Pre extends Ann
  case object Peri extends Ann
  case object Post extends Ann

  case class Label(p:ASTPath, ma:Option[Ann]) 

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
      case State(vm, eCtx, ths, nestedDecls) => State(vm, tctx, ths, nestedDecls)
    })
    _   <- put(st1)
  } yield ()

  /**
    * addThs - add the given context to the list of throwing context in the state
    *
    * @param tctx
    * @param m
    * @return
    */
  def addThs(tctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(vm, eCtx, ths, nestedDecls) => State(vm, eCtx, (ths.toSet + tctx).toList, nestedDecls )
    })
    _  <- put(st1)
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
      case State(varMap, eCtx, ths, nDecls) => {
        val nDecls1 = (nDecls.toSet + ((tctx, id, ty, mods))).toList
        State(varMap, eCtx, ths, nDecls1)
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
   * and union the ths and nDecls
   * */
  def mergeState(st1:State, st2:State):State = (st1, st2) match {
    case (State(vm1, eCtx1, ths1, nDecls1), State(vm2, eCtx2, ths2, nDecls2)) => State(unionVarMap(vm1, vm2), eCtx1, (ths1++ths2).toSet.toList, (nDecls1 ++ nDecls2).toSet.toList)
  }

  

  def extendAllVarsWithContextAndLabel(sctx:SCtx, tctx:TCtx, lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1 <- st match {
      case (State(vm0, eCtx, ths, nDecls)) => for {
        entries <- vm0.keySet.toList.traverse(v => for {
          v_lbl <- mkName(v, lbl)
        } yield (v, sctx, tctx, v_lbl ))
      } yield State(entries.foldLeft(vm0)((vm, ent) => ent match {
        case (v, sctx, tctx, v_lbl) => vm.get(v) match {
          case None => vm
          case Some(m) => vm + (v -> (m + (sctx -> (tctx, v_lbl))))
        }
      }), eCtx, ths, nDecls)
    }
    _ <- put(st1)
  } yield ()

  /**
    * converting a target context into label
    *
    * @param ctx
    * @return
    */
  def toLbl(ctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Label] = toLbl2(Nil, ctx)(m)

  def toLbl2(p:ASTPath, ctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Label] = ctx match {
    case TBox => m.pure(Label(p, None))
    case TLast(ctx2) => toLbl2(p, ctx2)
    case THead(ctx2) => toLbl2(p, ctx2) 
    case TTail(ctx2) => p match {
      case Nil => m.raiseError("toLbl failed with TTail.")
      case _   => {
        val pp = p.init
        val l  = p.last
        val p2 = pp ++ List(l+1)
        toLbl2(p2, ctx2)
      }
    }
    case TThen(ctx2) => toLbl2(p++List(0,0), ctx2) 
    case TElse(ctx2) => toLbl2(p++List(1,0), ctx2)
    case TIfPostPhi => m.pure(Label(p, Some(Post)))
    case TWhilePrePhi => m.pure(Label(p, Some(Pre)))
    case TWhile(ctx2) => toLbl2(p++List(0,0), ctx2)
    case TWhilePostPhi => m.pure(Label(p, Some(Post)))
    case TTry(ctx2) => toLbl2(p++List(0,0), ctx2)
    case TTryPeriPhi => m.pure(Label(p, Some(Peri)))
    case TCatch(ctx2) => toLbl2(p++List(1,0), ctx2)
    case TTryPostPhi => m.pure(Label(p, Some(Post)))
  } 


  implicit val eqTCtx:Eq[TCtx] = new Eq[TCtx]{
    override def eqv(x: TCtx, y: TCtx): Boolean = (x,y) match {
      case (TBox, TBox) => true
      case (TLast(ctx1), TLast(ctx2)) => eqv(ctx1, ctx2)
      case (THead(ctx1), THead(ctx2)) => eqv(ctx1, ctx2)
      case (TTail(ctx1), TTail(ctx2)) => eqv(ctx1, ctx2)
      case (TThen(ctx1), TThen(ctx2)) => eqv(ctx1, ctx2)
      case (TElse(ctx1), TElse(ctx2)) => eqv(ctx1, ctx2)
      case (TIfPostPhi, TIfPostPhi) => true
      case (TWhilePrePhi, TWhilePrePhi) => true
      case (TWhile(ctx1), TWhile(ctx2)) => eqv(ctx1, ctx2)
      case (TWhilePostPhi, TWhilePostPhi) => true
      case (TTry(ctx1), TTry(ctx2)) => eqv(ctx1, ctx2)
      case (TTryPeriPhi, TTryPeriPhi) => true
      case (TCatch(ctx1), TCatch(ctx2)) => eqv(ctx1, ctx2)
      case (TTryPostPhi, TTryPostPhi) => true
      case (_,_) => false 
    }
  }

  implicit val partialOrderTCtx:PartialOrder[TCtx] = new PartialOrder[TCtx]{
    override def partialCompare(x: TCtx, y: TCtx): Double = (x,y) match {
      case (_, _) if (eqv(x,y)) => 0.0
      case (TBox, _) => -1.0
      case (_, TBox) => 1.0
      case (TLast(ctx1), TLast(ctx2)) => partialCompare(ctx1,ctx2)

      case (THead(ctx1), THead(ctx2)) => partialCompare(ctx1,ctx2)
      case (THead(_), TTail(_)) => -1.0
      case (TTail(ctx1), TTail(ctx2)) => partialCompare(ctx1,ctx2)
      case (TTail(_), THead(_)) => 1.0

      case (TThen(ctx1), TThen(ctx2)) => partialCompare(ctx1, ctx2)
      case (TThen(_), TIfPostPhi) => -1.0
      case (TElse(ctx1), TElse(ctx2)) => partialCompare(ctx1, ctx2)
      case (TElse(_), TIfPostPhi) => -1.0
      case (TIfPostPhi, TThen(_)) => 1.0
      case (TIfPostPhi, TElse(_)) => 1.0

      case (TWhilePrePhi, TWhile(_)) => -1.0
      case (TWhilePrePhi, TWhilePostPhi) => -1.0
      case (TWhile(_), TWhilePrePhi) => 1.0
      case (TWhile(ctx1), TWhile(ctx2)) => partialCompare(ctx1, ctx2)
      case (TWhile(_), TWhilePostPhi) => -1.0
      case (TWhilePostPhi, TWhilePrePhi) => 1.0
      case (TWhilePostPhi, TWhile(_)) => 1.0

      case (TTry(ctx1), TTry(ctx2)) => partialCompare(ctx1, ctx2)
      case (TTry(_), TTryPeriPhi) => -1.0
      case (TTry(_), TCatch(_)) => -1.0
      case (TTry(_), TTryPostPhi) => -1.0
      case (TTryPeriPhi, TTry(_)) => 1.0
      case (TTryPeriPhi, TCatch(_)) => -1.0
      case (TTryPeriPhi, TTryPostPhi) => -1.0
      case (TCatch(_), TTry(_)) => 1.0
      case (TCatch(_), TTryPeriPhi) => 1.0
      case (TCatch(ctx1), TCatch(ctx2)) => partialCompare(ctx1, ctx2)
      case (TCatch(_), TTryPostPhi) => -1.0
      case (TTryPostPhi, TTry(_)) => 1.0
      case (TTryPostPhi, TTryPeriPhi) => 1.0
      case (TTryPostPhi, TCatch(_)) => 1.0

      case _ => Double.NaN
    }
  }


  /**
    * SemiLattice for target program context
    * 
    * It is a partial function since it has a partial order.
    * 
    * For all ctx from the same program, the LUB (and GLB) exist
    * 
    * combine(x,y) finds the LUB of x and y, i.e. join
    */

  implicit val semilatticeTCtx:Semilattice[TCtx] = new Semilattice[TCtx] {
    override def combine(x: TCtx, y: TCtx): TCtx = (x,y) match {
      case (_, _) if (eqTCtx.eqv(x,y)) => x 
      case (TBox, a) => a
      case (a, TBox) => a
      case (TLast(ctx1), TLast(ctx2)) => TLast(combine(ctx1, ctx2))

      case (THead(ctx1), THead(ctx2)) => THead(combine(ctx1, ctx2))
      case (THead(_), TTail(_)) => y 
      case (TTail(ctx1), TTail(ctx2)) => TTail(combine(ctx1, ctx2))
      case (TTail(_), THead(_)) => x
      
      case (TThen(ctx1), TThen(ctx2)) => TThen(combine(ctx1, ctx2))
      case (TThen(_), TIfPostPhi) => TIfPostPhi 
      case (TElse(ctx1), TElse(ctx2)) => TElse(combine(ctx1, ctx2))
      case (TElse(_), TIfPostPhi) => TIfPostPhi

      case (TWhilePrePhi, TWhile(_)) => y 
      case (TWhilePrePhi, TWhilePostPhi) => TWhilePostPhi
      case (TWhile(_),  TWhilePrePhi) => x
      case (TWhile(ctx1), TWhile(ctx2)) => TWhile(combine(ctx1, ctx2))
      case (TWhile(_),  TWhilePostPhi) => TWhilePostPhi
      case (TWhilePostPhi, TWhilePrePhi) => TWhilePostPhi
      case (TWhilePostPhi, TWhile(_)) => TWhilePostPhi

      case (TTry(ctx1), TTry(ctx2)) => TTry(combine(ctx1, ctx2))
      case (TTry(_), TTryPeriPhi) => TTryPeriPhi
      case (TTry(_), TCatch(_)) => y
      case (TTry(_), TTryPostPhi) => TTryPostPhi
      case (TTryPeriPhi, TTry(_)) => TTryPeriPhi 
      case (TTryPeriPhi, TCatch(_)) => y
      case (TTryPeriPhi, TTryPostPhi) => TTryPostPhi
      case (TCatch(_), TTry(_)) => x 
      case (TCatch(_), TTryPeriPhi) => x
      case (TCatch(ctx1), TCatch(ctx2)) => TCatch(combine(ctx1, ctx2))
      case (TCatch(_), TTryPostPhi) => TTryPostPhi
      case (TTryPostPhi, TTry(_)) => TTryPostPhi
      case (TTryPostPhi, TTryPeriPhi) => TTryPostPhi
      case (TTryPostPhi, TCatch(_)) => TTryPostPhi

    }

  }


  def Rlt(ths:List[TCtx], ctx:TCtx, vm:VarMap, x:Name):Option[Name] = R(ths, ctx, vm, x, 
    {
      case ((tctx1, tctx2))  => (partialOrderTCtx.partialCompare(tctx1, tctx2) == -1.0)
    }
  )

    def Rleq(ths:List[TCtx], ctx:TCtx, vm:VarMap, x:Name):Option[Name] = R(ths, ctx, vm, x, 
    {
      case ((tctx1, tctx2))  => ((partialOrderTCtx.partialCompare(tctx1, tctx2) == -1.0) || (partialOrderTCtx.partialCompare(tctx1, tctx2) == 0.0))
    }
  )

  /**
    * Compute the name from the lub of all the reachable context until ctx
    *
    * @param ths
    * @param ctx
    * @param vm
    * @param x
    * @param cmp - modifier to switch between leq or lt
    * @return optional target name, though it should never be None
    */
  def R(ths:List[TCtx], ctx:TCtx, vm:VarMap, x:Name, cmp:(TCtx,TCtx) => Boolean):Option[Name] = vm.get(x) match {
    case None => None // though this should never happen
    case Some(trs) => {
      val tcvs = for { 
        (sctx, (tctx, tx)) <- trs.toList
        if (cmp(tctx, ctx) && (!block(tctx, ths, ctx)))
      } yield (tctx, tx)

      // partial function, but lub should be in the set.
      def comb(px:(TCtx,Name), py:(TCtx,Name)):(TCtx, Name) = (px, py) match {
        case ((cx, vx), (cy, vy)) if (semilatticeTCtx.combine(cx, cy) == cx) => (cx, vx)
        case ((cx, vx), (cy, vy)) if (semilatticeTCtx.combine(cx, cy) == cy) => (cy, vy)
        
      }

      tcvs match {
        case Nil => None // this should not happen.
        case (tcv::tcvs) => tcvs.foldLeft(tcv)((x,y) => comb(x,y)) match {
          case (_, vx) => Some(vx)
        }
      }
    }
  }

  def block(src:TCtx, ths:List[TCtx], tar:TCtx):Boolean = {
    val ths1 = for { 
      th <- ths;  
      if (partialOrderTCtx.partialCompare(th, tar) == -1.0)
    } yield th
    block(src, ths1)
  }

  def block(src:TCtx, ths:List[TCtx]): Boolean = {
    val thsCl = closure(ths)
    thsCl.contains(src)
  }

  /**
    * Building a closure of a set of (blocking) contexts
    *  
    * base:
    *   curr \subseteq closure(curr)
    * 
    * recursive cases:
    *   case 1:
    *   ctx[if E {Box} else {\overline{B}} join {\overline{\phi}}] \in closure(curr) 
    *      and 
    *   ctx[if E {\overline{B}} else {Box} join {\overline{\phi}}] \in closure(curr)
    *      implies
    *   ctx \in closure(curr)
    * 
    *   case 2:
    *   ctx[try {Box} join {\overline{\phi}} catch (t x) {\overline{B}} join {\overline{\phi}}] \in closure(curr)
    *      and
    *   ctx[try {\overline{B}} join {\overline{\phi}} catch (t x) {Box} join {\overline{\phi}}] \in closure(curr)
    *      implies
    *   ctx \in closure(curr)
    * 
    *   case 3:
    *   ctx[B;Box] \in closure(curr)
    *      implies
    *   ctx[Box;\overline{B}] \in closure(curr) and ctx \in closure(curr)
    * 
    *   case 4:
    *   ctx[Box;\overline{B}] \in closure(curr)
    *      implies 
    *   ctx \in closure(curr)
    * 
    *   case 5:
    *   ctx[Box;] \in closure(curr)
    *      implies
    *   ctx \in closure(curr)
    *      
    * @param curr initial set of contexts with throw statement
    * @return the closture of the list of contexts in the blockage
    */
  
  def closure(curr:List[TCtx]):List[TCtx] = {
    val next = go(curr, curr.toSet).toSet

    if (next.size == curr.toSet.size) { curr } else closure(next.toList)
  }

  def go(curr:List[TCtx], acc:Set[TCtx]): Set[TCtx] = curr match {
    case Nil => acc
    case (TBox::rest) => go(rest, acc) // todo: check
    case (TIfPostPhi::rest) => go(rest, acc)
    case (TWhilePrePhi::rest) => go(rest, acc)
    case (TWhilePostPhi::rest) => go(rest, acc)
    case (TTryPeriPhi::rest) => go(rest, acc)
    case (TTryPostPhi::rest) => go(rest, acc)
    case (ctx::rest) => getLeaf(ctx, List(), List()) match {
      case None => go(rest,acc)
      // case 1 
      case Some((TThen(TBox), xtrs, ctrs)) => { // it is a Then, we need to find an Else
        rest.map( ctx1 => extract(xtrs, ctx1)).filterNot(_.isEmpty) match {
          case xs if xs.contains(Some(TElse(TBox))) => {
            val acc1 = acc + construct(ctrs, TBox)
            go(rest,acc1)
          }
          case _ => go(rest, acc) 
        }
      }
      case Some((TElse(TBox), xtrs, ctrs)) => { // it is a Else, we need to find a Then
        rest.map( ctx1 => extract(xtrs, ctx1)).filterNot(_.isEmpty) match {
          case xs if xs.contains(Some(TThen(TBox))) => {
            val acc1 = acc + construct(ctrs, TBox)
            go(rest,acc1)
          }
          case _ => go(rest, acc) 
        }
      }
      // case 2
      case Some((TTry(TBox), xtrs, ctrs)) => { // it is a Try, we need to find a Catch
        rest.map( ctx1 => extract(xtrs, ctx1)).filterNot(_.isEmpty) match {
          case xs if xs.contains(Some(TCatch(TBox))) => {
            val acc1 = acc + construct(ctrs, TBox) 
            go(rest,acc1)
          }
          case _ => go(rest, acc) 
        }
      }     
      case Some((TCatch(TBox), xtrs, ctrs)) => { // it is a Catch, we need to find a Try
        rest.map( ctx1 => extract(xtrs, ctx1)).filterNot(_.isEmpty) match {
          case xs if xs.contains(Some(TTry(TBox))) => {
            val acc1 = acc + construct(ctrs, TBox)
            go(rest,acc1)
          }
          case _ => go(rest, acc) 
        }
      }
      // case 3
      case Some((TTail(TBox), xtrs, ctrs)) => {
        val acc1 = acc ++ Set(construct(ctrs, THead(TBox)), construct(ctrs, TBox))
        go(rest, acc1)
      } 
      // case 4
      case Some((THead(TBox), xtrs, ctrs)) => {
        val acc1 = acc + construct(ctrs, TBox)
        go(rest, acc1)
      }
      // case 5 
      case Some((TLast(TBox), xtrs, ctrs)) => {
        val acc1 = acc + construct(ctrs, TBox)
        go(rest, acc1)
      }
      case _ => go(rest, acc)
    }
  }



  type Extractor = TCtx => Option[TCtx]
  type Constructor = TCtx => TCtx

  /**
    * extract - applies a sequence of extractors to a context
    *
    * @param xtrs
    * @param ctx
    * @return
    */
  def extract(xtrs:List[Extractor], ctx:TCtx): Option[TCtx] = xtrs match {
    case Nil => Some(ctx)
    case (x::xs) => x(ctx) match {
      case None => None
      case Some(ctx1) => extract(xs, ctx1)
    }
  }

  /**
    * construct - applies a sequence of constructors to a context
    *
    * @param ctrs
    * @param ctx
    * @return
    */
  def construct(ctrs:List[Constructor], ctx:TCtx): TCtx = ctrs match {
    case Nil => ctx
    case (c::cs) => construct(cs, c(ctx))
  }

  /**
    * getLeaf - given a non-leaf node, extract the node containing the leaf, the sequence of extractors and the sequence of constructors
    *
    * @param ctx current node
    * @param xtrs sequence of extractor, should be applied from left to right, outermost to inner most
    * @param ctrs seuqence of constructor, should be appluied from left to right, innermost to outer most
    * @return 
    */
  def getLeaf(ctx:TCtx, xtrs:List[Extractor], ctrs:List[Constructor]):Option[(TCtx, List[Extractor], List[Constructor])] = ctx match {
    case TBox => None
    case TLast(TBox) => Some(ctx, xtrs, ctrs)
    case TLast(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TLast(ctx1) => Some(ctx1)
      case _ => None
    }), (TLast(_))::ctrs)
    case THead(TBox) => Some(ctx, xtrs, ctrs)
    case THead(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case THead(ctx1) => Some(ctx1)
      case _ => None
    }), (THead(_))::ctrs)
    case TTail(TBox) => Some(ctx, xtrs, ctrs)
    case TTail(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TTail(ctx1) => Some(ctx1) 
      case _ => None
    }), (TTail(_))::ctrs)
    case TThen(TBox) => Some(ctx, xtrs, ctrs)
    case TThen(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TThen(ctx1) => Some(ctx1)
      case _ => None
    }), (TThen(_))::ctrs)
    case TElse(TBox) => Some(ctx, xtrs, ctrs) 
    case TElse(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TElse(ctx1) => Some(ctx1)
      case _ => None
    }), (TElse(_))::ctrs)
    case TIfPostPhi => None
    case TWhilePrePhi => None
    case TWhile(TBox) => Some(ctx, xtrs, ctrs)
    case TWhile(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TWhile(ctx1) => Some(ctx1)
      case _ => None
    }), (TWhile(_))::ctrs)
    case TWhilePostPhi => None
    case TTry(TBox) => Some(ctx, xtrs, ctrs)
    case TTry(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TTry(ctx1) => Some(ctx1)
      case _ => None
    }), (TTry(_))::ctrs)
    case TTryPeriPhi => None
    case TCatch(TBox) => Some(ctx, xtrs, ctrs)
    case TCatch(ctx1) => getLeaf(ctx1, xtrs++List((ctx:TCtx) => ctx match {
      case TCatch(ctx1) => Some(ctx1)
      case _ => None
    }), (TCatch(_))::ctrs)
    case TTryPostPhi => None
  }


  def kexp(e:Exp, ctx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Exp] = e match {
    case ArrayAccess(idx) => m.pure(e) // TODO: fixme
    case Cast(ty, exp) => for {
      exp1 <- kexp(exp, ctx) 
    } yield Cast(ty,exp1)
    case ArrayCreate(ty, exps, num_dims) => for {
      exps1 <- exps.traverse(kexp(_, ctx))
    } yield ArrayCreate(ty, exps1, num_dims)
    
    case ArrayCreateInit(ty, size, init) => m.pure(e) //TODO: fixme
    case Assign(lhs, op, rhs) => m.pure(e) // TODO: it should not be handled here.
    case BinOp(e1, op, e2) => for {
      e1p <- kexp(e1, ctx)
      e2p <- kexp(e2, ctx)
    } yield BinOp(e1p, op, e2p)
  
    case ClassLit(ty) => m.pure(e) 
    case Cond(cond, true_exp, false_exp) => for {
      cond1 <- kexp(cond,ctx)
      true_exp1 <- kexp(true_exp, ctx)
      false_exp1 <- kexp(false_exp, ctx)
    } yield Cond(cond1, true_exp1, false_exp1)
    case ExpName(name) => for {
      st <- get
      exp1 <- st match {
        case State(vm, eCtx, ths, nDecls) => Rlt(ths, ctx, vm, name) match {
          case None => m.raiseError("Rlt failed")
          case Some(name1) => m.pure(ExpName(name1))
        }
      }
    } yield exp1
    
    case FieldAccess_(access) => m.pure(e) // TODO: fixme
    case InstanceCreation(type_args, type_decl, args, body) => m.pure(e) //TODO: fixme
    case InstanceOf(e, ref_type) => for {
      e1 <- kexp(e, ctx)
    } yield InstanceOf(e1, ref_type) // TODO: fixme
    case Lambda(params, body) => m.pure(e)
    case Lit(lit) => m.pure(e)
    case MethodInv(methodInv) => m.pure(e) // TODO: it should not be handled here.
    case MethodRef(name, id) => m.pure(e) // TODO: fixme
    case PostDecrement(exp) => m.pure(e) // TODO: it should have been desugared.
    case PostIncrement(exp) => m.pure(e) // TODO: it should have been desugared.
    case PreBitCompl(exp) => for {
      e1 <- kexp(exp, ctx) 
    } yield PreBitCompl(e1) // TODO: fixme
    case PreDecrement(exp) => m.pure(e) // TODO: it should have been desugared.
    case PreIncrement(exp) => m.pure(e) // TODO: it should have been desugared.
    case PreMinus(exp) => for {
      exp1 <- kexp(exp, ctx)
    } yield PreMinus(exp1) 
    case PreNot(exp) => for {
      exp1 <- kexp(exp, ctx)
    } yield PreNot(exp1) 
    case PrePlus(exp) => for {
      exp1 <- kexp(exp, ctx)
    } yield PrePlus(exp1)
    case QualInstanceCreation(exp, type_args, id, args, body) => m.pure(e) //TODO: fixme
    case This => m.pure(e) 
    case ThisClass(name) => m.pure(e) 
  }




  

  def kstmt(stmt:Stmt, ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, SSABlock] = {
    val tctx = kctx(ctx)
    stmt match {
      case Assert(exp, msg) => for {
        lbl  <- toLbl(tctx)
        exp1 <- kexp(exp, tctx)
        msg1 <- msg.traverse( m => kexp(m, tctx))
        _    <- setECtx(tctx)
      } yield SSABlock(lbl, SSAAssert(exp1, msg1))

      case BasicFor(init, loop_cond, post_update, stmt) => m.raiseError("BasicFor should have been desugared.")

      case EnhancedFor(modifiers, ty, id, exp, stmt) => m.raiseError("EnhancedFor should have been desugared.")

      case Break(id) => m.raiseError("Break is not yet supported.") // TODO: fixme

      case Continue(id) => m.raiseError("Continue is not yet supported.") // TODO: fixme

      case Do(stmt, exp) => m.raiseError("Do should have been desguared.")

      case Empty => for {
        lbl <- toLbl(tctx)
        _   <- setECtx(tctx)
      } yield (SSABlock(lbl, SSAEmpty))

      case ExpStmt(Assign(lhs, op, rhs)) => lhs match {
        case NameLhs(x) => for {
          st <- get
          b <- st match {
            case State(vm, eCtx, ths, nDecls) => for {
              rhs1 <- kexp(rhs, tctx)
              lbl <- toLbl(tctx) 
              xlbl <- mkName(x,lbl)
              vm1 <- vm.get(x) match {
                case None => m.pure(vm + (x -> (ctx -> (tctx, xlbl))))
                case Some(im) => m.pure(vm + (x -> (im + (ctx -> (tctx, xlbl)))))
              }
            } yield SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(NameLhs(xlbl), op, rhs1)))))
          }
          _    <- setECtx(tctx)        
        } yield b

        case FieldLhs(fa) => fa match {
          case PrimaryFieldAccess(e1, id) => for {
            rhs1 <- kexp(rhs, tctx)
            lbl <- toLbl(tctx)
            e2  <- kexp(e1, tctx)
            _    <- setECtx(tctx)
          } yield SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(e2, id)), op, rhs1)))))
          case SuperFieldAccess(id) => for {
            rhs1 <- kexp(rhs, tctx)
            lbl <- toLbl(tctx)
            _    <- setECtx(tctx)
          } yield SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(FieldLhs(SuperFieldAccess(id)), op, rhs1)))))
          case ClassFieldAccess(name, id) => for {
            rhs1 <- kexp(rhs, tctx)
            lbl <- toLbl(tctx)
            _    <- setECtx(tctx)
          } yield SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(FieldLhs(ClassFieldAccess(name,id)), op, rhs1)))))
        }

        case ArrayLhs(ArrayIndex(e,es)) => for {
          rhs1 <- kexp(rhs, tctx)
          e1   <- kexp(e, tctx)
          es1 <- es.traverse( e => kexp(e, tctx))
          lbl <- toLbl(tctx)
          _    <- setECtx(tctx)
        } yield SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(e1,es1)), op, rhs1)))))
        
      }

      case ExpStmt(exp) => for {
        exp1 <- kexp(exp, tctx)
        lbl  <- toLbl(tctx)
        _    <- setECtx(tctx)
      } yield SSABlock(lbl, SSAExps(List(ExpStmt(exp1))))

      case IfThen(exp, stmt) => m.raiseError("If then statment should have been desugared.")

      
      case IfThenElse(exp, then_stmt, else_stmt) => for {
        exp1       <- kexp(exp, tctx)
        lbl        <- toLbl(tctx)
        // reset the ths in the state 
        st         <- get
        stThenIn   <- st match {
          case State(vm, eCtx, ths, nDecls) => m.pure(State(vm, eCtx, Nil, nDecls))
        }
        _          <- put(stThenIn)
        then_ctx   <- m.pure(putSCtx(ctx, SThen(SBox)))
        then_stmts <- kstmtBlock(then_stmt, then_ctx)
        stThenOut  <- get
        stElseIn   <- st match {
          case State(vm, eCtx, ths, nDecls) => m.pure(State(vm, eCtx, Nil, nDecls))
        }
        _          <- put(stElseIn)
        else_ctx   <- m.pure(putSCtx(ctx,SElse(SBox)))
        else_stmts <- kstmtBlock(else_stmt, else_ctx)
        stElseOut  <- get
        stMerged   <- m.pure(mergeState(st, stThenOut, stElseOut))
        _          <- put(stMerged)

        tctx2      <- m.pure(putTCtx(tctx, TIfPostPhi))
        lbl2       <- toLbl(tctx2)

        phis       <- mkPhi(stThenOut, stElseOut, lbl2)
        _          <- extendAllVarsWithContextAndLabel(ctx, tctx, lbl2)
        _          <- setECtx(tctx)
      } yield SSABlock(lbl, SSAIf(exp1, then_stmts, else_stmts, phis))
      
      case Labeled(id, stmt) => m.raiseError("Labeled statement is not supported.") // todo

      case Return(oexp) => oexp match {
        case Some(exp) => for {
          lbl  <- toLbl(tctx)
          exp1 <- kexp(exp,tctx) 
          _    <- setECtx(tctx)

        } yield SSABlock(lbl, SSAReturn(Some(exp1)))
        case None => for {
          lbl  <- toLbl(tctx)
          _    <- setECtx(tctx)
        } yield SSABlock(lbl, SSAReturn(None))
      }

      case StmtBlock(blk) => m.raiseError("Statement Block should not be handled here.") // todo

      case Switch(exp, blocks) => m.raiseError("Switch statement is not supported.") // todo
      case Synchronized(exp, blk) => m.raiseError("Synchronized statement is not supported.") // todo

      case Throw(exp) => for {
        lbl  <- toLbl(tctx)
        exp1 <- kexp(exp, tctx)
        _    <- addThs(tctx)
        _    <- setECtx(tctx)
      } yield SSABlock(lbl, SSAThrow(exp1))


      case Try(try_blk, Catch(param, catch_blk)::Nil, finally_blk) => finally_blk match {
        case Some(b) => m.raiseError("Try catch finally should be desugared to Try catch.")
        case None    => for {
          lbl       <- toLbl(tctx)
          st        <- get

          stTryIn   <- st match {
            case State(vm, eCtx, ths, nestedDecls) => m.pure(State(vm,eCtx,Nil, nestedDecls))
          }
          _         <- put(stTryIn)
          try_ctx   <- m.pure(putSCtx(ctx, STry(SBox)))
          try_stmts <- kBlock(try_blk, try_ctx)
          stTryOut  <- get

          tctx1p    <- m.pure(putTCtx(tctx, TTryPeriPhi))
          lbl1p     <- toLbl(tctx1p)
          phis_peri <- mkPhisFromThrows(stTryOut, thsFromState(st), lbl1p)

          catch_ctx <- m.pure(putSCtx(ctx, SCatch(SBox)))
          catch_tctx <- m.pure(putTCtx(tctx, TCatch(TBox)))
          stCatchIn <- stTryOut match {
            case State(vm1, eCtx1, ths1, nestedDecls1) => for {
              entries <- vm1.keySet.toList.traverse(v => for {
                v_lbl <- mkName(v, lbl1p)
              } yield (v, ctx, tctx1p, v_lbl))
            } yield State((entries.foldLeft(vm1)((vm, ent) => ent match {
              case (v, sctx, tctx, v_lbl) => vm.get(v) match {
                case None => vm
                case Some(m) => vm + (v -> (m + (sctx -> (tctx, v_lbl))))
              }}) + ((paramIdtoName(param)) -> Map(ctx -> ((catch_tctx, paramIdtoName(param)))))), 
              tctx1p, Nil, nestedDecls1)
            }
          catch_stmts <- kBlock(catch_blk, catch_ctx)
          stCatchOut <- get
          
          tctx3 <- m.pure(putTCtx(tctx, TTryPostPhi))
          lbl3  <- toLbl(tctx3)

          phis_post <- mkPhi(stTryOut, stCatchOut, lbl3)
          _         <- extendAllVarsWithContextAndLabel(ctx, tctx3, lbl3)
          _         <- setECtx(tctx)
          _         <- thsFromState(st).traverse( ctx => addThs(ctx))
        } yield SSABlock(lbl, SSATry(try_stmts, phis_peri, param, catch_stmts, phis_post))
      } 
      case Try(_, Nil, _) => m.raiseError("There is no catch in a try statement")
      case Try(_, _::_, _) => m.raiseError("Multiple catch clauses encountered, which should have been merged.")
      case While(exp, stmt) => for {
        lbl <- toLbl(tctx)
        st  <- get
        lblp  <- toLbl(eCtxFromState(st))
        tctx_pre <- m.pure(putTCtx(tctx, TWhilePrePhi))
        lbl0  <- toLbl(tctx_pre)
        phis_pre  <- st match {
          case State(vm, eCtx, ths, nestedDecls) if (ths.contains(eCtx)) => 
            vm.keySet.toList.traverse( v => for {
              v_lbl <- mkName(v, lbl0)
            } yield Phi(v, vlbl, Map()))
          case State(vm, eCtx, ths, nestedDecls) => 
            vm.keySet.toList.traverse( v => for {
              v_lbl <- mkName(v, lbl0)
              rhs <- m.pure(Rleq(ths, eCtx, vm, v) match {
                case None => Map()
                case Some(n) => Map(lblp -> n)
              })
            } yield Phi(v, v_lbl, rhs))
        }
        stBodyIn <- st match {
          case State(vm, eCtx, ths, nestedDecls) => for {
            entries <- vm.keySet.toList.traverse( v => for {
              v_lbl <- mkName(v, lbl0)
            } yield (v, ctx, tctx_pre, v_lbl))
          } yield State(entries.foldLeft(vm)((vm1, ent) => ent match {
            case (v, sctx, tctx2, v_lbl) => vm1.get(v) match {
              case None => vm1
              case Some(m) => vm1 + (v -> (m + (sctx -> (tctx2, vlbl))))
            }}), tctx_pre, Nil, nestedDecls)
        }
        _ <- put(stBodyIn)
        exp1 <- kexp(exp, tctx)
        body_ctx <- m.pure(putSCtx(ctx, SWhile(SBox)))
        body_stmts <- kstmtBlock(stmt, body_ctx)
        stBodyOut <- get
      }
    }
  } 

  def paramIdtoName(fp:FormalParam):Name = fp match {
    case FormalParam(mods, ty, arity, var_decl_id) => Name(List(idFromVarDeclId(var_decl_id)))
  }

  /**
    * mkPhi - create phis by merging all variables found in vmap in st1 and st2, 
    *
    * @param st1 - incoming state 1
    * @param st2 - incoming state 2
    * @param lbl - phi LHS variable label
    * @param m   - monad type class context
    * @return
    */

  def mkPhi(st1:State, st2:State, lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State, List[Phi]] = (st1, st2) match {
    case (State(vm1, eCtx1, ths1, _), State(vm2, eCtx2, ths2,_)) if (ths1.contains(eCtx1) && ths2.contains(eCtx2)) => for {
      vs <- m.pure(vm1.keySet ++ vm2.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
      } yield Phi(v, vlbl, Map()))
    } yield phis
    case (State(vm1, eCtx1, ths1, _), State(vm2, eCtx2, ths2,_)) if (!ths1.contains(eCtx1) && ths2.contains(eCtx2)) => for {
      vs <- m.pure(vm1.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbl1 <- toLbl(eCtx1)
        oname <- m.pure(Rleq(ths1, eCtx1, vm1, v))
      } yield Phi(v, vlbl, oname match {
        case None => Map()
        case Some(n) => Map(lbl1 -> n)
      }))
    } yield phis
    case (State(vm1, eCtx1, ths1, _), State(vm2, eCtx2, ths2,_)) if (ths1.contains(eCtx1) && !ths2.contains(eCtx2)) => for {
      vs <- m.pure(vm2.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbl2 <- toLbl(eCtx2)
        oname <- m.pure(Rleq(ths2, eCtx2, vm2, v))
      } yield Phi(v, vlbl, oname match {
        case None => Map()
        case Some(n) => Map(lbl2 -> n)
      }))
    } yield phis  
    case (State(vm1, eCtx1, ths1, _), State(vm2, eCtx2, ths2,_)) => for {
      vs <- m.pure(vm1.keySet ++ vm2.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbl1 <- toLbl(eCtx1)
        lbl2 <- toLbl(eCtx2)
        oname1 <- m.pure(Rleq(ths1, eCtx1, vm1, v))
        oname2 <- m.pure(Rleq(ths2, eCtx2, vm2, v))
      } yield Phi(v, vlbl, (oname1, oname2) match {
        case (Some(n1), Some(n2)) => Map(lbl1 -> n1, lbl2 -> n2) // do we need the remaining 3 cases?
        case (None, None) => Map()
        case (Some(n1), None) => Map(lbl1 -> n1)
        case (None, Some(n2)) => Map(lbl2 -> n2)
      }))
    } yield phis  
  }

  /**
    * mkPhisFromThrows - making the peri phis in try-catch statement from the throws contexts from try
    *
    * @param st
    * @param parentThs
    * @param lbl
    * @param m
    * @return
    */
  def mkPhisFromThrows(st:State, parentThs:List[TCtx], lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State, List[Phi]] = st match {
    case State(vm, eCtx, ths, nestedDecls) => for {
      vs <- m.pure(vm.keySet.toList)
      phis <- vs.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbls_onames <- ths.traverse( ctx => for {
          lbl1 <- toLbl(ctx)
          on   <- m.pure(Rleq(parentThs, ctx, vm, v))
        } yield (lbl1,on))
        rhs <- m.pure(
          lbls_onames
          .foldLeft(Map():Map[Label,Name])((m,p) => p match {
            case (lbl, None) => m 
            case (lbl, Some(n)) => m + (lbl -> n)
          }))
      } yield Phi(v, vlbl, rhs))
    } yield phis
  }

  

  /**
    * kstmtBlock - a special version just to handle StmtBlock
    *
    * @param stmt
    * @param ctx
    * @param st
    * @return
    */
  
  def kstmtBlock(stmt:Stmt, ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State,List[SSABlock]] = stmt match {
    case StmtBlock(Block(blkStmts)) => kblkStmts(blkStmts,ctx)
    case _ => for {
      b <- kstmt(stmt, ctx)
    } yield List(b)
  }

  def kBlock(blk:Block, ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State,List[SSABlock]] = blk match {
    case Block(blkStmts) => kblkStmts(blkStmts,ctx)
  }

  def kblkStmts(blkStmts:List[BlockStmt], ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, List[SSABlock]] = blkStmts match {
    case Nil => m.pure(Nil)
    case (bstmt::Nil) => for {
      b <- kblkStmt(bstmt,putSCtx(ctx, SLast(SBox)))
    } yield List(b)
    case (bstmt::rest) => for {
      b <- kblkStmt(bstmt,putSCtx(ctx, SHead(SBox)))
      bs <- kblkStmts(rest, putSCtx(ctx, STail(SBox)))
    } yield (b::bs)
  }

  def kblkStmt(blkStmt:BlockStmt, ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State,SSABlock] = blkStmt match {
    case BlockStmt_(stmt) => kstmt(stmt, ctx) 
    case LocalClass(_) => m.raiseError("local class is not supported.")
    case LocalVars(mods, ty, varDecls) => kVarDecls(mods, ty, varDecls, ctx)
  }
    
  def kVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl], ctx:SCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State, SSABlock] = for {
    tctx <- m.pure(kctx(ctx))
    // a new case, combining KVD and KSTMT assignment
    // we first record all the variable name and type
    _ <- recordVarDecls(mods, ty, varDecls, tctx)
    // we then convert varDecls to SSAVarDecl, hm... what about array init?
    // array id should not be renamed and should not be merged in phis, we keep them inplace as 
    lbl <- toLbl(tctx)
    varDecls1 <- kVarDecls(varDecls, tctx)
  } yield SSABlock(lbl, SSAVarDecls(mods, ty, varDecls1))
  
  def recordVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl], tCtx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State,Unit] = varDecls match {
    case Nil => m.pure(())
    case (varDecl::rest) => varDecl match {
      case VarDecl(VarId(id), v_init) => for {
        _ <- addNestedVarDecls(tCtx, id, ty, mods)
        _ <- recordVarDecls(mods, ty, rest, tCtx)
      } yield ()
      case VarDecl(_, v_init) => // it is an array declaration, we don't need to record it as array id will never be merged in phis.
        recordVarDecls(mods, ty, rest, tCtx)
    }
  }

  def kVarDecls(varDecls:List[VarDecl], tCtx:TCtx)(implicit m:MonadError[SSAState, ErrorM]):SState[State,List[VarDecl]] = varDecls match {
    case Nil => m.pure(Nil)
    case (varDecl::rest) => varDecl match {
      case VarDecl(VarId(id), ov_init) => for {
        lbl <- toLbl(tCtx)
        id1 <- mkId(id, lbl)
        ov_init1 <- ov_init match {
          case None => m.pure(None)
          case Some(v_init) => for { 
            vi1 <- kVarInit(v_init,tCtx)
          } yield Some(vi1)
        }
        rest1 <- kVarDecls(rest, tCtx)
      } yield (VarDecl(VarId(id1), ov_init1)::rest1)
      // array init, we do not rename the lhs ID, we only convert the referenced ids on the RHS
      case VarDecl(VarDeclArray(var_decl_id), ov_init) => for {
        ov_init1 <- ov_init match {
          case None => m.pure(None)
          case Some(v_init) => for {
            vi1 <- kVarInit(v_init, tCtx)
          } yield Some(vi1)
        }
        rest1 <- kVarDecls(rest, tCtx)
      } yield (VarDecl(VarDeclArray(var_decl_id), ov_init1)::rest1)
    }
  }

  def kVarInit(vInit:VarInit, tCtx:TCtx)(implicit m:MonadError[SSAState, ErrorM]): SState[State, VarInit] = vInit match {
    case InitExp(exp) => for {
      exp1 <- kexp(exp, tCtx) 
    } yield InitExp(exp1)
    case InitArray(ArrayInit(var_inits)) => for {
      var_inits1 <- var_inits.traverse(vi => kVarInit(vi, tCtx))
    } yield InitArray(ArrayInit(var_inits1))
  }

  def mkName(n:Name, lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Name] = n match 
    {
      case Name(Nil) => m.raiseError("mkName is applied to an empty name.")
      case Name(ids) => {
        val pre = ids.init
        val x   = ids.last
        val s   = lblToStr(lbl)
        val y   = appIdStr(x, s)
        m.pure(Name(pre++List(y)))
      }
    }

  def mkId(id:Ident, lbl:Label)(implicit m:MonadError[SSAState, ErrorM]):SState[State, Ident] = {
    val s = lblToStr(lbl)
    val y = appIdStr(id,s)
    m.pure(y)
  }

  def lblToStr(lbl:Label):String = lbl match {
    case Label(p, None)       => apToStr(p)
    case Label(p, Some(Pre))  => apToStr(p) ++ "_"
    case Label(p, Some(Peri)) => apToStr(p) ++ "__"
    case Label(p, Some(Post)) => apToStr(p) ++ "___"
  }


  /**
    * deprecated thanks to kblkStmts
    *
    * @param stmts
    * @param ctx
    * @param st
    * @return
    */

  /*
  
  def kstmts(stmts:List[Stmt], ctx:SCtx, st:State):Either[ErrorM, (List[SSABlock], State)] = stmts match {
    case Nil => Right((Nil, st))
    case (s::Nil) => kstmt(s, putSCtx(ctx,SLast(SBox)), st) match {
      case Left(err) => Left(err)
      case Right((b,st1)) => Right((List(b), st1))
    }
    case (s::ss) => kstmt(s, putSCtx(ctx,SHead(SBox)), st) match {
      case Left(err) => Left(err)
      case Right((b,st1)) => kstmts(ss, putSCtx(ctx, STail(SBox)), st1) match {
        case Left(err) => Left(err)
        case Right((bs, st2)) => Right((b::bs), st2)
      }
    }
  }
  */

  /**
    * kctx - converts a source program context into a target program context
    *
    * @param ctx
    * @return
    */

  def kctx(ctx:SCtx):TCtx = ctx match {
    case SBox => TBox
    case SLast(ctx1) => TLast(kctx(ctx1))
    case SHead(ctx1) => THead(kctx(ctx1))
    case STail(ctx1) => TTail(kctx(ctx1))
    case SThen(ctx1) => TThen(kctx(ctx1))
    case SElse(ctx1) => TElse(kctx(ctx1))
    case SWhile(ctx1) => TWhile(kctx(ctx1))
    case STry(ctx1) => TTry(kctx(ctx1))
    case SCatch(ctx1) => TCatch(kctx(ctx1))
  } 

  /**
    *     A 
    *   /   \
    *  B     C
    *  |     |
    *  D     E
    *   \   /
    *     F
    */

  /*
  sealed trait T 

  case object A extends T
  case object B extends T
  case object D extends T
  case object C extends T
  case object E extends T
  case object F extends T

  implicit val eqT:Eq[T] = new Eq[T]{
    override def eqv(x: T, y: T): Boolean = (x,y) match 
    {
      case (A,A) => true
      case (B,B) => true
      case (C,C) => true
      case (D,D) => true
      case (E,E) => true 
      case (F,F) => true
      case (_,_) => false
    }
  }

  implicit val partialOrderT:PartialOrder[T] = new PartialOrder[T] {
    override def partialCompare(x: T, y: T): Double = (x,y) match {
      case (x,y) if x == y => 0.0
      case (A,B) => -1.0
      case (A,C) => -1.0
      case (B,D) => -1.0
      case (C,E) => -1.0
      case (D,F) => -1.0
      case (E,F) => -1.0
      case (_,_) => Double.NaN
    }
  }
  */
}
