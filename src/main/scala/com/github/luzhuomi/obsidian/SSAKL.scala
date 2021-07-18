package com.github.luzhuomi.obsidian


import cats.kernel._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.obsidian.ASTPath._

// Kenny's version of SSA

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
    label: ASTPath,
    stmt: SSAStmt
  )

  sealed trait SSAStmt 

  case class SSAAssignments(stmts: List[Stmt]) extends SSAStmt
  
  case class SSAReturn(stmt:Stmt) extends SSAStmt

  case class SSAThrow(stmt:Stmt) extends SSAStmt

  case class SSAMethodInvocation(stmt:Stmt) extends SSAStmt

  /**
    * 
    *
    * @param stmt
    * @param phiCatch: Phis before the catch block
    * @param phiFinally: Phis after the catch block
    */
  case class SSATry(
    stmt:Stmt, 
    phiCatch: List[Phi],
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
    *
    * @param stmt
    * @param phiExit: Phis at the exit of the while stmt
    */
  case class SSAIf(
    stmt:Stmt,
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





  type VarMap = Map[Name, (SCtx, TCtx, Name)]
  
  /**
    * A state object for the conversion function
    *
    * @param varMap - the variable mapping
    * @param exitCtx - the exit context from the last block
    * @param throwCtxs - the list of contexts that throw exception
    */
  case class State(
    varMap: VarMap, 
    exitCtx: Option[TCtx],
    throwCtxs: List[TCtx]
  )

  sealed trait Ann

  case object Pre extends Ann
  case object Peri extends Ann
  case object Post extends Ann

  case class Label(p:ASTPath, ma:Option[Ann]) 

  /**
    * converting a target context into label
    *
    * @param ctx
    * @return
    */
  def toLbl(ctx:TCtx):Either[String, Label] = toLbl2(Nil, ctx)

  def toLbl2(p:ASTPath, ctx:TCtx):Either[String, Label] = ctx match {
    case TBox => Right(Label(p, None))
    case TLast(ctx2) => toLbl2(p, ctx2)
    case THead(ctx2) => toLbl2(p, ctx2) 
    case TTail(ctx2) => p match {
      case Nil => Left("toLbl failed with TTail.")
      case _   => {
        val pp = p.init
        val l  = p.last
        val p2 = pp ++ List(l+1)
        toLbl2(p2, ctx2)
      }
    }
    case TThen(ctx2) => toLbl2(p++List(0,0), ctx2) 
    case TElse(ctx2) => toLbl2(p++List(1,0), ctx2)
    case TIfPostPhi => Right(Label(p, Some(Post)))
    case TWhilePrePhi => Right(Label(p, Some(Pre)))
    case TWhile(ctx2) => toLbl2(p++List(0,0), ctx2)
    case TWhilePostPhi => Right(Label(p, Some(Post)))
    case TTry(ctx2) => toLbl2(p++List(0,0), ctx2)
    case TTryPeriPhi => Right(Label(p, Some(Peri)))
    case TCatch(ctx2) => toLbl2(p++List(1,0), ctx2)
    case TTryPostPhi => Right(Label(p, Some(Post)))
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


  def R(ths:List[TCtx], ctx:TCtx, vm:VarMap, x:Name):Name = {
    x // todo
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
    * @param ths
    * @return
    */
  
  def closure(ths:List[TCtx]):List[TCtx] = {
    ths // todo 
  }

  def kexp(e:Exp, ctx:TCtx, st:State):Exp = e match {
    case ArrayAccess(idx) => e // TODO: fixme
    case Cast(ty, exp) => Cast(ty,kexp(exp, ctx, st)) 
    case ArrayCreate(ty, exps, num_dims) => ArrayCreate(ty, exps.map(kexp(_, ctx, st)), num_dims) 
    case ArrayCreateInit(ty, size, init) => e //TODO: fixme
    case Assign(lhs, op, rhs) => e // TODO: it should not be handled here.
    case BinOp(e1, op, e2) => {
      val e1p = kexp(e1, ctx, st)
      val e2p = kexp(e2, ctx, st)
      BinOp(e1p, op, e2p)
    }
    case ClassLit(ty) => e 
    case Cond(cond, true_exp, false_exp) => Cond(kexp(cond,ctx,st), kexp(true_exp, ctx, st), kexp(false_exp, ctx, st))
    case ExpName(name) => e 
    case FieldAccess_(access) => e // TODO: fixme
    case InstanceCreation(type_args, type_decl, args, body) => e //TODO: fixme
    case InstanceOf(e, ref_type) => e // TODO: fixme
    case Lambda(params, body) => e
    case Lit(lit) => e
    case MethodInv(methodInv) => e // TODO: it should not be handled here.
    case MethodRef(name, id) => e // TODO: fixme
    case PostDecrement(exp) => e // TODO: it should have been desugared.
    case PostIncrement(exp) => e // TODO: it should have been desugared.
    case PreBitCompl(exp) => e // TODO: fixme
    case PreDecrement(exp) => e // TODO: it should have been desugared.
    case PreIncrement(exp) => e // TODO: it should have been desugared.
    case PreMinus(exp) => e // TODO: fixme
    case PreNot(exp) => e //TODO: fixme
    case PrePlus(exp) => e // TODO: fixme
    case QualInstanceCreation(exp, type_args, id, args, body) => e //TODO: fixme
    case This => e 
    case ThisClass(name) => e 
  }


  def reach(ap:ASTPath, ths:List[ASTPath], m:VarMap, n:Name):Option[Name] = {
    None // TODO: fixme
  }

  
  def block(ap:ASTPath, ths:List[ASTPath]):Boolean = ap match {
    case Nil => false
    case _   => {
      val p = ap.init
      val n = ap.last
      val end_with_throw_same_block = ths.filter( th => !(th.isEmpty) && (th.init == p))
        .exists( th => th.last >= n )
      true    
    }
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
