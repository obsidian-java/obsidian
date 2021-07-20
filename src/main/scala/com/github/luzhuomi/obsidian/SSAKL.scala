package com.github.luzhuomi.obsidian


import cats.kernel._
import cats.implicits._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.obsidian.ASTPath._
import scala.collection.immutable
import com.github.luzhuomi.obsidian.ASTUtils._

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
    label: Label,
    stmt: SSAStmt
  )

  sealed trait SSAStmt 

  case class SSAAssert(exp:Exp, msg:Option[Exp]) extends SSAStmt

  case class SSAAssignments(stmts: List[Stmt]) extends SSAStmt
  
  case class SSAReturn(stmt:Stmt) extends SSAStmt

  case class SSAThrow(stmt:Stmt) extends SSAStmt

  case class SSAMethodInvocation(stmt:Stmt) extends SSAStmt

  case object SSAEmpty extends SSAStmt

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





  type VarMap = Map[Name, Map[SCtx, (TCtx, Name)]]
  
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

  type ErrorM = String

  /**
    * converting a target context into label
    *
    * @param ctx
    * @return
    */
  def toLbl(ctx:TCtx):Either[ErrorM, Label] = toLbl2(Nil, ctx)

  def toLbl2(p:ASTPath, ctx:TCtx):Either[ErrorM, Label] = ctx match {
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


  def kexp(e:Exp, ctx:TCtx, st:State):Either[ErrorM, Exp] = e match {
    case ArrayAccess(idx) => Right(e) // TODO: fixme
    case Cast(ty, exp) => kexp(exp, ctx, st) match {
      case Left(err) => Left(err)
      case Right(exp1) => Right(Cast(ty,exp1))
    } 
    case ArrayCreate(ty, exps, num_dims) => for {
      exps1 <- exps.traverse(kexp(_, ctx, st))
    } yield ArrayCreate(ty, exps1, num_dims)
    
    case ArrayCreateInit(ty, size, init) => Right(e) //TODO: fixme
    case Assign(lhs, op, rhs) => Right(e) // TODO: it should not be handled here.
    case BinOp(e1, op, e2) => for {
      e1p <- kexp(e1, ctx, st);
      e2p <- kexp(e2, ctx, st)
    } yield BinOp(e1p, op, e2p)
  
    case ClassLit(ty) => Right(e) 
    case Cond(cond, true_exp, false_exp) => for {
      cond1 <- kexp(cond,ctx,st);
      true_exp1 <- kexp(true_exp, ctx, st)
      false_exp1 <- kexp(false_exp, ctx, st)
    } yield Cond(cond1, true_exp1, false_exp1)
    case ExpName(name) => st match {
      case State(vm, eCtx, ths) => Rlt(ths, ctx, vm, name) match {
        case None => Left("Rlt failed")
        case Some(name1) => Right(ExpName(name1))
      }
    }
    case FieldAccess_(access) => Right(e) // TODO: fixme
    case InstanceCreation(type_args, type_decl, args, body) => Right(e) //TODO: fixme
    case InstanceOf(e, ref_type) => for {
      e1 <- kexp(e, ctx, st)
    } yield InstanceOf(e1, ref_type) // TODO: fixme
    case Lambda(params, body) => Right(e)
    case Lit(lit) => Right(e)
    case MethodInv(methodInv) => Right(e) // TODO: it should not be handled here.
    case MethodRef(name, id) => Right(e) // TODO: fixme
    case PostDecrement(exp) => Right(e) // TODO: it should have been desugared.
    case PostIncrement(exp) => Right(e) // TODO: it should have been desugared.
    case PreBitCompl(exp) => for {
      e1 <- kexp(exp, ctx, st) 
    } yield PreBitCompl(e1) // TODO: fixme
    case PreDecrement(exp) => Right(e) // TODO: it should have been desugared.
    case PreIncrement(exp) => Right(e) // TODO: it should have been desugared.
    case PreMinus(exp) => for {
      exp1 <- kexp(exp, ctx, st)
    } yield PreMinus(exp1) 
    case PreNot(exp) => for {
      exp1 <- kexp(exp, ctx, st)
    } yield PreNot(exp1) 
    case PrePlus(exp) => for {
      exp1 <- kexp(exp, ctx, st)
    } yield PrePlus(exp1)
    case QualInstanceCreation(exp, type_args, id, args, body) => Right(e) //TODO: fixme
    case This => Right(e) 
    case ThisClass(name) => Right(e) 
  }

  def kstmt(stmt:Stmt, ctx:SCtx, st:State):Either[ErrorM, (SSABlock, State)] = {
    val tctx = kctx(ctx)
    stmt match {
      case Assert(exp, msg) => for {
        lbl  <- toLbl(tctx)
        exp1 <- kexp(exp, tctx, st)
        msg1 <- msg.traverse( m => kexp(m, tctx, st))
      } yield (SSABlock(lbl, SSAAssert(exp1, msg1)), st)

      case BasicFor(init, loop_cond, post_update, stmt) => Left("BasicFor should have been desugared.")

      case EnhancedFor(modifiers, ty, id, exp, stmt) => Left("EnhancedFor should have been desugared.")

      case Break(id) => Left("Break is not yet supported.") // TODO: fixme

      case Continue(id) => Left("Continue is not yet supported.") // TODO: fixme

      case Do(stmt, exp) => Left("Do should have been desguared.")

      case Empty => for {
        lbl <- toLbl(tctx)
      } yield (SSABlock(lbl, SSAEmpty), st)

      case ExpStmt(Assign(lhs, op, rhs)) => lhs match {
        case NameLhs(x) => st match {
          case State(vm, eCtx, ths) => for {
            rhs1 <- kexp(rhs, tctx, st)
            lbl <- toLbl(tctx) 
            xlbl <- mkName(x,lbl)
            vm1 <- Right(vm.get(x) match {
              case None => vm + (x -> (ctx -> (tctx, xlbl)))
              case Some(im) => vm + (x -> (im + (ctx -> (tctx, xlbl))))
            })
          } yield ((SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(NameLhs(xlbl), op, rhs1)))))), st)
        }

        case FieldLhs(fa) => fa match {
          case PrimaryFieldAccess(e1, id) => for {
            rhs1 <- kexp(rhs, tctx, st)
            lbl <- toLbl(tctx)
            e2  <- kexp(e1, tctx, st)
          } yield ((SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(e2, id)), op, rhs1)))))), st)
          case SuperFieldAccess(id) => for {
            rhs1 <- kexp(rhs, tctx, st)
            lbl <- toLbl(tctx)
          } yield ((SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(FieldLhs(SuperFieldAccess(id)), op, rhs1)))))), st)
          case ClassFieldAccess(name, id) => for {
            rhs1 <- kexp(rhs, tctx, st)
            lbl <- toLbl(tctx)
          } yield ((SSABlock(lbl, SSAAssignments(List(ExpStmt(Assign(FieldLhs(ClassFieldAccess(name,id)), op, rhs1)))))), st)
        }
        // todo continue from here. 

        
      }
    }
  } 


  def mkName(n:Name, lbl:Label):Either[ErrorM, Name] = n match 
    {
      case Name(Nil) => Left("mkName is applied to an empty name.")
      case Name(ids) => {
        val pre = ids.init
        val x   = ids.last
        val s   = lblToStr(lbl)
        val y   = appIdStr(x, s)
        Right(Name(pre++List(y)))
      }
    }

  def lblToStr(lbl:Label):String = lbl match {
    case Label(p, None)       => apToStr(p)
    case Label(p, Some(Pre))  => apToStr(p) ++ "_"
    case Label(p, Some(Peri)) => apToStr(p) ++ "__"
    case Label(p, Some(Post)) => apToStr(p) ++ "___"
  }


  
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
