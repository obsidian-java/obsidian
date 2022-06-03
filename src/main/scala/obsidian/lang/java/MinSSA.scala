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




}
