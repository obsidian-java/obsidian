package com.github.luzhuomi.obsidian

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
    * @param phiFinally: Phis before the finally block 
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


  type Label = ASTPath 

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

  type VarMap = Map[Name, Map[ASTPath, Name]]
  
  /**
    * A state object for the conversion function
    *
    * @param varMap - the variable mapping
    * @param exitLabel - the exit label from the last block
    * @param throwLabels - the list of labels that throws exception
    */
  case class State(
    varMap: VarMap, 
    exitLabel: Option[ASTPath],
    throwLabels: List[ASTPath]
  )

  def kexp(e:Exp, ap:ASTPath, st:State):Exp = e match {
    case ArrayAccess(idx) => e // TODO: fixme
    case Cast(ty, exp) => Cast(ty,kexp(exp, ap, st)) 
    case ArrayCreate(ty, exps, num_dims) => ArrayCreate(ty, exps.map(kexp(_, ap, st)), num_dims) 
    case ArrayCreateInit(ty, size, init) => e //TODO: fixme
    case Assign(lhs, op, rhs) => e // TODO: it should not be handled here.
    case BinOp(e1, op, e2) => {
      val e1p = kexp(e1, ap, st)
      val e2p = kexp(e2, ap, st)
      BinOp(e1p, op, e2p)
    }
    case ClassLit(ty) => e 
    case Cond(cond, true_exp, false_exp) => Cond(kexp(cond,ap,st), kexp(true_exp, ap, st), kexp(false_exp, ap, st))
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
}
