package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Syntax._

object ASTUtils {
    def isWhileStmt(stmt:Stmt):Boolean = stmt match {
        case While(_, _) => true
        case _ => false
    }

    def isForStmt(stmt:Stmt): Boolean = stmt match {
        case BasicFor(_,_,_,_) => true
        case EnhancedFor(_,_,_,_,_) => true
        case _ => false
    }

    def isLocalVarsBlockStmt(blockStmt:BlockStmt):Boolean = blockStmt match {
        case LocalVars(_,_,_) => true 
        case _ => false
    }

    // def assign(lhs:Exp, rhs:Exp) : Exp = Assign(lhs, EqualA, rhs)

    def eeq(lhs:Exp, rhs:Exp) : Exp = BinOp(lhs, Equal, rhs)

    def eor(lhs:Exp, rhs:Exp) : Exp = BinOp(lhs, Or, rhs)

    def dotField(e:Exp, f:Ident) : Exp = FieldAccess_(PrimaryFieldAccess(e, f))

    def appStmt(stmt1:Stmt, stmts2:List[Stmt]):Stmt = stmt1 match {
        case StmtBlock(Block(blkstmts)) => StmtBlock(Block(blkstmts ++ stmts2.map(BlockStmt_(_))))
        case _ => StmtBlock(Block(BlockStmt_(stmt1)::stmts2.map(BlockStmt_(_))))
    }

    def prpDecl(modifiers: List[Modifier], ty:Type, var_decls:List[VarDecl], stmt:Stmt): Stmt = stmt match {
        case StmtBlock(Block(blkstmts)) => StmtBlock(Block(LocalVars(modifiers, ty, var_decls)::blkstmts))
        case _ => StmtBlock(Block(LocalVars(modifiers, ty, var_decls)::BlockStmt_(stmt)::Nil))
    }

    // convert a lhs into a rhs exp
    def lhsToRhs(lhs:Lhs):Exp = lhs match {
        case NameLhs(name) => ExpName(name)
        case FieldLhs(field_access) => FieldAccess_(field_access)
        case ArrayLhs(array_idx) => ArrayAccess(array_idx)
    }

    def appBlockStmts(stmt: Stmt, blkStmts: List[BlockStmt]): Stmt = stmt match {
        case StmtBlock(Block(blkStmts_)) =>
            StmtBlock(Block(blkStmts_ ++ blkStmts))
        case _ => StmtBlock(Block(BlockStmt_(stmt) :: blkStmts))
    }



}