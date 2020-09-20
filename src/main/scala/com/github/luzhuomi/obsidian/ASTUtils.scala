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
}