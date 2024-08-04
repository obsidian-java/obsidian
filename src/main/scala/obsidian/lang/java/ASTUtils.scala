package obsidian.lang.java

import obsidian.lang.java.scalangj.Syntax.*

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


    def appIdStr(id:Ident, suff:String): Ident = id match {
        case Ident(s) => Ident(s ++ "_" ++ suff)
    }

    def idFromVarDeclId(vdid: VarDeclId): Ident =
    vdid match {
        case VarId(id) => id
        case VarDeclArray(vid) => idFromVarDeclId(vid)
    }

    // the following functions should be moved to scalangj
    // apply name -> name substitution everywhere in statment

    trait NameSubst[A] {
        def applySubst(m:Map[Name,Name], a:A):A
    }


    given applySubstOption[A](using ns:NameSubst[A]):NameSubst[Option[A]] = new NameSubst[Option[A]] {
        def applySubst(m:Map[Name,Name], o:Option[A]):Option[A] = o match {
            case None => None 
            case Some(value) => Some(ns.applySubst(m,value))
        }
    }

    given applySubstStmt:NameSubst[Stmt] = new NameSubst[Stmt] {
        def applySubst(m:Map[Name, Name],stmt:Stmt) :Stmt = stmt match { 
            case Assert(exp, msg) => Assert(applySubstExp.applySubst(m, exp), msg) 
            case BasicFor(init, loop_cond, post_update, stmt) => null // TODO 
        }
    }

    given applySubstExp:NameSubst[Exp] = new NameSubst[Exp] { 
        def applySubst(m:Map[Name, Name], exp:Exp):Exp = null // TODO
    }
}