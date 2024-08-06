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

    def idFromVarDeclId(vdid: VarDeclId): Ident = vdid match {
        case VarId(id) => id
        case VarDeclArray(vid) => idFromVarDeclId(vid)
    }

    def idFromFormalParam(fp: FormalParam): Ident = fp match {
        case FormalParam(modifiers, ty, has_arity, var_decl_id) => idFromVarDeclId(var_decl_id)
    }


    def idsFromLambdaParams(lps: LambdaParams): List[Ident] = lps match {
        case LambdaFormalParams(formal_params) => formal_params.map(idFromFormalParam(_))
        case LambdaInferredParams(ids) => ids
        case LambdaSingleParam(id) => List(id)
    }

    // the following functions should be moved to scalangj
    // apply name -> name substitution everywhere in statment

    trait NameSubst[A] {
        def applySubst(m:Map[Name,Name], a:A):A
    }


    given applySubstBlock:NameSubst[Block] = new NameSubst[Block] {
        def applySubst(m: Map[Name, Name], blk: Block): Block = blk match {
            case Block(stmts) => Block(stmts.map(applySubstBlockStmt.applySubst(m, _)))
        }
    }

    given applySubstBlockStmt:NameSubst[BlockStmt] = new NameSubst[BlockStmt] {
        def applySubst(m: Map[Name, Name], a: BlockStmt): BlockStmt = a match {
            case BlockStmt_(stmt) => BlockStmt_(applySubstStmt.applySubst(m, stmt))
            case LocalClass(class_decl) => LocalClass(class_decl) // TODO: this might be tricky
            case LocalVars(modifiers, ty, var_decls) => 
                LocalVars(modifiers, ty, var_decls.map(applySubstVarDecl.applySubst(m, _)))
        }
    }

    given applySubstClassDecl:NameSubst[ClassDecl] = new NameSubst[ClassDecl] {
        def applySubst(m: Map[Name, Name], a: ClassDecl): ClassDecl = a match {
            case ClassDecl_(modifiers, id, type_params, ref_type, ref_types, body) => {
                val m1 = m - Name(List(id))
                ClassDecl_(modifiers, id, type_params, ref_type, ref_types, applySubstClassBody.applySubst(m1,body))
            }
            case EnumDecl(modifiers, id, ref_types, body) => {
                val m1 = m - Name(List(id))
                EnumDecl(modifiers, id, ref_types, applySubstEnumBody.applySubst(m1,body))
            }
        }
    }


    given applySubstClassBody:NameSubst[ClassBody] = new NameSubst[ClassBody] {
        def applySubst(m: Map[Name, Name], a: ClassBody): ClassBody = a match {
            case ClassBody(decls) => ClassBody(decls.map(applySubstDecl.applySubst(m, _)))
        }
    }

    given applySubstEnumBody:NameSubst[EnumBody] = new NameSubst[EnumBody] {
        def applySubst(m: Map[Name, Name], a: EnumBody): EnumBody = a match {
            case EnumBody(constants, decls) => EnumBody(constants.map(applySubstEnumConstant.applySubst(m,_)), decls.map(applySubstDecl.applySubst(m, _)))
        }
    }

    given applySubstEnumConstant:NameSubst[EnumConstant] = new NameSubst[EnumConstant] {
        def applySubst(m: Map[Name, Name], a: EnumConstant): EnumConstant = a match {
            case EnumConstant(id, args, body) => {
                val m1 = m - Name(List(id))
                EnumConstant(id, args.map(applySubstExp.applySubst(m1, _)), body.map(applySubstClassBody.applySubst(m1,_)))
            }
        }
    }

    given applySubstDecl:NameSubst[Decl] = new NameSubst[Decl] {
        def applySubst(m: Map[Name, Name], a: Decl): Decl = a match {
            case InitDecl(is_static, blk) => InitDecl(is_static, applySubstBlock.applySubst(m,blk))
            case MemberDecl_(member) => MemberDecl_(applySubstMemberDecl.applySubst(m, member))
        }
    }

    given applySubstMemberDecl:NameSubst[MemberDecl] = new NameSubst[MemberDecl] {
        def applySubst(m: Map[Name, Name], a: MemberDecl): MemberDecl = a match {
            case ConstructorDecl(modifiers, type_params, id, formal_parms, ex_types, body) => {
                val m1 = m - Name(List(id))
                val fp_ids = formal_parms.map(idFromFormalParam(_))
                val fp_names = fp_ids.map(id => Name(List(id)))
                val m2 = m1 -- fp_names
                ConstructorDecl(modifiers, type_params, id, formal_parms, ex_types, applySubstConstructorBody.applySubst(m2, body))
            } 
            case FieldDecl(modifiers, ty, var_decls) => FieldDecl(modifiers, ty, var_decls.map(applySubstVarDecl.applySubst(m,_)))
            case MemberClassDecl(class_decl) => MemberClassDecl(applySubstClassDecl.applySubst(m, class_decl))
            case MemberInterfaceDecl(iface_decl) => MemberInterfaceDecl(applySubstInterfaceDecl.applySubst(m,iface_decl))
            case MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body) => { // TODO: what is this exp
                val m1 = m - Name(List(id))
                val fp_ids = formal_params.map(idFromFormalParam(_))
                val fp_names = fp_ids.map(id => Name(List(id)))
                val m2 = m1 -- fp_names
                MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp.map(applySubstExp.applySubst(m2,_)), applySubstMethodBody.applySubst(m2, body))
            }
        }
    }

    given applySubstMethodBody:NameSubst[MethodBody] = new NameSubst[MethodBody] {
        def applySubst(m: Map[Name, Name], a: MethodBody): MethodBody = a match {
            case MethodBody(block) => MethodBody(block.map(applySubstBlock.applySubst(m, _)))
        }
    }

    given applySubstInterfaceDecl:NameSubst[InterfaceDecl] = new NameSubst[InterfaceDecl] {
        def applySubst(m: Map[Name, Name], a: InterfaceDecl): InterfaceDecl = a match {
            case InterfaceDecl(kind, modifiers, id, type_params, ref_types, body) => {
                val m1 = m - Name(List(id))
                InterfaceDecl(kind, modifiers, id, type_params, ref_types, applySubstInterfaceBody.applySubst(m, body))
            }
        }
    }

    given applySubstInterfaceBody:NameSubst[InterfaceBody] = new NameSubst[InterfaceBody] {
        def applySubst(m: Map[Name, Name], a: InterfaceBody): InterfaceBody = a match {
            case InterfaceBody(member_decls) => InterfaceBody(member_decls.map(applySubstMemberDecl.applySubst(m,_)))
        }
    }

    given applySubstConstructorBody:NameSubst[ConstructorBody] = new NameSubst[ConstructorBody] {
        def applySubst(m: Map[Name, Name], a: ConstructorBody): ConstructorBody = a match {
            case ConstructorBody(expl_Constr_inv, blk_stmts) => ConstructorBody(expl_Constr_inv.map(applySubstExplConstrInv.applySubst(m,_)), blk_stmts.map(applySubstBlockStmt.applySubst(m,_)))
        }
    }

    given applySubstExplConstrInv:NameSubst[ExplConstrInv] = new NameSubst[ExplConstrInv] {
        def applySubst(m: Map[Name, Name], a: ExplConstrInv): ExplConstrInv = a match {
            case PrimarySuperInvoke(exp, ref_type, args) => PrimarySuperInvoke(applySubstExp.applySubst(m,exp), ref_type, args.map(applySubstExp.applySubst(m,_)))
            case SuperInvoke(ref_type, args) => SuperInvoke(ref_type, args.map(applySubstExp.applySubst(m,_)))
            case ThisInvoke(ref_types, args) => ThisInvoke(ref_types, args.map(applySubstExp.applySubst(m,_)))
        }
    }


    given applySubstStmt:NameSubst[Stmt] = new NameSubst[Stmt] {
        def applySubst(m:Map[Name, Name],stmt:Stmt) :Stmt = stmt match { 
            case Assert(exp, msg) => Assert(applySubstExp.applySubst(m, exp), msg) 
            case BasicFor(init, loop_cond, post_update, stmt) => null // TODO 
            case Break(id) => Break(id)
            case Continue(id) => Continue(id)
            case Do(stmt, exp) => Do(applySubst(m,stmt), applySubstExp.applySubst(m,exp))
            case Empty => Empty
            case EnhancedFor(modifiers, ty, id, exp, stmt) => {
                val toExclude = Name(List(id))
                val m1 = m - toExclude
                EnhancedFor(modifiers, ty, id, applySubstExp.applySubst(m1, exp), applySubst(m1, stmt))
            }
            case ExpStmt(exp) => ExpStmt(applySubstExp.applySubst(m, exp))
            case IfThen(exp, stmt) => IfThen(applySubstExp.applySubst(m, exp), applySubst(m, stmt))
            case IfThenElse(exp, then_stmt, else_stmt) => 
                IfThenElse(applySubstExp.applySubst(m, exp), applySubst(m, then_stmt), applySubst(m, else_stmt))
            case Labeled(id, stmt) => Labeled(id, applySubst(m, stmt))
            case Return(exp) => Return(exp.map(applySubstExp.applySubst(m, _)))
            case StmtBlock(blk) => StmtBlock(applySubstBlock.applySubst(m, blk))
            case Switch(exp, blocks) => Switch(applySubstExp.applySubst(m, exp), blocks.map(applySubstSwitchBlock.applySubst(m,_)))
            case Synchronized(exp, blk) => Synchronized(applySubstExp.applySubst(m, exp), applySubstBlock.applySubst(m, blk))
            case Throw(exp) => Throw(applySubstExp.applySubst(m, exp))
            case Try(try_blk, catches, finally_blk) => Try( applySubstBlock.applySubst(m, try_blk), catches.map(applySubstCatch.applySubst(m, _)), finally_blk.map(applySubstBlock.applySubst(m,_)))
            case While(exp, stmt) => While(applySubstExp.applySubst(m, exp), applySubstStmt.applySubst(m, stmt))
        }
    }

    given applySubstExp:NameSubst[Exp] = new NameSubst[Exp] { 
        def applySubst(m:Map[Name, Name], exp:Exp):Exp = exp match { 
            case ArrayAccess(idx) => ArrayAccess(applySubstArrayIndex.applySubst(m, idx))
            case ArrayCreate(ty, exps, num_dims) => ArrayCreate(ty, exps.map(applySubstExp.applySubst(m,_)), num_dims)
            case ArrayCreateInit(ty, size, init) => ArrayCreateInit(ty, size, applySubstArrayInit.applySubst(m, init))
            case Assign(lhs, op, rhs) => Assign(applySubstLhs.applySubst(m,lhs), op, applySubst(m, rhs))
            case BinOp(e1, op, e2) => BinOp(applySubst(m, e1), op, applySubst(m, e2))
            case Cast(ty, exp) => Cast(ty, applySubst(m,exp))
            case ClassLit(ty) => ClassLit(ty)
            case Cond(cond, true_exp, false_exp) => Cond(applySubst(m, cond), applySubst(m, true_exp), applySubst(m, false_exp))
            case ExpName(name) => {
                m.get(name) match {
                    case None => ExpName(name) 
                    case Some(value) => ExpName(value)
                }
            }
            case FieldAccess_(access) => FieldAccess_(applySubstFieldAccess.applySubst(m, access))
            case InstanceCreation(type_args, type_decl, args, body) => 
                InstanceCreation(type_args, type_decl, args.map(applySubstExp.applySubst(m,_)), body.map(applySubstClassBody.applySubst(m,_)))

            case InstanceOf(e, ref_type) => InstanceOf(applySubstExp.applySubst(m,e), ref_type)
            case Lambda(params, body) => { 
                val lp_ids = idsFromLambdaParams(params)
                val fp_names = lp_ids.map(id => Name(List(id)))
                val m1 = m -- fp_names
                Lambda(params, applySubstLambdaExpression.applySubst(m,body))
            }
            case Lit(lit) => Lit(lit) 
            case MethodInv(methodInv) => MethodInv(applySubstMethodInvocation.applySubst(m, methodInv)) // TODO: check
            case MethodRef(name, id) => MethodRef(name, id) // TODO: check what is method ref?
            case PostDecrement(exp) => PostDecrement(applySubstExp.applySubst(m,exp))
            case PostIncrement(exp) => PostIncrement(applySubstExp.applySubst(m,exp)) 
            case PreBitCompl(exp) => PreBitCompl(applySubstExp.applySubst(m,exp))
            case PreDecrement(exp) => PreDecrement(applySubstExp.applySubst(m,exp))
            case PreIncrement(exp) => PreIncrement(applySubstExp.applySubst(m,exp))
            case PreMinus(exp) => PreMinus(applySubstExp.applySubst(m,exp))
            case PreNot(exp) => PreNot(applySubstExp.applySubst(m,exp))
            case PrePlus(exp) => PrePlus(applySubstExp.applySubst(m,exp))
            case QualInstanceCreation(exp, type_args, id, args, body) => 
                QualInstanceCreation(applySubstExp.applySubst(m,exp), type_args, id, args.map(applySubstExp.applySubst(m,_)), body.map(applySubstClassBody.applySubst(m,_)))
            case This => This
            case ThisClass(name) => ThisClass(name) 
        }
    }

    given applySubstMethodInvocation:NameSubst[MethodInvocation] = new NameSubst[MethodInvocation] { 
        def applySubst(m: Map[Name, Name], a: MethodInvocation): MethodInvocation = a match {
            case ClassMethodCall(name, ref_types, id, args) => ClassMethodCall(name, ref_types, id, args.map(applySubstExp.applySubst(m,_)))
            case MethodCall(name, args) => MethodCall(name, args.map(applySubstExp.applySubst(m,_)))
            case PrimaryMethodCall(e, ref_types, id, args) => PrimaryMethodCall(applySubstExp.applySubst(m,e), ref_types, id, args.map(applySubstExp.applySubst(m,_)))
            case SuperMethodCall(ref_types, id, args) => SuperMethodCall(ref_types, id, args.map(applySubstExp.applySubst(m,_)))
            case TypeMethodCall(name, ref_types, id, args) => TypeMethodCall(name, ref_types, id, args.map(applySubstExp.applySubst(m,_)))
        }
    }

    given applySubstLambdaExpression:NameSubst[LambdaExpression] = new NameSubst[LambdaExpression] {
        def applySubst(m: Map[Name, Name], a: LambdaExpression): LambdaExpression = a match {
            case LambdaBlock(blk) => LambdaBlock(applySubstBlock.applySubst(m, blk))
            case LambdaExpression_(e) => LambdaExpression_(applySubstExp.applySubst(m, e))
        }
    }




    given applySubstLhs:NameSubst[Lhs] = new NameSubst[Lhs] {
        def applySubst(m: Map[Name, Name], a: Lhs): Lhs = a match {
            case ArrayLhs(array_idx) => ArrayLhs(applySubstArrayIndex.applySubst(m, array_idx))
            case FieldLhs(field_access) => FieldLhs(applySubstFieldAccess.applySubst(m, field_access))
            case NameLhs(name) => { 
                m.get(name) match {
                    case None => NameLhs(name)
                    case Some(value) => NameLhs(value)
                }
            }
        }
    }

    given applySubstFieldAccess:NameSubst[FieldAccess] = new NameSubst[FieldAccess] {
        def applySubst(m: Map[Name, Name], a: FieldAccess): FieldAccess = a match {
            case ClassFieldAccess(name, id) => ClassFieldAccess(name, id) // the name is a class name, not a variable name.
            case PrimaryFieldAccess(e, id) => PrimaryFieldAccess(applySubstExp.applySubst(m, e), id)
            case SuperFieldAccess(id) => SuperFieldAccess(id) 
        }
    }

    given applySubstArrayIndex:NameSubst[ArrayIndex] = new NameSubst[ArrayIndex] {
        def applySubst(m: Map[Name, Name], a: ArrayIndex): ArrayIndex = a match  {
            case ArrayIndex(e, es) => ArrayIndex(applySubstExp.applySubst(m, e), es.map(applySubstExp.applySubst(m, _)))
        }
    }

    given applySubstSwitchBlock:NameSubst[SwitchBlock] = new NameSubst[SwitchBlock] {
        def applySubst(m:Map[Name, Name], sb:SwitchBlock):SwitchBlock = sb match {
            case SwitchBlock(label, blk_stmts) => SwitchBlock(label, blk_stmts.map(applySubstBlockStmt.applySubst(m, _)))
        }
    }

    given applySubstCatch:NameSubst[Catch] = new NameSubst[Catch] {
        def applySubst(m: Map[Name, Name], a: Catch): Catch = a match {
            case Catch(params, blk) => params match {
                case FormalParam(modifiers, ty, has_arity, var_decl_id) => {
                    val id = idFromVarDeclId(var_decl_id) 
                    val toExclude = Name(List(id))
                    val m1 = m - toExclude
                    Catch(params, applySubstBlock.applySubst(m1, blk))
                }
            } 
        }
    }

    given applySubstForInit:NameSubst[ForInit] = new NameSubst[ForInit] { 
        def applySubst(m:Map[Name, Name], init:ForInit):ForInit = init match {
            case ForInitExps(exps) => ForInitExps(exps.map(e => applySubstExp.applySubst(m, e)))
            case ForLocalVars(modifiers, ty, var_decls) => ForLocalVars(modifiers, ty, var_decls.map(applySubstVarDecl.applySubst(m, _) ))
        }
    }

    given applySubstVarDecl:NameSubst[VarDecl] = new NameSubst[VarDecl] { 
        def applySubst(m:Map[Name, Name], vd:VarDecl):VarDecl = vd match {
            case VarDecl(id, var_init) => { 
                val toExclude = Name(List(idFromVarDeclId(id)))
                val m1 = m - toExclude
                VarDecl(id, var_init.map(applySubstVarInit.applySubst(m1,_)))
            }
        }
    }

    given applySubstVarInit:NameSubst[VarInit] = new NameSubst[VarInit] {
        def applySubst(m:Map[Name, Name], vi:VarInit):VarInit = vi match {
            case InitArray(array_init) => InitArray(applySubstArrayInit.applySubst(m, array_init))
            case InitExp(exp) => InitExp(applySubstExp.applySubst(m, exp))
        }
    }

    given applySubstArrayInit:NameSubst[ArrayInit] = new NameSubst[ArrayInit] { 
        def applySubst(m:Map[Name, Name], ai:ArrayInit):ArrayInit = ai match  {
            case ArrayInit(var_inits) => ArrayInit(var_inits.map( applySubstVarInit.applySubst(m,_)))
        }
    }
}

