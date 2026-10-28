package obsidian.lang.java.obsidian

import cats.*
import cats.implicits.*
import cats.data.StateT
import scala.annotation.tailrec
import scala.collection.immutable

// constructing CPS from SSA
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.obsidian.ASTUtils.{given, *}
import obsidian.lang.java.obsidian.Common.*
import obsidian.lang.java.obsidian.MinSSA.{SSABlock, SSAStmt, SSAIf, SSAWhile, SSATry, SSAAssert, SSAAssignments, /* SSABreak, SSAContinue, */ SSAEmpty, charcode, Label, TCtx, Phi}
import obsidian.lang.java.obsidian.MinSSA.SSAVarDecls

import obsidian.lang.java.obsidian.CPSFixture.* 
object CPS {

    /**
      * the monad state for CPS conversion
      *
      * @param chararray - the character array for generating id from context, passed from the SSA generation step
      */
    case class State(
        cid: Option[Ident], // context class var id 
        cClassid: Option[Ident], // context class name 
        localVarMap: Map[Name, Name], // list of all local vars
        chararray: List[Char]
    )

    type ErrorM = String


    def initState(carr:List[Char]):State = State(None, None, Map(), carr)


    enum CPSResult[+A] {
        case CPSError(msg:ErrorM) extends CPSResult[Nothing]
        case CPSOk[A](result:A) extends CPSResult[A]
    }
    
    import CPSResult.*

    given cpsResultFunctor:Functor[CPSResult] = new Functor[CPSResult] {
        def map[A, B](fa: CPSResult[A])(f: A => B): CPSResult[B] =
        fa match {
            case CPSError(s) => CPSError(s)
            case CPSOk(a) => CPSOk(f(a))
        }
    }
    

    given cpsResultApplicative: ApplicativeError[CPSResult, ErrorM] = 
        new ApplicativeError[CPSResult, ErrorM] {
        override def ap[A, B](ff: CPSResult[A => B])(fa: CPSResult[A]): CPSResult[B] =
            ff match {
            case CPSOk(f) =>
                fa match {
                case CPSOk(a) => CPSOk(f(a))
                case CPSError(s) => CPSError(s)
                }
            case CPSError(s) => CPSError(s)
            }

        override def pure[A](a: A): CPSResult[A] = CPSOk(a)

        override def raiseError[A](e: ErrorM): CPSResult[A] = CPSError(e)

        override def handleErrorWith[A](fa: CPSResult[A])(f: ErrorM => CPSResult[A]): CPSResult[A] =
            fa match {
            case CPSError(s) => f(s)
            case CPSOk(a) => CPSOk(a)
            }
        }

    given cpsResultMonadError(using app:ApplicativeError[CPSResult, ErrorM]):MonadError[CPSResult, ErrorM] = {
        new MonadError[CPSResult, ErrorM] {
        override def raiseError[A](e: ErrorM): CPSResult[A] = app.raiseError(e)

        override def handleErrorWith[A](fa: CPSResult[A])(f: ErrorM => CPSResult[A]): CPSResult[A] = app.handleErrorWith(fa)(f)

        override def flatMap[A, B](fa: CPSResult[A])(f: A => CPSResult[B]): CPSResult[B] =
            fa match {
            case CPSOk(a) => f(a)
            case CPSError(s) => CPSError(s)
            }

        override def pure[A](a: A): CPSResult[A] = app.pure(a)

        @annotation.tailrec
        def tailRecM[A, B](init: A)(fn: A => CPSResult[Either[A, B]]): CPSResult[B] =
            fn(init) match {
            case CPSError(msg) => CPSError(msg)
            case CPSOk(Right(b)) => CPSOk(b)
            case CPSOk(Left(a)) => tailRecM(a)(fn)
            }
        }
    }

    type SState[S,A] = StateT[CPSResult, S, A]
    type CPSState[A] = SState[State, A]

    def get:SState[State, State] = StateT { state => CPSOk((state, state))} 

    def put(st:State):SState[State, Unit] = StateT { _ => CPSOk((st,()))}     


    def getCtxtId:SState[State, Option[Ident]] = for {
        st <- get 
    } yield st.cid

    def getCtxtIdPrefix:SState[State, List[Ident]] = for {
        oid <- getCtxtId
        pf = oid match {
            case None => Nil 
            case Some(id) => List(id)
        }
    } yield pf


    def getCtxtClassId:SState[State, Option[Ident]] = for {
        st <- get 
    } yield st.cClassid

    def getCtxtClassIdPrefix:SState[State, List[Ident]] = for {
        oid <- getCtxtClassId
        pf = oid match {
            case None => Nil 
            case Some(id) => List(id)
        }
    } yield pf

    def setCtxtId(id:Ident):SState[State, Unit] = for {
        st <- get
        st1 = st match  {
            case State(cid, classId, vars, chararray) =>  State(Some(id), classId, vars, chararray)
        }
        _ <- put(st1)
    } yield ()

    def setCtxtClassId(id:Ident):SState[State, Unit] = for {
        st <- get
        st1 = st match  {
            case State(cid, classId, vars, chararray) =>  State(cid, Some(id), vars, chararray)
        }
        _ <- put(st1)
    } yield ()

    def getLocalVarsMap:SState[State, Map[Name, Name]] = for { 
        st <- get 
        m = st match {
            case State(cid, classId, localVarMap, chararray) => localVarMap
        }
    } yield m

    def setLocalVarsMap(m:Map[Name, Name]):SState[State, Unit] = for {
        st <- get 
        st1 = st match {
            case State(cid, classId, localVarMap, chararray) => State(cid, classId, m, chararray)
        }
        _ <- put(st1)
    } yield () 

    type VarDecl = BlockStmt // actually must be LocalVars(mods, ty, var_decls)


    /**
     * convert a SSA Method Declration into an obfuscated CPS function 
     * 
     * */
    def cpsmethoddecl(methd:MinSSA.SSAMethodDecl)(using m:MonadError[CPSState,ErrorM]):SState[State, MemberDecl] = methd match {
        case MinSSA.SSAMethodDecl(
            modifiers, // List[Modifier],
            type_params,  // List[TypeParam],
            oty, // : Option[Type], 
            id, // : Ident, 
            formal_params, // : List[FormalParam], 
            ex_types,  // : List[ExceptionType], 
            exp, // : Option[Exp],
            body // : SSAMethodBody
        ) => {
            // no need to rename input parameter?
            val idcps = appIdStr(id, "cps")
            val mods  = List()
            val (in_typs,in_args) = extractFormParams(formal_params)
            val ret_typ  = oty match { // ret_type
                case None => voidType
                case Some(ty) => ty
            }
            // Ctxt class 
            val ctxtId = Ident("Ctxt")
            val ctxtVarId = Ident("ctxt")

            // (Exception => Void) => (re_typ => Void) => Void
            val exceptVoidArrTpVoidArrVoid = exceptVoidArrTVoidArrVoid(ret_typ)
            // ity1 -> ... ityn -> (Exception => Void) => (re_typ => Void) => Void
            val itysArrExceptVoidArrTpVoidArrVoid = curryType(in_typs, exceptVoidArrTpVoidArrVoid)
            // (some inner cps decl,  raise -> k -> {E(raise)(()->k(res))} )
            for {
                _ <- setCtxtId(ctxtVarId)
                _ <- setCtxtClassId(ctxtId)
                (local_var_decls, inner_cps_decls, lamb) <- body match {
                    case MinSSA.SSAMethodBody(blks) => {
                        // separating the variable declaration blocks from the non 
                        // variable declraation blocks
                        // variable declaration block statement
                        val vardecls = blks.flatMap(ssablk => ssablk match {
                            case SSABlock(label, stmts) => stmts.map( stmt => stmt match {
                                case SSAVarDecls(mods, ty, varDecls) => List(LocalVars(mods, ty, varDecls))
                                case _ => Nil
                            })
                        })
                        // non variable declaration block statement
                        val notVarDecls = blks.flatMap(ssablk => ssablk match {
                            case SSABlock(label, stmts) => {
                                val stmtsp = stmts.filter(s => s match {
                                    case SSAVarDecls(mods, ty, varDecls) => false
                                    case _ => true
                                })
                                if (stmtsp.isEmpty) { Nil }
                                else {List(SSABlock(label, stmtsp))} 
                            }
                        })
                        val localVarDecls = vardecls.flatten
                        // the localvars map for the monad environment, it will be used in cpsexp and cpsstmt 
                        // the mapping environment is from var_id (without ctxt. prefix) to ctxt.var_id
                        val localVarsMap:Map[Name, Name] = localVarDecls.flatMap((decl:BlockStmt) => decl match {
                            case LocalVars(modifiers, ty, var_decls) => {
                                var_decls.map(var_decl => var_decl match {
                                    case obsidian.lang.java.scalangj.Syntax.VarDecl(vid, var_init) => {
                                        val id = idFromVarDeclId(vid)
                                        (Name(List(id)), Name(List(ctxtVarId, id)))
                                    }
                                })
                            }
                            case _ => Nil
                        }).toMap
                        // convert the non variable declaration block statement
                        for {
                            _ <- setLocalVarsMap(localVarsMap)
                            (decs, exp) <- cpsblk(notVarDecls, Nil)
                        } yield (localVarDecls, decs, 
                            Lambda(LambdaSingleParam(raiseIdent), LambdaExpression_(
                                Lambda(LambdaSingleParam(kIdent), LambdaExpression_(
                                    eapply(eapply(exp,ExpName(Name(List(raiseIdent)))), thunk(eapply(ExpName(Name(List(kIdent))), ExpName(Name(List(ctxtVarId, resIdent))))) // TODO: check apply ctxt.res or null?
                                )))
                            ))
                        )
                    }
                }
                // x1 -> ... xn -> raise -> k -> {E(raise)(()->k(res))}
                bodyp = curryLamb(in_args, lamb)
                // the main CPS function m_cps's declaration
                main_cps_decl  = LocalVars(mods, itysArrExceptVoidArrTpVoidArrVoid, 
                                    List(VarDecl(VarId(idcps), Some(InitExp(bodyp)))))
                // t' res ; Exception ex
                // move this into the nested ctxt class
                ret_and_except_decls = List(
                    LocalVars(mods, ret_typ, List(VarDecl(VarId(resIdent), None))),
                    LocalVars(mods, exceptType, List(VarDecl(VarId(exIdent), None))))
                // class Ctxt { ... }
                ctxtClassDef = LocalClass(mkCtxtClass(ctxtId, local_var_decls, ret_and_except_decls))
                // Ctxt ctxt = new Ctxt();
                ctxtClassVarDecl = LocalVars(mods, RefType_(ClassRefType(ClassType(List((ctxtId, Nil))))), 
                        List(VarDecl(VarId(ctxtVarId), Some(InitExp(InstanceCreation(
                            Nil,TypeDeclSpecifier_(ClassType(List((ctxtId, Nil)))) ,Nil, None
                        ))))))
                appStmt = {
                    // M_cps.apply(arg1).apply(arg2)...
                    val mcps_app_in_args = curryApply(ExpName(Name(List(idcps))), in_args.map(a=>ExpName(Name(List(a)))))
                    val e = eapply(mcps_app_in_args, ExpName(Name(List(ctxtId, idHandlerIdent))))
                    // r -> { res = r; return;}
                    val f = Lambda(LambdaSingleParam(Ident("r")), LambdaBlock(Block(List(
                        BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(ctxtVarId, resIdent))), EqualA, ExpName(Name(List(Ident("r"))))))),
                        BlockStmt_(Return(Some(nullExp)))
                    ))))
                    val d = eapply(e, f)
                    BlockStmt_(ExpStmt(d))
                }
                bodypp = MethodBody(Some(Block( List(ctxtClassDef, ctxtClassVarDecl) ++ inner_cps_decls ++ List(main_cps_decl) ++  List(
                    appStmt, 
                    BlockStmt_(Return(Some(ExpName(Name(List(ctxtVarId, resIdent)))))) // return res
                )))) // TODO: fixme
            
            } yield MethodDecl(modifiers, type_params, oty, id, formal_params, ex_types, exp, bodypp)
        }
    }

    /**
     * Create a nested context class
     *   1. convert the local variable declarations found in the obfuscated function into the member field declarations of 
     * the nested context class. 
     *  @param classId - the name of the nested context class 
     *  @param localVars - the list of local variable declaration found in the obfuscated function (where this class should be nested in)
     *  @param res_and_except - the result and exception variable declaration of the obfuscated function 
     * 
     *  @return the nested context class declaration 
     */ 
    def mkCtxtClass(classId:Ident, localVars:List[BlockStmt], res_and_except:List[BlockStmt]):ClassDecl = {
        val classMods = Nil
        val classTyParams = Nil
        val refType = None
        val refTypes = Nil
        val decls =  (localVars ++ res_and_except).flatMap ( (bstmt:BlockStmt) => bstmt match {
            case LocalVars(mods, refType, vardecls) => List(MemberDecl_(FieldDecl(mods, refType, vardecls)))
            case _ => Nil
        }) ++ List(id_cps, id_handler_cps, loop_cps, seq_cps, ifelse_cps)
        val body = ClassBody(decls)
        ClassDecl_(classMods, classId, classTyParams,refType, refTypes, body) // fix me
    }

    def cpsbody(body:MinSSA.SSAMethodBody)(using m:MonadError[CPSState,ErrorM]):SState[State, MethodBody] = body match {
        case MinSSA.SSAMethodBody(blks) => for {
            (decls, exp) <- cpsblk(blks, Nil)
            blkStmts     <- m.pure(decls ++ List(BlockStmt_(ExpStmt(exp))))
        } yield MethodBody(Some(Block(blkStmts)))
    }

    /**
     * convert a non variable declaration sequence of SSA blocks into CPS.
     *  @param blks - the sequence of blocks
     *  @param phiK - the phi assignments (beta) for the normal flow at the exit of the sequence, 
     *               they are applied at the last block of the sequence (which is labelled by the exit context), 
     *               or passed into the nested continuation (the next blocks) of a control flow statement.
     *  @return a CPS expression of the type raise -> k -> Void
     * 
     * note. the exception flow phis (the exceptPhis of SSAIf/SSAWhile/SSATry and the catchPhis of SSATry) 
     * are treated as the identity for now: the try-catch statement is not supported by the CPS stage yet, 
     * an exception escapes an obfuscated method through the entry exception continuation Ctxt.idHandler 
     * (the identity handler), and no exception handler can observe the renamed variables on the exception path. 
     * */
    def cpsblk(blks:List[SSABlock], phiK:List[Phi])(using m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = blks match {
        case Nil => cpsk(Nil, MinSSA.TBox) // an empty sequence, a nop continuation. 
        case List(SSABlock(lbl, stmts)) => cpsSBlk(lbl, stmts, phiK)
        case (SSABlock(lbl, stmts))::rest => for {
            (declh, eh) <- cpsSBlk(lbl, stmts, Nil)
            (declt, et) <- cpsblk(rest, phiK)
            classpf     <- getCtxtClassIdPrefix
        } yield (declh ++ declt, seq(eh, et, classpf))
    }

    /**
     * convert the SSA statements of one block.
     *
     * @param lbl - the label of the block. when the block is the last one of a sequence of blocks, 
     *              it labels the exit context of the sequence, which is the context to look up in 
     *              the rhs of a phi assignment in phiK.
     * @param stmts - the SSA statements of the block
     * @param phiK - the normal flow phis to be applied at the exit of the block.
     * */
    def cpsSBlk(lbl:Label, stmts:List[SSAStmt], phiK:List[Phi])(using m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = stmts match {
        case Nil => cpsk(phiK, lbl)

        case (MinSSA.SSAVarDecls(_, _, varDecls)::rest) => 
            // the variables are declared at the entry of the obfuscated method (see MinSSA.genVarDecls), 
            // a declaration with an initialiser becomes an assignment here. 
            val assigns:List[Stmt] = varDecls.flatMap(
                (vd:obsidian.lang.java.scalangj.Syntax.VarDecl) => vd match {
                    case obsidian.lang.java.scalangj.Syntax.VarDecl(VarId(id), Some(InitExp(e))) => List(ExpStmt(Assign(NameLhs(Name(List(id))), EqualA, e)))
                    case obsidian.lang.java.scalangj.Syntax.VarDecl(VarId(_), _)                => Nil
                    case _                                                                      => Nil // array declaration initialiser, TODO 
                })
            if (assigns.isEmpty) { cpsSBlk(lbl, rest, phiK) }
            else                 { cpsSBlk(lbl, MinSSA.SSAAssignments(assigns)::rest, phiK) }

        case List(MinSSA.SSAEmpty) => cpsk(phiK, lbl) // the end of a sequence of blocks, the normal flow phis are applied here. 

        case List(MinSSA.SSAAssert(e, msg)) => for {
            e1          <- cpsexp(e)
            msg1        <- msg.traverse(cpsexp)
            (decl, exp) <- cpsMethod(lbl, List(Assert(e1, msg1)), phiK)
        } yield (decl, exp)

        case List(MinSSA.SSAAssignments(stmts)) => for {
            stmtsp      <- cpsstmts(stmts)
            (decl, exp) <- cpsMethod(lbl, stmtsp, phiK)
        } yield (decl, exp)

        case List(MinSSA.SSAExps(stmts)) => for {
            stmtsp      <- cpsstmts(stmts)
            (decl, exp) <- cpsMethod(lbl, stmtsp, phiK)
        } yield (decl, exp)

        case List(MinSSA.SSAThrow(e)) => for {
            exp     <- cpsexp(e)
            mods    <- m.pure(List())
            mkl     <- mkId("mr", lbl)
            // the exception flow phis are the identity for now, the exception simply escapes through raise. 
            body    <- m.pure(Block(List(Return(Some(MethodInv(MethodCall(Name(List(raiseIdent, Ident("apply"))), List(exp)))))).map(BlockStmt_(_))))
            decl    <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                    List(VarDecl(VarId(mkl), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                                LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
        } yield (List(decl), ExpName(Name(List(mkl))))

        case List(MinSSA.SSAReturn(oe)) => for {
            ctxtVarPref  <- getCtxtIdPrefix
            resAsmts     <- oe match {
                case Some(e) => for {
                    exp  <- cpsexp(e)
                } yield List(ExpStmt(Assign(NameLhs(Name(ctxtVarPref ++ List(resIdent))), EqualA, exp)))
                case None    => m.pure(Nil)
            }
            mods         <- m.pure(List())
            ml           <- mkId("m", lbl)
            body         <- m.pure(Block((resAsmts ++ List(Return(Some(MethodInv(PrimaryMethodCall(ExpName(Name(List(kIdent))), Nil, Ident("apply") , List(nullExp))))))).map(BlockStmt_(_))))
            decl         <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                    List(VarDecl(VarId(ml), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                                LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
        } yield (List(decl), ExpName(Name(List(ml))))

        case List(MinSSA.SSAIf(e, thBlks, elBlks, joinPhis, nextBlks, exceptPhis)) => for {
            // the branches end with SSAEmpty blocks labelled by the exit contexts of the branches, 
            // the join phis are resolved and applied there. 
            (declth, eth) <- cpsblk(thBlks, joinPhis)
            (declel, eel) <- cpsblk(elBlks, joinPhis)
            // the next blocks are the continuation of the enclosing sequence, they carry the enclosing phis. 
            (decln, en)   <- cpsblk(nextBlks, phiK)
            exp           <- cpsexp(e)
            classpf       <- getCtxtClassIdPrefix
        } yield (declth ++ declel ++ decln, seq(ifelse(exp, eth, eel, classpf), en, classpf))

        case List(MinSSA.SSAWhile(joinPhis, e, bodyBlks, nextBlks, exceptPhis)) => for {
            // the loop entry phis, resolved at the entry. TODO: review the context to resolve the phis 
            // at the loop entry when MinSSA implements the while statement. 
            preOp   <- joinPhis match {
                case Nil => m.pure((List():List[VarDecl], None:Option[Exp]))
                case _   => for {
                    lbl2        <- minlabel(joinPhis)
                    (decl, exp) <- cpsk(joinPhis, lbl2)
                } yield (decl, Some(exp))
            }
            // the body ends with an SSAEmpty block labelled by the exit context of the body, 
            // the back edge phis are resolved and applied there. 
            (declb, eb) <- cpsblk(bodyBlks, joinPhis)
            (decln, en) <- cpsblk(nextBlks, phiK)
            exp         <- cpsexp(e)
            classpf     <- getCtxtClassIdPrefix
        } yield preOp match {
            case (declpre, None)      => (declpre ++ declb ++ decln, loop(exp, eb, en, classpf))
            case (declpre, Some(ep))  => (declpre ++ declb ++ decln, seq(ep, loop(exp, eb, en, classpf), classpf))
        }

        case List(MinSSA.SSATry(tryStmts, catchParam, catchPhis, catchStmts, joinPhis, joinStmts, exceptPhis)) =>
            m.raiseError("SSA to CPS conversion failed, try-catch(-finally) is not supported by the CPS stage yet.")

        case (stmt::rest) => for { // a block with multiple statements (generated by MinSSA.prependVarDecls), chain them. 
            (declh, eh) <- cpsSBlk(lbl, List(stmt), Nil)
            (declt, et) <- cpsSBlk(lbl, rest, phiK)
            classpf     <- getCtxtClassIdPrefix
        } yield (declh ++ declt, seq(eh, et, classpf))

        case _ => m.raiseError(s"SSA to CPS conversion failed, the SSA block at ${lbl} contains unsupported SSA statements: ${stmts}.")
    }

    /**
     * construct a CPS function m_lbl = raise -> k -> { stmts; k.apply(null); }, 
     * and compose it with the normal flow phi assignments at the exit of the block. 
     * */
    def cpsMethod(lbl:Label, stmts:List[Stmt], phiK:List[Phi])(using m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = for {
        mods          <- m.pure(List())
        ml            <- mkId("m", lbl)
        body          <- m.pure(Block((stmts ++ List(Return(Some(MethodInv(PrimaryMethodCall(ExpName(Name(List(kIdent))), Nil, Ident("apply") , List(nullExp))))))).map(BlockStmt_(_))))
        decl          <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                List(VarDecl(VarId(ml), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                            LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
        (declk, ek)   <- cpsk(phiK, lbl)
        classpf       <- getCtxtClassIdPrefix
    } yield (List(decl) ++ declk, seq(ExpName(Name(List(ml))), ek, classpf))

    /**
      * convert the end of a block into the continuation expression.
      *
      * @param phi - the post phi assignments
      * @param lbl - the label of the current block
      * @param m
      * @return
      */
    def cpsk(phi:List[Phi], lbl:Label)(using m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = for {
        xeAsmts      <- cpsphi(phi, lbl)
        mods         <- m.pure(List())
        mkl          <- mkId("mk", lbl)
        // body         <- m.pure(Block((xeAsmts ++ List(Return(Some(MethodInv(MethodCall(Name(List(kIdent)), List())))))).map(BlockStmt_(_))))
        body         <- m.pure(Block((xeAsmts ++ List(Return(Some(MethodInv(PrimaryMethodCall(ExpName(Name(List(kIdent))), Nil, Ident("apply") , List(nullExp))))))).map(BlockStmt_(_))))

        decl         <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                List(VarDecl(VarId(mkl), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                            LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
    } yield (List(decl), ExpName(Name(List(mkl))))
    
    /**
      * convert phi assignment into a sequence of statements based on the input ctxt.
      *
      * @param phis
      * @param ctxt
      * @param m
      * @return
      */
    def cpsphi(phis:List[Phi], ctxt:Label)(using m:MonadError[CPSState, ErrorM]):SState[State, List[Stmt]] = phis.traverse(
        (phi:Phi) => phi match {
            case Phi(srcVar, renVar, rhs) => lookup(rhs,ctxt) match {
                case None => m.raiseError("cpsphi failed: the rhs of the phi assignment does not contain the input ctxt.")
                case Some(n) => for { 
                    pf <- getCtxtIdPrefix
                } yield ExpStmt(Assign(NameLhs(appNamePrefix(pf,renVar)), EqualA, ExpName(appNamePrefix(pf, n))))
            }
        }
    )

    /**
      * convert an expression from SSA to CPS, it should be an identity function.
      *
      * @param e
      * @param m
      * @return
      */
    def cpsexp(e:Exp)(using m:MonadError[CPSState, ErrorM]):SState[State, Exp] = for {
        m <- getLocalVarsMap
        e1 = applySubstExp.applySubst(m,e)
    } yield e1 

    /**
      * convert a list of assignment stmts from SSA to CPS, it should be an identity function
      *
      * @param stmts
      * @param m
      * @return
      */
    // TODO:
    // we need to apply append ctxt variable prefix to all the name everywhere? in e 
    // we need to keep track of the list of local variable names. 
    def cpsstmts(stmts:List[Stmt])(using m:MonadError[CPSState, ErrorM]):SState[State, List[Stmt]] = for {
        m <- getLocalVarsMap
        stmts1 = stmts.map(applySubstStmt.applySubst(m,_))
    } yield stmts1 

    /**
      * create an Identifier by applying the chararray used in SSA phase
      *
      * @param s
      * @param ctxt
      * @param m
      * @return
      */
    def mkId(s:String, ctxt:Label)(using m:MonadError[CPSState, ErrorM]):SState[State, Ident] = for {
        st <- get
        id <- st match {
            case State(cid, classid, vars, chararr) => m.pure(Ident( s + "_" + charcode(ctxt, chararr).mkString))
        }
    } yield (id)

    /**
      * apply seq to both e1 and e2 and the context class name, note seq is a method?
      *
      * @param e1
      * @param e2
      * @param classPrefix
      * @return
      */
    def seq(e1:Exp, e2:Exp, classPrefix:List[Ident]):Exp = 
    {
        val args = List(e1, e2)
        val seqname = Name(classPrefix ++ List(Ident("seq")))
        MethodInv(MethodCall(seqname, args))
    }


    def trycatch(e1:Exp, e2:Exp, classPrefix:List[Ident]):Exp = 
    {
        val args = List(e1, e2)
        val seqname = Name(classPrefix ++ List(Ident("trycatch")))
        MethodInv(MethodCall(seqname, args))
    }

    def ifelse(e1:Exp, e2:Exp, e3:Exp, classPrefix:List[Ident]):Exp = 
    {
        val thunkede1 = thunk(e1) 
        val args = List(thunkede1, e2, e3) 
        val ifelsename = Name(classPrefix ++ List(Ident("ifelse")))
        MethodInv(MethodCall(ifelsename, args))   
    }


    def loop(e1:Exp, e2:Exp, e3:Exp, classPrefix:List[Ident]):Exp = 
    {
        val thunkede1 = thunk(e1) 
        val args = List(thunkede1, e2, e3) 
        val loopname = Name(classPrefix ++ List(Ident("loop")))
        MethodInv(MethodCall(loopname, args))   
    }

    // this will return a delayed expression () -> e
    def thunk(e:Exp):Exp = 
    {
        Lambda(LambdaSingleParam(unitIdent), LambdaExpression_(e))
    }

        

    /**
      * find the min label from the rhs of the phi assignments.
      *  assumption, the set of phi assignments should be non empty
      *              the min labels for all phi assignments are the same.
      *              the min labels are the left most item
      * @param phis
      * @param m
      */
    def minlabel(phis:List[Phi])(using m:MonadError[CPSState, ErrorM]):SState[State, Label] = {
        val rhss:List[List[(Label,Name)]] = phis.map(phi => phi match {
            case Phi(srcVar, renVar, rhs) => rhs
        })
        val oplabels:List[Option[Label]] = rhss.map(rhs => rhs match { case Nil => None; case p::ps => Some(p._1)})
        oplabels match {
            case Nil => m.raiseError("minlabel failed: the phi list is empty.")
            case (oplabel::rest) if oplabels.forall( oplabel => !(oplabel.isEmpty)) && allSame(oplabels) => {
                oplabel match {
                    case Some(label) => m.pure(label)
                    case None => m.raiseError("minlabel failed: the phi list's first item is empty, but this should not happen.")
                }
            }
        }
    }


    def appNamePrefix(pf:List[Ident], name:Name):Name = name match {
        case Name(ids) => Name(pf ++ ids)
    }


    val funcIdent = Ident("Function")
    val javaIdent = Ident("java") 
    val utilIdent = Ident("util") 
    val functionIdent = Ident("function") 

    val javautilfunctionPrefix = List( (javaIdent, Nil), (utilIdent, Nil), (functionIdent, Nil))

    // some helper functions to construct types
    // partial function, t1 and t2 must be RefType, otherwise, a corresponding boxed type will be 
    // created to replace t1 (like wise, t2)
    def mkArrType(t1:Type, t2:Type):Type = (t1, t2) match {
        case (RefType_(s1), RefType_(s2)) => 
            RefType_(ClassRefType(ClassType(javautilfunctionPrefix ++ List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        case (PrimType_(p1), PrimType_(p2)) => {
            val s1 = ClassRefType(prim2Class(p1))
            val s2 = ClassRefType(prim2Class(p2))
            RefType_(ClassRefType(ClassType(javautilfunctionPrefix ++ List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        }
        case (RefType_(s1), PrimType_(p2)) => {
            val s2 = ClassRefType(prim2Class(p2))
            RefType_(ClassRefType(ClassType(javautilfunctionPrefix ++ List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        }
        case (PrimType_(p1), RefType_(s2)) => {
            val s1 = ClassRefType(prim2Class(p1))
            RefType_(ClassRefType(ClassType(javautilfunctionPrefix ++ List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        }
    }

    val exceptIdent = Ident("Exception")
    val voidIdent = Ident("Void")
    val raiseIdent = Ident("raise")
    val kIdent = Ident("k")
    val unitIdent = Ident("unit")
    val resIdent = Ident("res")
    val exIdent = Ident("ex")
    val idHandlerIdent = Ident("idHandler")
    // val nullIdent = Ident("null")
    // val nullExp = ExpName(Name(List(nullIdent)))
    val nullExp = Lit(NullLit)

    val exceptRefType = ClassRefType(ClassType(List((exceptIdent, List()))))
    val voidRefType   = ClassRefType(ClassType(List((voidIdent, List()))))
    val voidType      = RefType_(voidRefType)
    val exceptType    = RefType_(exceptRefType)
    // Exception => Void
    // Function<Exception, Void>
    val exceptArrVoid = mkArrType(exceptType,voidType)
    // Void => Void
    // Function<Void, Void>
    val voidArrVoid   = mkArrType(voidType,voidType) 
    // (Exception => Void) => ((Void => Void) => Void)
    // Function<Function<Exception, Void>,  Function<Function<Void, Void>, Void>>
    val exceptVoidArrVoidVoidArrVoid = mkArrType(exceptArrVoid, mkArrType(voidArrVoid, voidType))


    def prim2Class(pt:PrimType):ClassType = pt match {
        case BooleanT => ClassType(List((Ident("Boolean"),Nil)))
        case ByteT    => ClassType(List((Ident("Byte"),Nil)))
        case CharT    => ClassType(List((Ident("Char"),Nil)))
        case DoubleT  => ClassType(List((Ident("Double"),Nil)))
        case FloatT   => ClassType(List((Ident("Float"), Nil)))
        case IntT     => ClassType(List((Ident("Integer"), Nil)))
        case LongT    => ClassType(List((Ident("Long"), Nil)))
        case ShortT   => ClassType(List((Ident("Short"), Nil)))    
    }

    
    def exceptVoidArrTVoidArrVoid(t:Type):Type = { 
        val s = t match {
            case RefType_(r_ty) =>  t
            case PrimType_(p_ty) => RefType_(ClassRefType(prim2Class(p_ty)))
        }
        val sArrVoid   = mkArrType(s,voidType) 
        mkArrType(exceptArrVoid, mkArrType(sArrVoid, voidType))
    }


    // e1.apply(e2) assuming e1 is a lambda expressin
    def eapply(e1:Exp, e2:Exp):Exp = {
        val typ_args = List()
        MethodInv(PrimaryMethodCall(e1,typ_args, Ident("apply"), List(e2)))
    }

    // extract the types and the idents from the formal parameters 
    def extractFormParams(formal_params:List[FormalParam]):(List[Type], List[Ident]) = { 
        formal_params.map( x => x match {
                case FormalParam(modifiers, ty, has_arity, var_decl_id) => (ty,idFromVarDeclId(var_decl_id))
        }).unzip
    }
    // create curry type
    
    def curryType(tys: List[Type], inner_type:Type):Type = tys match {
        case Nil => inner_type
        case (ty::tysp) => mkArrType(ty, curryType(tysp, inner_type))
    }
    
    // create curry lambda exp
    def curryLamb(idents: List[Ident], inner_exp:Exp):Exp = idents match {
        case Nil => inner_exp
        case (ident::identsp) => Lambda(LambdaSingleParam(ident), LambdaExpression_(curryLamb(identsp, inner_exp)))
    }

    // nested eapply a curry lambda exp to a list of args
    def curryApply(exp:Exp, args:List[Exp]):Exp = args match {
        case Nil => exp
        case (arg::argsp) => curryApply(eapply(exp,arg), argsp)
    }


}
