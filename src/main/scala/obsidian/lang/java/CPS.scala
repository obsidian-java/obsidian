package obsidian.lang.java

import cats.*
import cats.implicits.*
import cats.data.StateT
import scala.annotation.tailrec
import scala.collection.immutable

// constructing CPS from SSA
import com.github.luzhuomi.scalangj.Syntax.*
import obsidian.lang.java.ASTUtils.*
import obsidian.lang.java.Common.*
import obsidian.lang.java.MinSSA.{SSABlock, SSAStmt, SSAIf, SSAWhile, SSATry, SSAAssert, SSAAssignments, SSABreak, SSAContinue, SSAEmpty, charcode, Label, TCtx, Phi}
import obsidian.lang.java.MinSSA.SSAVarDecls


object CPS {

    /**
      * the monad state for CPS conversion
      *
      * @param chararray - the character array for generating id from context, passed from the SSA generation step
      */
    case class State(
        chararray: List[Char]
    )

    type ErrorM = String


    def initState(carr:List[Char]):State = State(carr)


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

    type VarDecl = BlockStmt // actually must be LocalVars(mods, ty, var_decls)


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
            // (Exception => Void) => (re_typ => Void) => Void
            val exceptVoidArrTpVoidArrVoid = exceptVoidArrTVoidArrVoid(ret_typ)
            // ity1 -> ... ityn -> (Exception => Void) => (re_typ => Void) => Void
            val itysArrExceptVoidArrTpVoidArrVoid = curryType(in_typs, exceptVoidArrTpVoidArrVoid)
            // (some inner cps decl,  raise -> k -> {E(raise)(()->k(res))} )
            for {
                (inner_cps_decls, lamb) <- body match {
                    case MinSSA.SSAMethodBody(blks) => {
                            val vardecls = blks.flatMap(ssablk => ssablk match {
                                case SSABlock(label, stmts) => stmts.map( stmt => stmt match {
                                    case SSAVarDecls(mods, ty, varDecls) => List(LocalVars(mods, ty, varDecls))
                                    case _ => Nil
                                })
                            })
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
                            for {
                                (decs, exp) <- cpsblk(notVarDecls, Nil, Nil)
                            } yield (vardecls.flatten ++ decs, 
                                Lambda(LambdaSingleParam(raiseIdent), LambdaExpression_(
                                    Lambda(LambdaSingleParam(kIdent), LambdaExpression_(
                                        eapply(eapply(exp,ExpName(Name(List(raiseIdent)))), thunk(eapply(ExpName(Name(List(kIdent))), ExpName(Name(List(resIdent)))))
                                    )))
                                ))
                            )
                    }
                }
                // x1 -> ... xn -> raise -> k -> {E(raise)(()->k(res))}
                bodyp = curryLamb(in_args, lamb)
                // m_cps's decl
                decl  = LocalVars(mods, itysArrExceptVoidArrTpVoidArrVoid, 
                            List(VarDecl(VarId(idcps), Some(InitExp(bodyp)))))
                // t' res ; Exception ex
                declspp = List(
                    LocalVars(mods, ret_typ, List(VarDecl(VarId(resIdent), None))),
                    LocalVars(mods, exceptType, List(VarDecl(VarId(exIdent), None))))
                appStmt = {
                    // M_cps.apply(arg1).apply(arg2)...
                    val mcps_app_in_args = curryApply(ExpName(Name(List(idcps))), in_args.map(a=>ExpName(Name(List(a)))))
                    val e = eapply(mcps_app_in_args, ExpName(Name(List(idraiseIdent))))
                    // r -> { res = r; return;}
                    val f = Lambda(LambdaSingleParam(Ident("r")), LambdaBlock(Block(List(
                        BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(resIdent))), EqualA, ExpName(Name(List(Ident("r"))))))),
                        BlockStmt_(Return(None))
                    ))))
                    val d = eapply(e, f)
                    BlockStmt_(ExpStmt(d))
                }
                bodypp = MethodBody(Some(Block(declspp ++ inner_cps_decls ++ List(decl) ++  List(
                    appStmt, 
                    BlockStmt_(Return(Some(ExpName(Name(List(resIdent)))))) // return res
                )))) // TODO: fixme
            
            } yield MethodDecl(modifiers, type_params, oty, id, formal_params, ex_types, exp, bodypp)
        }
    }

    def cpsbody(body:MinSSA.SSAMethodBody)(using m:MonadError[CPSState,ErrorM]):SState[State, MethodBody] = body match {
        case MinSSA.SSAMethodBody(blks) => for {
            (decls, exp) <- cpsblk(blks, Nil, Nil)
            blkStmts     <- m.pure(decls ++ List(BlockStmt_(ExpStmt(exp))))
        } yield MethodBody(Some(Block(blkStmts)))
    }

    def cpsblk(blks:List[SSABlock], phiK:List[Phi], phiR:List[Phi])(using m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = blks match {
        case List(SSABlock(lbl, stmts)) => stmts match {
            case List(SSAIf(e, blksp, blkspp, phi)) => for {
                (declp, expp)     <- cpsblk(blksp, phi, phiR)
                (declpp, exppp)   <- cpsblk(blkspp, phi, phiR)
                exp               <- cpsexp(e)
                (declppp, expppp) <- cpsk(phiK, lbl)
            } yield ((declp ++ declpp ++ declppp, seq(ifelse( thunk(exp), expp, exppp), expppp)))
            case List(SSAWhile(phi_pre, e, blks, phi_post)) => for {
                lbl2              <- minlabel(phi_pre)
                (decl, exp)       <- cpsk(phi_pre, lbl2)
                expp              <- cpsexp(e)
                (declpp, exppp)   <- cpsblk(blks, phi_pre, phiR)
                (declppp, expppp) <- cpsk(phiK, lbl)

            } yield (decl ++ declpp ++ declppp, seq(exp, loop( thunk(expp), exppp, expppp)))
            case List(MinSSA.SSAThrow(e)) => for {
                exp          <- cpsexp(e)
                xeAsmts      <- cpsphi(phiR,lbl)
                mods         <- m.pure(List())
                mkl          <- mkId("mr", lbl)
                body         <- m.pure(Block((xeAsmts ++ List(Return(Some(MethodInv(MethodCall(Name(List(raiseIdent)), List(exp))))))).map(BlockStmt_(_))))
                decl         <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                        List(VarDecl(VarId(mkl), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                                    LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
            } yield (List(decl), ExpName(Name(List(mkl))))
            case List(MinSSA.SSAReturn(oe)) => for {
                resAsmts     <- oe match {
                    case Some(e) => for {
                        exp  <- cpsexp(e)
                    } yield List(ExpStmt(Assign(NameLhs(Name(List(resIdent))), EqualA, exp)))
                    case None    => m.pure(Nil)
                }
                mods         <- m.pure(List())
                ml           <- mkId("m", lbl)
                body         <- m.pure(Block((resAsmts ++ List(Return(Some(MethodInv(MethodCall(Name(List(kIdent)), List())))))).map(BlockStmt_(_))))
                decl         <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                        List(VarDecl(VarId(ml), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                                    LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
            } yield (List(decl), ExpName(Name(List(ml))))

            case List(MinSSA.SSAAssignments(stmts)) => for {
                stmtsp        <- cpsstmts(stmts) // this should be an identity function
                mods          <- m.pure(List())
                ml            <- mkId("m", lbl)
                body          <- m.pure(Block((stmtsp ++ List(Return(Some(MethodInv(MethodCall(Name(List(kIdent)), List())))))).map(BlockStmt_(_))))
                decl          <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                        List(VarDecl(VarId(ml), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                                    LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
                (declp, expp) <- cpsk(phiK, lbl)
            } yield (List(decl) ++ declp, seq(ExpName(Name(List(ml))), expp))
            

            case List(MinSSA.SSATry(blks, phiRp, catchparams, blksp, phiKp)) => for {
                (decl, exp)       <- cpsblk(blks, phiKp, phiRp)
                (declp, expp)     <- cpsblk(blksp, phiKp, phiR)
                mods              <- m.pure(List())
                params            <- m.pure(List(FormalParam(mods,exceptType,false,VarId(Ident("x")))))
                stmtsp            <- m.pure(List(ExpStmt(Assign(NameLhs(Name(List(Ident("ex")))), EqualA, ExpName(Name(List(Ident("x"))))))))
                body              <- m.pure(Block((stmtsp ++ List(Return(Some(expp)))).map(BlockStmt_(_))))
                exppp             <- m.pure(Lambda(LambdaFormalParams(params), LambdaBlock(body))) 
                (declppp, expppp) <- cpsk(phiK, lbl)
            } yield (decl ++ declp ++ declppp, seq(trycatch(exp, exppp), expppp))
            // todo: more cases here.

            
        }
        case SSABlock(lbl, stmts)::blksppp => stmts match {
            case List(SSAIf(e, blksp, blkspp, phi)) => for {
                (declp, expp)     <- cpsblk(blksp, phi, phiR)
                (declpp, exppp)   <- cpsblk(blkspp, phi, phiR)
                exp               <- cpsexp(e)
                (declppp, expppp) <- cpsblk(blksppp, phiK, phiR)
            } yield ((declp ++ declpp ++ declppp, seq(ifelse( thunk(exp), expp, exppp), expppp)))
            case List(SSAWhile(phi_pre, e, blks, phi_post)) => for {
                lbl2              <- minlabel(phi_pre)
                (decl, exp)       <- cpsk(phi_pre, lbl2)
                expp              <- cpsexp(e)
                (declpp, exppp)   <- cpsblk(blks, phi_pre, phiR)
                (declppp, expppp) <- cpsblk(blksppp, phiK, phiR)
            } yield (decl ++ declpp ++ declppp, seq(exp, loop( thunk(expp), exppp, expppp)))


            case List(MinSSA.SSAAssignments(stmts)) => for {
                stmtsp        <- cpsstmts(stmts) // this should be an identity function
                mods          <- m.pure(List())
                ml            <- mkId("m", lbl)
                body          <- m.pure(Block((stmtsp ++ List(Return(Some(MethodInv(MethodCall(Name(List(kIdent)), List())))))).map(BlockStmt_(_))))
                decl          <- m.pure(LocalVars(mods, exceptVoidArrVoidVoidArrVoid, 
                                        List(VarDecl(VarId(ml), Some(InitExp(Lambda(LambdaSingleParam(raiseIdent), 
                                                    LambdaExpression_(Lambda(LambdaSingleParam(kIdent), LambdaBlock(body))))))))))
                (declp, expp) <- cpsblk(blksppp, phiK, phiR)
            } yield (List(decl) ++ declp, seq(ExpName(Name(List(ml))), expp))


            case List(MinSSA.SSATry(blks, phiRp, catchparams, blksp, phiKp)) => for {
                (decl, exp)       <- cpsblk(blks, phiKp, phiRp)
                (declp, expp)     <- cpsblk(blksp, phiKp, phiR)
                mods              <- m.pure(List())
                params            <- m.pure(List(FormalParam(mods,exceptType,false,VarId(Ident("x")))))
                stmtsp            <- m.pure(List(ExpStmt(Assign(NameLhs(Name(List(exIdent))), EqualA, ExpName(Name(List(Ident("x"))))))))
                body              <- m.pure(Block((stmtsp ++ List(Return(Some(expp)))).map(BlockStmt_(_))))
                exppp             <- m.pure(Lambda(LambdaFormalParams(params), LambdaBlock(body))) 
                (declppp, expppp) <- cpsblk(blksppp, phiK, phiR)
            } yield (decl ++ declp ++ declppp, seq(trycatch(exp, exppp), expppp))

            // todo: more cases here. try?

        }
    }

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
        body         <- m.pure(Block((xeAsmts ++ List(Return(Some(MethodInv(MethodCall(Name(List(kIdent)), List())))))).map(BlockStmt_(_))))
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
                case Some(n) => m.pure(ExpStmt(Assign(NameLhs(renVar), EqualA, ExpName(n))))
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
    def cpsexp(e:Exp)(using m:MonadError[CPSState, ErrorM]):SState[State, Exp] = m.pure(e)


    /**
      * convert a list of assignment stmts from SSA to CPS, it should be an identity function
      *
      * @param stmts
      * @param m
      * @return
      */
    def cpsstmts(stmts:List[Stmt])(using m:MonadError[CPSState, ErrorM]):SState[State, List[Stmt]] = m.pure(stmts)


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
            case State(chararr) => m.pure(Ident( s + "_" + charcode(ctxt, chararr).mkString))
        }
    } yield (id)

    /**
      * apply seq to both e1 and e2, note seq is a method?
      *
      * @param e1
      * @param e2
      * @return
      */
    def seq(e1:Exp, e2:Exp):Exp = 
    {
        val args = List(e1, e2)
        val seqname = Name(List(Ident("seq")))
        MethodInv(MethodCall(seqname, args))
    }


    def trycatch(e1:Exp, e2:Exp):Exp = 
    {
        val args = List(e1, e2)
        val seqname = Name(List(Ident("trycatch")))
        MethodInv(MethodCall(seqname, args))
    }

    def ifelse(e1:Exp, e2:Exp, e3:Exp):Exp = 
    {
        val thunkede1 = thunk(e1) 
        val args = List(thunkede1, e2, e3) 
        val ifelsename = Name(List(Ident("ifelse")))
        MethodInv(MethodCall(ifelsename, args))   
    }


    def loop(e1:Exp, e2:Exp, e3:Exp):Exp = 
    {
        val thunkede1 = thunk(e1) 
        val args = List(thunkede1, e2, e3) 
        val ifelsename = Name(List(Ident("loop")))
        MethodInv(MethodCall(ifelsename, args))   
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


    // some helper functions to construct types
    // partial function, t1 and t2 must be RefType, otherwise, a corresponding boxed type will be 
    // created to replace t1 (like wise, t2)
    def mkArrType(t1:Type, t2:Type):Type = (t1, t2) match {
        case (RefType_(s1), RefType_(s2)) => 
            RefType_(ClassRefType(ClassType(List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        case (PrimType_(p1), PrimType_(p2)) => {
            val s1 = ClassRefType(prim2Class(p1))
            val s2 = ClassRefType(prim2Class(p2))
            RefType_(ClassRefType(ClassType(List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        }
        case (RefType_(s1), PrimType_(p2)) => {
            val s2 = ClassRefType(prim2Class(p2))
            RefType_(ClassRefType(ClassType(List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        }
        case (PrimType_(p1), RefType_(s2)) => {
            val s1 = ClassRefType(prim2Class(p1))
            RefType_(ClassRefType(ClassType(List((funcIdent, List(ActualType(s1),ActualType(s2)))))))
        }
    }
    // val funcIdents = List(Ident("java"), Ident("util"), Ident("function"), Ident("Function"))
    val funcIdent = Ident("Function")

    val exceptIdent = Ident("Exception")
    val voidIdent = Ident("Void")
    val raiseIdent = Ident("raise")
    val kIdent = Ident("k")
    val unitIdent = Ident("unit")
    val resIdent = Ident("res")
    val exIdent = Ident("ex")
    val idraiseIdent = Ident("id_raise")

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

