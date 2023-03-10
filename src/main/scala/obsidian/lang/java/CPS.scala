package obsidian.lang.java


import cats._
import cats.implicits._
import cats.data.StateT
import scala.annotation.tailrec
import scala.collection.immutable

// constructing CPS from SSA
import com.github.luzhuomi.scalangj.Syntax._
import obsidian.lang.java.MinSSA.{SSABlock, SSAStmt, SSAIf, SSAWhile, SSATry, SSAAssert, SSAAssignments, SSABreak, SSAContinue, SSAEmpty, charcode, Label, TCtx, Phi}




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

    sealed trait CPSResult[+A]
    
    case class CPSError(msg:ErrorM) extends CPSResult[Nothing]
  
    case class CPSOk[A](result:A) extends CPSResult[A]

    implicit def cpsResultFunctor: Functor[CPSResult] =
        new Functor[CPSResult] {
        override def map[A, B](fa: CPSResult[A])(f: A => B): CPSResult[B] =
            fa match {
            case CPSError(s) => CPSError(s)
            case CPSOk(a) => CPSOk(f(a))
            }
        }

    implicit def cpsResultApplicative: ApplicativeError[CPSResult, ErrorM] = 
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

    implicit def cpsResultMonadError(implicit app:ApplicativeError[CPSResult, ErrorM]):MonadError[CPSResult, ErrorM] = {
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

    def cpsblk(blks:List[SSABlock], phiK:List[Phi], phiR:List[Phi])(implicit m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = blks match {
        case List(SSABlock(lbl, stmts)) => stmts match {
            case List(SSAIf(e, blksp, blkspp, phi)) => for {
                (declp, expp)     <- cpsblk(blksp, phi, phiR)
                
                (declpp, exppp)   <- cpsblk(blkspp, phi, phiR)
                exp               <- cpsexp(e)
                (declppp, expppp) <- cpsk(phiK, lbl)
            } yield ((declp ++ declpp ++ declppp, seq(ifelse( thunk(exp), expp, exppp), expppp)))
        }
        // todo: more cases here.
    }

    // partial function, t1 and t2 must be RefType. 
    def mkArrType(t1:Type, t2:Type):Option[Type] = (t1, t2) match {
        case (RefType_(s1), RefType_(s2)) => Some(RefType_(ClassRefType(ClassType(List((funcIdent, List(ActualType(s1),ActualType(s2))))))))
        case _ => None
    }
    // val funcIdents = List(Ident("java"), Ident("util"), Ident("function"), Ident("Function"))
    val funcIdent = Ident("Function")

    val exceptIdent = Ident("Exception")
    val voidIdent = Ident("Void")
    val raiseIdent = Ident("raise")
    val kIdent = Ident("k")
    val unitIdent = Ident("unit")

    val exceptRefType = ClassRefType(ClassType(List((exceptIdent, List()))))
    val voidRefType   = ClassRefType(ClassType(List((voidIdent, List()))))
    val voidType      = RefType_(voidRefType)
    val exceptType    = RefType_(exceptRefType)
    // Exception => Void
    // Function<Exception, Void>
    val exceptArrVoid = mkArrType(exceptType,voidType) match { case Some(t) => t }
    // Void => Void
    // Function<Void, Void>
    val voidArrVoid   = mkArrType(voidType,voidType) match {case Some(t) => t }
    // (Exception => Void) => ((Void => Void) => Void)
    // Function<Function<Exception, Void>,  Function<Function<Void, Void>, Void>>
    val exceptVoidArrVoidVoidArrVoid = mkArrType(exceptArrVoid, mkArrType(voidArrVoid, voidType)match {case Some(t) => t}) match {case Some(t) => t}

    
    /**
      * convert the end of a block into the continuation expression.
      *
      * @param phi - the post phi assignments
      * @param lbl - the label of the current block
      * @param m
      * @return
      */
    def cpsk(phi:List[Phi], lbl:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = for {
        xeAsmts      <- cpsphi(phi, lbl)
        mods         <- m.pure(List())
        mkl          <- mkId("m", lbl)
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
    def cpsphi(phis:List[Phi], ctxt:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, List[Stmt]] = phis.traverse(
        (phi:Phi) => phi match {
            case Phi(srcVar, renVar, rhs) => rhs.get(ctxt) match {
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
    def cpsexp(e:Exp)(implicit m:MonadError[CPSState, ErrorM]):SState[State, Exp] = m.pure(e)


    def mkId(s:String, ctxt:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, Ident] = for {
        st <- get
        id <- st match {
            case State(chararr) => m.pure(Ident( s + "_" + charcode(ctxt, chararr).mkString))
        }
    } yield (id)



    // apply seq to both e1 and e2, note seq is a method?
    def seq(e1:Exp, e2:Exp):Exp = 
    {
        val args = List(e1, e2)
        val seqname = Name(List(Ident("seq")))
        MethodInv(MethodCall(seqname, args))
    }

    def ifelse(e1:Exp, e2:Exp, e3:Exp):Exp = 
    {
        val thunkede1 = thunk(e1) 
        val args = List(thunkede1, e2, e3) 
        val ifelsename = Name(List(Ident("ifelse")))
        MethodInv(MethodCall(ifelsename, args))   
    }

    // this will return a delayed expression () -> e
    def thunk(e:Exp):Exp = 
    {
        Lambda(LambdaSingleParam(unitIdent), LambdaExpression_(e))
    }
        
}

