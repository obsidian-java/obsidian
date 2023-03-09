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

    }

    // Exception => Void
    // Function<Exception, Void>
    def mkArrType(t1:RefType, t2:RefType):Type = {
        RefType_(ClassRefType(ClassType(List((funcIdent, List(ActualType(t1),ActualType(t2)))))))
    }
    // val funcIdents = List(Ident("java"), Ident("util"), Ident("function"), Ident("Function"))
    val funcIdent = Ident("Function")

    val exceptIdent = Ident("Exception")
    val voidIdent = Ident("Void")
    val raiseIdent = Ident("raise")
    val kIdent = Ident("k")

    val exceptRefType = ClassRefType(ClassType(List((exceptIdent, List()))))
    val voidRefType   = ClassRefType(ClassType(List((voidIdent, List()))))
    val voidType      = RefType_(voidRefType)
    val exceptType    = RefType_(exceptRefType)
    val exceptArrVoid = mkArrType(exceptRefType,voidRefType)
    val voidArrVoid = mkArrType(voidRefType,voidRefType)
    

    def cpsk(phi:List[Phi], lbl:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = for {
        xeAsmts      <- cpsphi(phi, lbl)
        mods         <- m.pure(List())
        mkl          <- mkId("m", lbl)
        typarams     <- m.pure(List())
        formalParams <- m.pure(List(FormalParam(List(), exceptArrVoid, false, VarId(raiseIdent)), 
                                    FormalParam(List(), voidArrVoid, false,  VarId(kIdent))))
        exceptTypes  <- m.pure(List())
        exp          <- m.pure(None)
        body         <- m.pure(MethodBody(Some(Block((xeAsmts ++ List(Return(Some(MethodInv(MethodCall(Name(List(kIdent)), List())))))).map(BlockStmt_(_))))))
        decl         <- m.pure(MethodDecl(mods, typarams, Some(voidType), mkl, formalParams, exceptTypes, exp, body)) // fixme  this should be a var decl with a lambda expression
    } yield (List(decl), ExpName(Name(List(mkl))))


    def cpsphi(phi:List[Phi], ctxt:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, List[Stmt]] = m.pure(List())

    def cpsexp(e:Exp)(implicit m:MonadError[CPSState, ErrorM]):SState[State, Exp] = 
        m.pure(ExpName(Name(List(kIdent)))) // TODO:fixme

    def mkId(s:String, ctxt:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, Ident] = for {
        st <- get
        id <- st match {
            case State(chararr) => m.pure(Ident( s + "_" + charcode(ctxt, chararr).mkString))
        }
    } yield (id)

    def seq(e1:Exp, e2:Exp):Exp = ExpName(Name(List(kIdent))) // TODO:fixme
    def ifelse(e1:Exp, e2:Exp, e3:Exp):Exp = ExpName(Name(List(kIdent))) // TODO:fixme
    def thunk(e:Exp):Exp = ExpName(Name(List(kIdent))) // TODO:fixme
}

