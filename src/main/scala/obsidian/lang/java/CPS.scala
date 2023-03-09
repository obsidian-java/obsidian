package obsidian.lang.java


import cats._
import cats.implicits._
import cats.data.StateT


// constructing CPS from SSA
import com.github.luzhuomi.scalangj.Syntax._
import obsidian.lang.java.MinSSA._




object CPS {

    /**
      * the monad state for CPS conversion
      *
      * @param chararray - the character array for generating id from context, passed from the SSA generation step
      */
    case class State(
        chararray: List[Char]
    )


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
            } yield ((declp ++ declpp ++ declppp, seq(ifelse( lambda(List(), exp), expp, exppp), expppp)))
        }

    }

    // Exception => Void
    // Function<Exception, Void>
    def mkArrType(t1:Type, t2:Type):Type = {
        RefType_(ClassRefType(ClassType(funcIdent, List(t1,t2))))
    }
    val funcIdent = List(Ident("java"), Ident("util"), Ident("function"), Ident("Function"))

    val exceptIdent = List(Ident("Exception"))
    val voidIdent = List(Ident("Void"))

    val exceptType = RefType_(ClassRefType(ClassType(List(exceptIdent), List())))
    val voidType   = RefType_(ClassRefType(ClassType(List(voidIdent), List())))
    val exceptArrVoid = mkArryType(exceptType,voidType)
    val voidArrVoid = mkArryType(voidType,voidType)
    

    def cpsk(phi:List[Phi], lbl:Label)(implicit m:MonadError[CPSState, ErrorM]):SState[State, (List[VarDecl], Exp)] = for {
        xeAsmts      <- cpsphi(phi, lbl)
        mods         <- m.pure(List())
        mklid        <- mkId("m", lbl)
        typarmas     <- m.pure(List())
        formalParams <- 
        decl     <- m.pure(MethodDecl(mods, typarams, Some(voidType), formalParams)) // todo
    }
}