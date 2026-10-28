package obsidian.lang.java.obsidian

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.obsidian.MinSSA.*

/**
  * Helpers shared by the MinSSA / CPS unit tests: 
  *  - run the obfuscation pipeline (Flatten -> Desugar -> MinSSA -> CPS) on a method source, 
  *    returning an error message on the left or the result on the right. 
  *  - traverse the SSA tree (the nested blocks of SSAIf/SSAWhile/SSATry are included). 
  * */
object PipelineTestSupport {

    /** parse a method declaration (a class body member) from source. */
    def parseMethodDecl(src: String): MethodDecl = {
        val decl = classBodyStatement.apply(new Lexer.Scanner(src)).get.get
        decl match {
            case MemberDecl_(md @ MethodDecl(_, _, _, _, _, _, _, _)) => md
            case _ => sys.error("the source is not a method declaration.")
        }
    }

    /** Flatten -> Desugar -> MinSSA. @return Left(error message) or Right((ssa state, ssa method decl)) */
    def runSSA(src: String, useDebugState: Boolean = false): Either[String, (MinSSA.State, SSAMethodDecl)] = {
        val md = parseMethodDecl(src)
        Flatten.flatMethodDecl(md).run(Flatten.initStateInfo) match {
            case Flatten.FlatResult.FlatError(m) => Left("flatten error: " + m)
            case Flatten.FlatResult.FlatOk((_, fmd)) => {
                val dmd = Desugar.dsgOps.desugar(fmd)
                val init = if (useDebugState) MinSSA.debugInitState else MinSSA.initState
                MinSSA.kmethodDecl(dmd).run(init) match {
                    case MinSSA.SSAResult.SSAError(m) => Left("ssa error: " + m)
                    case MinSSA.SSAResult.SSAOk((st, ssa)) => Right((st, ssa))
                }
            }
        }
    }

    /** Flatten -> Desugar -> MinSSA -> CPS. @return Left(error message) or Right(pretty printed target code) */
    def runCPS(src: String): Either[String, String] = runSSA(src) match {
        case Left(err) => Left(err)
        case Right((st, ssa)) =>
            CPS.cpsmethoddecl(ssa).run(CPS.initState(MinSSA.getCharCodes(st))) match {
                case CPS.CPSResult.CPSError(m) => Left("cps error: " + m)
                case CPS.CPSResult.CPSOk((_, cmd)) => Right(prettyPrint(cmd))
            }
    }

    /** the nested block sequences of a control-flow SSA statement. */
    def nestedBlocks(ssa: SSAStmt): List[SSABlock] = ssa match {
        case SSAIf(_, thBlks, elBlks, _, nextBlks, _) => thBlks ++ elBlks ++ nextBlks
        case SSAWhile(_, _, bodyBlks, nextBlks, _)    => bodyBlks ++ nextBlks
        case SSATry(thBlks, _, _, catchBlks, _, joinBlks, _) => thBlks ++ catchBlks ++ joinBlks
        case _                                 => Nil
    }

    /** all blocks of an SSA method body, including the blocks nested in control-flow statements. */
    def allBlocks(body: SSAMethodBody): List[SSABlock] = {
        def go(blks: List[SSABlock]): List[SSABlock] =
            blks.flatMap(b => b :: go(b.stmts.flatMap(nestedBlocks)))
        go(body.blocks)
    }

    /** all SSA statements of an SSA method body, including those nested in control-flow statements. */
    def allStmts(body: SSAMethodBody): List[SSAStmt] = allBlocks(body).flatMap(_.stmts)

    /** the control-flow statements of a given kind in an SSA method body. */
    def findStmts[A](body: SSAMethodBody)(pf: PartialFunction[SSAStmt, A]): List[A] =
        allStmts(body).collect(pf)

    /** the lhs variable name of an SSA assignment statement, if it is a simple name assignment. */
    def lhsName(stmt: Stmt): Option[String] = stmt match {
        case ExpStmt(Assign(NameLhs(Name(List(Ident(id)))), _, _)) => Some(id)
        case _ => None
    }

    /** the identifiers read by an expression, as reported by its ExpName nodes. */
    def readNames(e: Exp): List[String] = e match {
        case ExpName(Name(List(Ident(id)))) => List(id)
        case BinOp(l, _, r)                 => readNames(l) ++ readNames(r)
        case _                              => Nil
    }

    /** an identifier name as written in a renamed variable (Name node). */
    def nameId(n: Name): Option[String] = n match {
        case Name(List(Ident(id))) => Some(id)
        case _                     => None
    }
}
