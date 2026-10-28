package obsidian.lang.java.obsidian

import org.scalatest.{funsuite, matchers}

import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.obsidian.MinSSA.*
import obsidian.lang.java.obsidian.PipelineTestSupport.*

/**
  * End-to-end tests of the obfuscation pipeline (Flatten -> Desugar -> MinSSA -> CPS). 
  *
  * Note. generated code for methods with formal parameters is not asserted to be javac-compileable: 
  * the curried CPS wrapper currently shadows the formal parameters with the lambda parameters 
  * (a known codegen issue, see README "Known remaining work"). The samples use parameterless methods. 
  * */

class TestCPSSimple extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val SRC = """
public static int simple() {
    int x;
    x = 1;
    return x;
}
"""
    test("a straight-line method is converted to CPS") {
        runCPS(SRC) match {
            case Left(msg) => fail(msg)
            case Right(code) => {
                code should include ("raise -> k ->")
                code should include ("Ctxt.seq(")
                // the identity exception continuation is passed at the method entry. 
                code should include (".apply(Ctxt.idHandler)")
            }
        }
    }
}

class TestCPSExpressionStatements extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val SRC = """
public static int printer() {
    System.out.println(1);
    return 0;
}
"""
    test("expression statements (SSAExps) are preserved in the CPS output") {
        runCPS(SRC) match {
            case Left(msg) => fail(msg)
            case Right(code) => {
                code should include ("System.out.println")
            }
        }
    }
}

class TestCPSIfElse extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val SRC = """
public static int joinPredefined() {
    int x;
    x = 1;
    if (x > 0) { x = 2; } else { x = 3; }
    return x;
}
"""
    test("if-else uses the ifelse combinator and the join phi is applied at the branch exits") {
        runCPS(SRC) match {
            case Left(msg) => fail(msg)
            case Right(code) => {
                code should include ("Ctxt.ifelse(")
                // the join phi assignment (x_join = x_branch) is emitted in the branch exit continuations. 
                val phiAssign = """ctxt\.\w+ = ctxt\.\w+;""".r
                phiAssign.findFirstIn(code).isDefined shouldBe true
            }
        }
    }
}

class TestCPSThrow extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val SRC = """
public static int thrower() {
    throw new RuntimeException("boom");
}
"""
    test("a throw raises through the exception continuation (raise.apply)") {
        runCPS(SRC) match {
            case Left(msg) => fail(msg)
            case Right(code) => {
                code should include ("raise.apply(new RuntimeException")
            }
        }
    }
}

class TestCPSTryCPS extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // the SSA stage cannot build try-catch SSA yet (see TestMinSSAGaps), so SSAMethodDecl 
    // with an SSATry statement is constructed here directly: the CPS stage must reject it 
    // with a clean error. 
    val ssaTry = SSAMethodDecl(List(Public, Static),List(),Some(PrimType_(IntT)),Ident("trycatch"),List(),List(),None,
        SSAMethodBody(List(SSABlock(TBox,List(SSATry(
            List(SSABlock(THead(TBox),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,Lit(IntLit(1))))))))),
            FormalParam(List(),RefType_(ClassRefType(ClassType(List((Ident("Exception"),List()))))),false,VarId(Ident("e"))),
            Nil,
            List(SSABlock(THead(TBox),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,Lit(IntLit(2))))))))),
            Nil,
            Nil,
            Nil))))))

    test("the CPS stage rejects SSATry with an explicit unsupported error") {
        CPS.cpsmethoddecl(ssaTry).run(CPS.initState("0123456789abcdefghiuvw".toList)) match {
            case CPS.CPSResult.CPSError(msg) => msg should include ("try-catch(-finally) is not supported")
            case CPS.CPSResult.CPSOk(_)      => fail("SSATry should not be converted by the CPS stage yet.")
        }
    }
}

class TestCPSCoreCombinators extends funsuite.AnyFunSuite with matchers.should.Matchers {
    test("every block function keeps the raise -> k -> shape required by the runtime combinators") {
        val SRC = """
public static int shapes() {
    int x;
    x = 1;
    if (x > 0) { x = 2; } else { x = 3; }
    throw new RuntimeException("boom");
}
"""
        runCPS(SRC) match {
            case Left(msg) => fail(msg)
            case Right(code) => {
                // all block functions take the exception continuation first, then the normal one. 
                code should include ("raise -> k ->")
                code should include ("Ctxt.ifelse(")
                code should include ("raise.apply(new RuntimeException")
                code should include (".apply(Ctxt.idHandler)")
            }
        }
    }
}

class TestCPSGaps extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // while is not implemented in the SSA construction yet (the CPS scaffolding is ready). 
    ignore("while: the CPS scaffolding awaits the SSA construction") {
        val SRC = """
public static int whileloop() {
    int x;
    x = 0;
    while (x < 10) { x = x + 1; }
    return x;
}
"""
        runCPS(SRC) match {
            case Left(msg) => fail("expected the while pipeline to work, got: " + msg)
            case Right(_)  => succeed
        }
    }
}
