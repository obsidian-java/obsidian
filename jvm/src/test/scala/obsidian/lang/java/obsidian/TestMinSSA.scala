package obsidian.lang.java.obsidian

import org.scalatest.{funsuite, matchers}

import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.obsidian.MinSSA.*
import obsidian.lang.java.obsidian.PipelineTestSupport.*

/**
  * Unit tests for the SSA construction (MinSSA.kmethodDecl) over the currently supported 
  * statements: sequences, if-else (with/without else branch), throw, return. 
  *
  * The old TestMinSSA1-5 (while loops, written against the previous SSA AST) are kept as 
  * documentation in jvm/src/test/deprecated/TestMinSSA.scala. 
  * */

class TestMinSSASeq extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // each assignment to a variable must be renamed to a distinct variable (the SSA property), 
    // and the following uses must read the most recent definition. 
    val SRC = """
public static int seqRenames() {
    int x;
    x = 1;
    x = x + 1;
    return x;
}
"""
    test("assignments are renamed to distinct variables, uses read the latest definition") {
        runSSA(SRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                val assigns = allStmts(ssa.body).collect({ case SSAAssignments(as) => as }).flatten
                assigns.length shouldBe 2
                val lhs1 = lhsName(assigns(0)).get
                val lhs2 = lhsName(assigns(1)).get
                lhs1 should not equal lhs2
                // the rhs of the second assignment reads the first definition
                val rhs2 = assigns(1) match { case ExpStmt(Assign(_, _, e)) => readNames(e) }
                rhs2 shouldBe List(lhs1)
                // the return reads the second (latest) definition
                val rets = findStmts(ssa.body)({ case r@SSAReturn(Some(e)) => r })
                rets.length shouldBe 1
                val reads = rets.head match { case SSAReturn(Some(e)) => readNames(e) }
                reads shouldBe List(lhs2)
            }
        }
    }

    test("all local variable declarations are hoisted to the first block") {
        runSSA(SRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                val decls = allBlocks(ssa.body).filter(_.stmts.exists({ case SSAVarDecls(_, _, _) => true; case _ => false }))
                decls.length shouldBe 1
                decls.head.label shouldBe ssa.body.blocks.head.label
            }
        }
    }
}

class TestMinSSAIfJoin extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // a variable defined before the if and re-defined in both branches must get a join phi 
    // (this was a correctness bug: the statements after the if read the pre-if value). 
    val SRC = """
public static int joinPredefined() {
    int x;
    x = 1;
    if (x > 0) { x = 2; } else { x = 3; }
    return x;
}
"""
    test("a join phi is built for a variable re-defined in both branches") {
        runSSA(SRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                val ifs = findStmts(ssa.body)({ case i: SSAIf => i })
                ifs.length shouldBe 1
                val SSAIf(_, thBlks, elBlks, joinPhis, nextBlks, _) = ifs.head
                // each branch sequence ends with an SSAEmpty block labelled by the exit of the branch
                thBlks.last.stmts shouldBe List(SSAEmpty)
                elBlks.last.stmts shouldBe List(SSAEmpty)
                // one join phi for x, its rhs entries are keyed by the two branch exits
                joinPhis.length shouldBe 1
                val Phi(srcVar, renVar, rhs) = joinPhis.head
                srcVar shouldBe Name(List(Ident("x")))
                rhs.map(_._1).toSet shouldBe Set(thBlks.last.label, elBlks.last.label)
                // the rhs of the then/else entries are the names assigned in the branches
                val rhsMap = rhs.toMap
                val thenVal = nameId(rhsMap(thBlks.last.label)).get
                val elseVal = nameId(rhsMap(elBlks.last.label)).get
                lhsNamesOf(thBlks) should contain (thenVal)
                lhsNamesOf(elBlks) should contain (elseVal)
                // the next statements read the renamed join variable, not the pre-if one
                val rets = nextBlks.flatMap(_.stmts).collect({ case r@SSAReturn(Some(_)) => r })
                rets.length shouldBe 1
                val reads = rets.head match { case SSAReturn(Some(e)) => readNames(e) }
                reads shouldBe List(nameId(renVar).get)
            }
        }
    }

    def lhsNamesOf(blks: List[SSABlock]): List[String] =
        blks.flatMap(_.stmts).collect({ case SSAAssignments(as) => as }).flatten.flatMap(lhsName)
}

class TestMinSSAIfNoElse extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // an if without an else branch (desugared to an empty else): the value at the empty else 
    // exit is the value the variable carries into the branch, it must appear in the join phi. 
    val SRC = """
public static int joinIfNoElse() {
    int x;
    x = 1;
    if (x > 0) { x = 2; }
    return x;
}
"""
    test("the join phi resolves the empty else branch to the inherited value") {
        runSSA(SRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                val ifs = findStmts(ssa.body)({ case i: SSAIf => i })
                ifs.length shouldBe 1
                val SSAIf(_, thBlks, elBlks, joinPhis, _, _) = ifs.head
                thBlks.last.stmts shouldBe List(SSAEmpty)
                elBlks.exists(_.stmts.contains(SSAEmpty)) shouldBe true
                joinPhis.length shouldBe 1
                val Phi(_, _, rhs) = joinPhis.head
                rhs.length shouldBe 2
                // the pre-if definition
                val preIf = lhsNamesOf(ssa.body.blocks.takeWhile(!_.label.isInstanceOf[TThen]))
                preIf should not be empty
                val values = rhs.map(p => nameId(p._2).get).toSet
                // one entry is the value assigned in the then branch, the other is the inherited value
                values.subsetOf((lhsNamesOf(thBlks) ++ preIf).toSet) shouldBe true
                values.size shouldBe 2
            }
        }
    }

    def lhsNamesOf(blks: List[SSABlock]): List[String] =
        blks.flatMap(_.stmts).collect({ case SSAAssignments(as) => as }).flatten.flatMap(lhsName)
}

class TestMinSSAThrow extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val SRC = """
public static int thrower() {
    throw new RuntimeException("boom");
}
"""
    test("a throw statement is converted to an SSAThrow") {
        runSSA(SRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                findStmts(ssa.body)({ case t: SSAThrow => t }).length shouldBe 1
            }
        }
    }
}

class TestMinSSAReturn extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // Flatten appends `return;` to a void method, the cReturn rule must handle it. 
    val SRC = """
public static void voidish() {
    int x;
    x = 1;
}
"""
    test("the implicit return of a void method is converted (cReturn)") {
        runSSA(SRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                allBlocks(ssa.body).exists(_.stmts.exists({ case SSAReturn(None) => true; case _ => false })) shouldBe true
            }
        }
    }

    val RETSRC = """
public static int retval() {
    return 5;
}
"""
    test("a return with a value is converted to SSAReturn(Some _)") {
        runSSA(RETSRC) match {
            case Left(msg) => fail(msg)
            case Right((_, ssa)) => {
                findStmts(ssa.body)({ case r@SSAReturn(Some(_)) => r }).length shouldBe 1
            }
        }
    }
}

class TestMinSSACtxOrd extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // the partial order on target contexts (CtxOrd): the join context precedes the next context 
    // of the same control statement (required by Rleq in the statements after a control-flow node). 
    val po = MinSSA.partialOrderTCtx(List(), List())

    test("a context is comparable to itself") {
        for (c <- List(TBox, TIfJoin, TIfNext(THead(TBox)), TTryJoin, TTryNext(TBox), TThen(TBox))) {
            po.partialCompare(c, c) shouldBe 0.0
        }
    }

    test("TBox is below any other context") {
        for (c <- List(TIfJoin, TIfNext(TBox), TTryJoin, TTryNext(TBox), THead(TBox))) {
            po.partialCompare(TBox, c) shouldBe -1.0
            po.partialCompare(c, TBox) shouldBe 1.0
        }
    }

    test("THead is below TTail (CtxOrdSeq)") {
        po.partialCompare(THead(TBox), TTail(TBox)) shouldBe -1.0
        po.partialCompare(TTail(TBox), THead(TBox)) shouldBe 1.0
        po.partialCompare(TThen(THead(TBox)), TThen(TTail(TLast))) shouldBe -1.0
    }

    test("TIfJoin is below TIfNext") {
        po.partialCompare(TIfJoin, TIfNext(THead(TBox))) shouldBe -1.0
        po.partialCompare(TIfNext(THead(TBox)), TIfJoin) shouldBe 1.0
        // and the order propagates through a context hole
        po.partialCompare(TTail(TTail(TIfJoin)), TTail(TTail(TIfNext(TBox)))) shouldBe -1.0
    }

    test("TTryJoin is below TTryNext") {
        po.partialCompare(TTryJoin, TTryNext(THead(TBox))) shouldBe -1.0
        po.partialCompare(TTryNext(THead(TBox)), TTryJoin) shouldBe 1.0
        po.partialCompare(TTail(TTryJoin), TTail(TTryNext(TBox))) shouldBe -1.0
    }
}

class TestMinSSAGaps extends funsuite.AnyFunSuite with matchers.should.Matchers {
    // the try-catch SSA construction has open bugs (see README, "Known remaining work"): 
    // the catch parameter is not registered in beta by the cTry rule. 
    ignore("try-catch: the catch parameter is not registered in beta (Rleq fails on the catch body)") {
        val src = """
public static int trycatch() {
    int x;
    try { x = 1; } catch (Exception e) { x = 2; }
    return x;
}
"""
        runSSA(src) match {
            case Left(msg) => fail("expected the try-catch construction to work, got: " + msg)
            case Right(_)  => succeed
        }
    }

    // while is not implemented in kblkStmts yet (the CPS scaffolding for SSAWhile is ready). 
    ignore("while: not implemented in kblkStmts yet") {
        val src = """
public static int whileloop() {
    int x;
    x = 0;
    while (x < 10) { x = x + 1; }
    return x;
}
"""
        runSSA(src) match {
            case Left(msg) => fail("expected the while construction to work, got: " + msg)
            case Right(_)  => succeed
        }
    }
}
