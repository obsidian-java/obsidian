package obsidian.lang.java

import obsidian.lang.java.CFG._
import obsidian.lang.java.SSA._
import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class TestSSA extends FunSuite with Matchers with AppendedClues {



  val seqCFG = Map(
    List(0) -> AssignmentsNode(List(0), Nil, Nil, Nil, Nil, Nil, List(List(1))),
    List(1) -> ReturnNode(List(1), Nil, Nil, List(List(0))),
  ) // cfg with assignments followed by a return

  val ifThenElseCFG = Map(
    List(0) -> IfThenElseNode(List(0), List(0,0), List(0,1), Nil, Nil, Nil, List(List(0,0), List(0,1))),
    List(0,0) -> AssignmentsNode(List(0,0), Nil, Nil, Nil, Nil, List(List(0)), List(List(1))),
    List(0,1) -> AssignmentsNode(List(0,1), Nil, Nil, Nil, Nil, List(List(0)), List(List(1))),
    List(1) -> ReturnNode(List(1), Nil, Nil, List(List(0,0), List(0,1))),
  )

  val whileCFG = Map(
    List(0) -> WhileNode(List(0), List(0,0), Nil, Nil, List(List(0,0)), List(List(0,0), List(1))),
    List(0,0) -> AssignmentsNode(List(0,0), Nil, Nil, Nil, Nil, List(List(0)), List(List(0))),
    List(1) -> ReturnNode(List(1), Nil, Nil, List(List(0))),
  )

  val examples = Table("cfg", seqCFG, ifThenElseCFG, whileCFG)

  test("A node should dominate itself") {
    forAll(examples) {cfg =>
      val sdom = SSA.buildSDom(cfg, List(0));
      cfg.keys.foreach(node => assert(isDom(sdom, node, node)))
    }
  }

  test("A non-root node is dominated by another node iff all its predecessors are dominated") {
    forAll(examples) {cfg =>
      val sdom = SSA.buildSDom(cfg, List(0));
      val ids = cfg.keySet
      (cfg - List(0)).foreach{node =>
        val (k, v) = node
        (ids - k).foreach{dtor =>
          isSDom(sdom, dtor, k) shouldBe getPreds(v).forall(isDom(sdom, dtor, _)) withClue s"$k, $dtor"
        }
      }
    }
  }

  test("A non-root node should have an immediate dominator") {
    forAll(examples){cfg =>
      val idom = buildIDom(cfg, List(0))
      val sdom = buildSDom(cfg, List(0))
      (cfg.keySet - List(0)).foreach(node => {
        assert(idom.contains(node)) withClue s", can't find idom of $node"
        val dtor = idom(node)
        assert(isSDom(sdom, dtor, node)) withClue s", idom ($dtor) should dominate $node"
      })
    }
  }

  test("A node's dominance frontier should be all nodes N such that a predecessor is dominated, N is not strictly dominated"){
    forAll(examples){ cfg =>
      val sdom = buildSDom(cfg, List(0))
      val df = buildDFTable(cfg, List(0))
      cfg.keySet.foreach { node =>
        val dtees = sdom(node) + node
        df(node) shouldBe (dtees.flatMap(id => getSuccs(cfg(id))) -- sdom(node)) withClue s", df of $node"
      }
    }
  }

  test("Test nextVarLabel"){
    val stateInfo = SSA.StateInfo(Map(Ident("v") -> 0), Map(), Map(Ident("v") -> Nil), Map(), Map(), Map())
    val (info, i) = nextVarLabel(Ident("v")).run(stateInfo).value
    i shouldBe 0
    info shouldEqual SSA.StateInfo(Map(Ident("v") -> 1), Map(), Map(Ident("v")-> List(0)), Map(), Map(), Map())
  }

  test("Test labelLHSVar"){
    val stateInfo = SSA.StateInfo(Map(Ident("v") -> 0), Map(), Map(Ident("v") -> Nil), Map(), Map(), Map())
    val (info, _) = labelLHSVar(Ident("v"), List(0)).run(stateInfo).value
    info shouldEqual SSA.StateInfo(Map(Ident("v") -> 1), Map(), Map(Ident("v")-> List(0)), Map(List(0)-> Map(Ident("v")->0)), Map(), Map())
  }

}

class TestSSAFib extends FunSuite with Matchers with AppendedClues{
  val METHODSTR =
    """
      |    public static int fib(int n)
      |    {
      |        int f1 = 1;    //0 f1 -> 0
      |        int f2 = 1;    //1 f2 -> 0
      |        int i=0;       //2
      |        while(i < n) {   //3 phis: i, 1 -> ((0) -> 0, (3 0 1) -> 2), f1, 1 -> ((0) -> 0, (3 0 1) -> 2), f2, 1 -> ((0) -> 0, (3 0 1) -> 2)
      |            int t = f1 + f2; //3 0 0 (t 2)
      |            f1 = f2;         //3 0 1 (f1 2)
      |            f2 = t;          //3 0 2 (f2 2)
      |            i++;             //3 0 3 (i 2)
      |        }
      |        return f2;           //4 (f2 1)
      |    }""".stripMargin

  val searchFibExpectPhiRhs = Map(
    List(3) -> Map(
      Ident("f1") -> Map(List(0) -> 1, List(3, 0, 0) -> 3),
      Ident("f2") -> Map(List(0) -> 1, List(3, 0, 0) -> 3),
      Ident("t") -> Map(List(0) -> 0, List(3, 0, 0) -> 2),
      Ident("i") -> Map(List(0) -> 1, List(3, 0, 0) -> 3),
    )
  )

  val searchFibExpectPhiLhs = Map(
    List(3) -> Map(
      Ident("f1") -> 2,
      Ident("f2") -> 2,
      Ident("t") -> 1,
      Ident("i") -> 2,
    )
  )

  val searchFibExpectLhs = Map(
    Nil -> Map( Ident("t") -> 0, Ident("n") -> 0, Ident("i") -> 0, Ident("f1") -> 0, Ident("f2") -> 0),
    List(0) -> Map(Ident("f1") -> 1),
    List(1) -> Map(Ident("f2") -> 1),
    List(2) -> Map(Ident("i") -> 1),
    List(3,0,0) -> Map(Ident("t") -> 2),
    List(3,0,1) -> Map(Ident("f1") -> 3),
    List(3,0,2) -> Map(Ident("f2") -> 3),
    List(3,0,3) -> Map(Ident("i") -> 3),
  )

  val searchFibExpectRhs = Map(
    List(3) -> Map(Ident("i") -> 2, Ident("n") -> 0),
    List(3,0,0) -> Map(Ident("f1") -> 2, Ident("f2") -> 2),
    List(3,0,1) -> Map(Ident("f2") -> 2),
    List(3,0,2) -> Map(Ident("t") -> 2),
    List(3,0,3) -> Map(Ident("i") -> 2),
    List(4) -> Map(Ident("f2")-> 2),
  )

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get



  test("Test search(4) on fib"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case Label.LabelError(message) => fail(message)
          case Label.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case Flatten.FlatError(message) => fail(message)
              case Flatten.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                val info = buildSSAStateInfo(d_methodDecl, cfg, List(0))
                info.phiRhs shouldEqual searchFibExpectPhiRhs
                info.phiLhsVarReplacements shouldEqual searchFibExpectPhiLhs
                info.lhsVarReplacements shouldEqual searchFibExpectLhs
               info.rhsVarReplacements shouldEqual searchFibExpectRhs
              }
            }
          }
        }
      }
      case _ =>
        fail(
          "It is supposed to be a MethodDecl member, but some other type is encountered."
        )
    }
  }
}
/*
class TestSSAIfWhile extends FunSuite with Matchers with AppendedClues {

  val METHODSTR = """
    |    public static int fun(int n)
    |    {
    |       int a = 0; // 0
    |       int b = 0; // 1
    |       int c = 0; // 2
    |
    |       if (n > a) { // 3
    |         c = a; // 3 0 0
    |       } else { // 3 1
    |         a = n; // 3 1 0
    |       }
    |       while (b > 5){// 4
    |         b--; // 4 0 0
    |         c = b; // 4 0 1
    |       }
    |       return c;
    |    }""".stripMargin

  val searchExpectPhiRhs = Map(
    List(4) -> Map(
      Ident("a") -> Map(List(3, 1, 0) -> 2, List(3, 0, 0) -> 0, List(4, 0, 0) -> 1),
      Ident("b") -> Map(List(3, 1, 0) -> 0, List(3, 0, 0) -> 0, List(4, 0, 0) -> 2),
      Ident("c") -> Map(List(3, 1, 0) -> 0, List(3, 0, 0) -> 3, List(4, 0, 0) -> 2),
    )
  )

  val searchExpectPhiLhs = Map(
    List(4) -> Map(
      Ident("a") -> 1,
      Ident("b") -> 1,
      Ident("c") -> 1,
    )
  )

  val searchExpectLhs = Map(
    List(0) -> Map(Ident("a") -> 0, Ident("n") -> 0),
    List(1) -> Map(Ident("b") -> 0),
    List(2) -> Map(Ident("c") -> 0),
    List(3,0,0) -> Map(Ident("c") -> 3),
    List(3,1,0) -> Map(Ident("a") -> 2),
    List(4,0,0) -> Map(Ident("b") -> 2),
    List(4,0,1) -> Map(Ident("c") -> 2),
  )

  val searchExpectRhs = Map(
    List(3) -> Map(Ident("a") -> 0, Ident("n") -> 0),
    List(3,0,0) -> Map(Ident("a") -> 0),
    List(3,1,0) -> Map(Ident("n") -> 0),
    List(4) -> Map(Ident("b") -> 1),
    List(4,0,0) -> Map(Ident("b") -> 1),
    List(4,0,1) -> Map(Ident("b")-> 2),
    List(5) -> Map(Ident("c")-> 1),
  )

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get



  test("Test search(4) on if while"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case Label.LabelError(message) => fail(message)
          case Label.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case Flatten.FlatError(message) => fail(message)
              case Flatten.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                val info = buildSSAStateInfo(d_methodDecl, cfg, List(0))
                info.phiRhs shouldEqual searchExpectPhiRhs
                info.phiLhsVarReplacements shouldEqual searchExpectPhiLhs
                info.lhsVarReplacements shouldEqual searchExpectLhs
                info.rhsVarReplacements shouldEqual searchExpectRhs
              }
            }
          }
        }
      }
      case _ =>
        fail(
          "It is supposed to be a MethodDecl member, but some other type is encountered."
        )
    }
  }

}
*/