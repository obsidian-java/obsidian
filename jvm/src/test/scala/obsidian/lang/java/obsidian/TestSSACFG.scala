package obsidian.lang.java.obsidian

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.obsidian.CFG.{CFGResult, cfgOps, initStateInfo}
import obsidian.lang.java.obsidian.SSACFG.*
import obsidian.lang.java.obsidian.*
import obsidian.lang.java.obsidian.Desugar.*
import obsidian.lang.java.obsidian.Label.*
import obsidian.lang.java.obsidian.Flatten.*
import org.scalatest.{funsuite, matchers}

class TestSSACFGFib extends funsuite.AnyFunSuite with matchers.should.Matchers {
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


  val expectedSsaCfg = Map(
    List(3, 0, 0) ->
      AssignmentsNode(List(3, 0, 0),List(List(3, 0, 0), List(3, 0, 1), List(3, 0, 2), List(3, 0, 3)),List(Ident("t")),List(Ident("t"), Ident("f1"), Ident("f2"), Ident("i")),List(Ident("f1"), Ident("f2"), Ident("f2"), Ident("t"), Ident("i")),
        List(List(3)),List(List(3)),Map(List(3, 0, 0) -> Map(Ident("f1") -> 2, Ident("f2") -> 2), List(3, 0, 1) -> Map(Ident("f2") -> 2), List(3, 0, 2) -> Map(Ident("t") -> 2), List(3, 0, 3) -> Map(Ident("i") -> 2)),Map(List(3, 0, 0) -> Map(Ident("t") -> 2), List(3, 0, 1) -> Map(Ident("f1") -> 3), List(3, 0, 2) -> Map(Ident("f2") -> 3), List(3, 0, 3) -> Map(Ident("i") -> 3))),
    
    List(3) -> WhileNode(List(3),List(3, 0),List(),List(Ident("i"), Ident("n")),List(List(0), List(3, 0, 0)),List(List(3, 0, 0), List(4)),Map(Ident("i") -> 2, Ident("n") -> 0),Map(Ident("i") -> (2,Map(List(0) -> 1, List(3, 0, 0) -> 3)), Ident("t") -> (1,Map(List(0) -> 0, List(3, 0, 0) -> 2)), Ident("f2") -> (2,Map(List(0) -> 1, List(3, 0, 0) -> 3)), Ident("f1") -> (2,Map(List(0) -> 1, List(3, 0, 0) -> 3))),Map()),

    List(0) -> AssignmentsNode(List(0),List(List(0), List(1), List(2)),List(Ident("f1"), Ident("f2"), Ident("i")),List(Ident("f1"), Ident("f2"), Ident("i"), Ident("n")),List(),List(),List(List(3)),Map(),Map(List(1) -> Map(Ident("f2") -> 1), List(0) -> Map(Ident("f1") -> 1), List(2) -> Map(Ident("i") -> 1))),
    List(4) -> ReturnNode(List(4),List(),List(Ident("f2")),List(List(3)),Map(Ident("f2") -> 2)))

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get


  import LabelResult.*
  import FlatResult.*
  import CFGResult.*

  test("Test SSACFG on fib"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelError(message) => fail(message)
          case LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatError(message) => fail(message)
              case FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                val ssacfg = SSACFG.runSSACFG(d_methodDecl, cfg, List(0))
                ssacfg shouldBe SsacfgOk(expectedSsaCfg)
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

class TestSSACFGIfWhile extends funsuite.AnyFunSuite with matchers.should.Matchers {

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


  val expectedSsaCfg = Map()

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get



  import LabelResult.*
  import FlatResult.*
  import CFGResult.*

  test("Test ssacfg on if followed by while"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelError(message) => fail(message)
          case LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatError(message) => fail(message)
              case FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                println(cfg)
                val ssacfg = SSACFG.runSSACFG(d_methodDecl, cfg, List(0))
                println(ssacfg)
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

class TestSSACFGIfElifElse extends funsuite.AnyFunSuite with matchers.should.Matchers {

  val METHODSTR = """
                    |    public static int fun(int n)
                    |    {
                    |       int a = 0;
                    |
                    |       if (n%2==0) a = 1;
                    |       else if (n%3 == 0) a = 2;
                    |       else a = 3;
                    |       return a;
                    |    }""".stripMargin


  val expectedSsaCfg = Map()

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get


  import LabelResult.*
  import FlatResult.*
  import CFGResult.*


  test("Test ssacfg on if elif else"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelError(message) => fail(message)
          case LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatError(message) => fail(message)
              case FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                println(cfg)
                val ssacfg = SSACFG.runSSACFG(d_methodDecl, cfg, List(0))
                println(ssacfg)
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

class TestSSACFGTryCatch extends funsuite.AnyFunSuite with matchers.should.Matchers {

  val METHODSTR = """
                    |    public static int fun(int n)
                    |    {
                    |       bool error = false;
                    |       try {
                    |         if (n > 0){
                    |           throw new Exception();
                    |         }
                    |       } catch(Exception e){
                    |         error = true;
                    |       }
                    |    }""".stripMargin


  import LabelResult.*
  import FlatResult.*
  import CFGResult.*

  val expectedSsaCfg = Map()

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get



  test("Test ssacfg on try-catch"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelError(message) => fail(message)
          case LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatError(message) => fail(message)
              case FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                println(prettyPrint(MemberDecl_(d_methodDecl): Decl))
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                println(cfg)
                println(SSA.buildSSAStateInfo(d_methodDecl, cfg, List(0)))
                val ssacfg = SSACFG.runSSACFG(d_methodDecl, cfg, List(0))
                println(ssacfg)
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

class TestSSACFGCatchJoin extends funsuite.AnyFunSuite with matchers.should.Matchers {

  val METHODSTR = """
                    |    public static int fun(int n)
                    |    {
                    |       int error_type = 0;
                    |       try {
                    |         if (n < 0){
                    |           error_type = 1;
                    |           throw new Exception();
                    |         } else if (n > 100) {
                    |           error_type = 2;
                    |           throw new Exception();
                    |         }
                    |       } catch(Exception e){
                    |       }
                    |       return error_type;
                    |    }""".stripMargin


  val expectedSsaCfg = Map()

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get


  import LabelResult.*
  import FlatResult.*
  import CFGResult.*


  test("Test ssacfg on try-catch with catch join"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelError(message) => fail(message)
          case LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatError(message) => fail(message)
              case FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                println(prettyPrint(MemberDecl_(d_methodDecl): Decl))
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                println(cfg)
                println(SSA.buildSSAStateInfo(d_methodDecl, cfg, List(0)))
                val ssacfg = SSACFG.runSSACFG(d_methodDecl, cfg, List(0))
                println(ssacfg)
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


class TestSSACFGWhileBreak extends funsuite.AnyFunSuite with matchers.should.Matchers {

  val METHODSTR = """
                    |    public static int fun(int n)
                    |    {
                    |       int n = n0;
                    |       // prejoin n1 = 0 0 1, n3
                    |       while(n > 0){ // n1
                    |         // 0 0 0 n1
                    |         if (n%100 == 0) {
                    |           n = 0; // 0 0 0 0 0 n2
                    |           break; // 0 0 0 0 1
                    |         }// no join
                    |         n = n-1; // 0 0 1 n3 = n1-1
                    |       } // postjoin n4 = 0 0 0 0 1, 2, 0, 1
                    |       return n; // n4
                    |    }""".stripMargin


  val expectedSsaCfg = Map()

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get


  import LabelResult.*
  import FlatResult.*
  import CFGResult.*


  test("Test ssacfg on while-break"){
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelError(message) => fail(message)
          case LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatError(message) => fail(message)
              case FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                  case CFGError(message) => fail(message)
                  case CFGOk((st, unit)) => {
                    // println(st.cfg)
                    st.cfg
                  }
                }
                println(cfg)
                val ssacfg = SSACFG.runSSACFG(d_methodDecl, cfg, List(0))
                println(ssacfg)
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
