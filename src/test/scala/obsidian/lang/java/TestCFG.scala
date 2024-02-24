package obsidian.lang.java

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser.*
import com.github.luzhuomi.scalangj.Syntax.*
import com.github.luzhuomi.scalangj.Pretty.*
import obsidian.lang.java.*
import obsidian.lang.java.Label.*
import obsidian.lang.java.Flatten.*
import obsidian.lang.java.CFG.*
import org.scalatest.{funsuite, matchers}

class TestCFG1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val METHODSTR = """
    public static int fib(int n)
    {
        int f1 = 1;
        int f2 = 1;
        int i=0;
        while(i<n) {
            int t = f1 + f2;
            f1 = f2;
            f2 = t;
            i++;
        }
        return f2;
    }
    """
  import CFGResult.*

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
  val cfg: CFG = Map(List(3, 0, 0) -> AssignmentsNode(List(3, 0, 0),List(List(3, 0, 0), List(3, 0, 1), List(3, 0, 2), List(3, 0, 3)),List(Ident("t")),List(Ident("t"), Ident("f1"), Ident("f2"), Ident("i")),List(Ident("f1"), Ident("f2"), Ident("f2"), Ident("t"), Ident("i")),List(List(3)),List(List(3)))
                   , List(3) -> WhileNode(List(3),List(3, 0),List(), List(Ident("i"), Ident("n")),List(List(0), List(3, 0, 0)),List(List(3, 0, 0), List(4)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0), List(1), List(2)),List(Ident("f1"), Ident("f2"), Ident("i")),List(Ident("f1"), Ident("f2"), Ident("i"), Ident("n")),List(),List(),List(List(3)))
                   , List(4) -> ReturnNode(List(4),List(),List(Ident("f2")),List(List(3))))
  test("TestCFG1") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        // println(st.cfg)
                        assert(st.cfg == cfg)
                    }
                }
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


class TestCFG2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	switch (x) {
        case 1: 
            x++;
            break;
        case 2:
            x--;
            break;
        default:
            System.out.println(x);
    }
  return;
}
    """
  import CFGResult.*

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1) -> SwitchNode(List(1),List(List(1, 0), List(1, 1), List(1, 2)),List(),List(Ident("x")),List(List(0)),List(List(1, 0), List(1, 1), List(1, 2)))
                   , List(1, 1) -> CaseNode(List(1, 1),List(List(1, 1, 0), List(1, 1, 1)),List(List(1)),List(List(1, 1, 0)))
                   , List(1, 2) -> DefaultNode(List(1, 2),List(List(1, 2, 0)),List(List(1)),List(List(1, 2, 0)))
                   , List(1, 0, 1) -> BreakNode(List(1, 0, 1),List(List(1, 0, 0)),List(List(2)))
                   , List(1, 2, 0) -> AssignmentsNode(List(1, 2, 0),List(List(1, 2, 0)),List(),List(),List(Ident("x")),List(List(1, 2)),List(List(2)))
                   , List(2) -> ReturnNode(List(2),List(),List(),List(List(1, 2, 0), List(1, 0, 1), List(1, 1, 1)))
                   , List(1, 0) -> CaseNode(List(1, 0),List(List(1, 0, 0), List(1, 0, 1)),List(List(1)),List(List(1, 0, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 0)),List(List(1, 0, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("x"), Ident("args")),List(),List(),List(List(1)))
                   , List(1, 1, 1) -> BreakNode(List(1, 1, 1),List(List(1, 1, 0)),List(List(2)))
                   , List(1, 1, 0) -> AssignmentsNode(List(1, 1, 0),List(List(1, 1, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 1)),List(List(1, 1, 1))))
  test("TestCFG2") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        assert(st.cfg == cfg)
                    }
                }
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


class TestCFG3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	switch (x) {
        case 1: 
            x++;
            break;
        case 2:
            x--;
            break;
        default:
            System.out.println(x);
    }
    ;
  return;
}
    """
  import CFGResult.*

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1) -> SwitchNode(List(1),List(List(1, 0), List(1, 1), List(1, 2)),List(),List(Ident("x")),List(List(0)),List(List(1, 0), List(1, 1), List(1, 2)))
                   , List(1, 1) -> CaseNode(List(1, 1),List(List(1, 1, 0), List(1, 1, 1)),List(List(1)),List(List(1, 1, 0)))
                   , List(1, 2) -> DefaultNode(List(1, 2),List(List(1, 2, 0)),List(List(1)),List(List(1, 2, 0)))
                   , List(1, 0, 1) -> BreakNode(List(1, 0, 1),List(List(1, 0, 0)),List(List(2)))
                   , List(1, 2, 0) -> AssignmentsNode(List(1, 2, 0),List(List(1, 2, 0)),List(),List(),List(Ident("x")),List(List(1, 2)),List(List(2)))
                   , List(3) -> ReturnNode(List(3),List(),List(),List(List(2)))
                   , List(1, 0) -> CaseNode(List(1, 0),List(List(1, 0, 0), List(1, 0, 1)),List(List(1)),List(List(1, 0, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 0)),List(List(1, 0, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("x"), Ident("args")),List(),List(),List(List(1)))
                   , List(1, 1, 1) -> BreakNode(List(1, 1, 1),List(List(1, 1, 0)),List(List(2)))
                   , List(2) -> AssignmentsNode(List(2),List(List(2)),List(),List(),List(),List(List(1, 2, 0), List(1, 0, 1), List(1, 1, 1)),List(List(3)))
                   , List(1, 1, 0) -> AssignmentsNode(List(1, 1, 0),List(List(1, 1, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 1)),List(List(1, 1, 1))))
  test("TestCFG3") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        assert(st.cfg == cfg)
                    }
                }
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



class TestCFG4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val METHODSTR = """
public static void main (String[] args)
{
  int x = 10;
  for (int i = 0; i < x; i++) {
      System.out.println(i);
  }
  return;
}
    """
  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1, 1) -> WhileNode(List(1, 1),List(1, 1, 0),List(), List(Ident("i"), Ident("x")),List(List(1, 0), List(1, 1, 0, 0)),List(List(1, 1, 0, 0), List(1, 2)))
                   , List(1, 2) -> AssignmentsNode(List(1, 2),List(List(1, 2)),List(),List(),List(),List(List(1, 1)),List(List(2)))
                   , List(1, 1, 0, 0) -> AssignmentsNode(List(1, 1, 0, 0),List(List(1, 1, 0, 0), List(1, 1, 0, 1)),List(),List(Ident("i")),List(Ident("i"), Ident("i")),List(List(1, 1)),List(List(1, 1)))
                   , List(1, 0) -> AssignmentsNode(List(1, 0),List(List(1, 0)),List(Ident("i")),List(Ident("i")),List(),List(List(0)),List(List(1, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("x"), Ident("args")),List(),List(),List(List(1, 0)))
                   , List(2) -> ReturnNode(List(2),List(),List(),List(List(1, 2)))) 
  import CFGResult.*

  test("TestCFG4") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        assert(st.cfg == cfg)
                    }
                }
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



class TestCFG5 extends funsuite.AnyFunSuite with matchers.should.Matchers { // probably a bad test case, we can't handle unchecked exception
  val METHODSTR = """
public static void main(String [] args) {
    int x = 0;
    try { 
        x = x / 0 ;
    } 
    catch (ArrayIndexOutOfBoundsException e) {
        System.out.println("arrayout of bound");
    }
} 
  """
  import CFGResult.*

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("x"), Ident("args")),List(),List(),List(List(1)))
                   , List(1) -> TryCatchFinallyNode(List(1),List(1, 0),List(1, 1),List(Ident("exception_desugared")),List(1, 2),List(List(0)),List(List(1, 0, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1)),List(List(1, 2)))
                   , List(1, 1, 0) -> IfThenElseNode(List(1, 1, 0),List(1, 1, 0, 0),List(1, 1, 0, 1),List(),List(Ident("exception_desugared")),List(),List(List(1, 1, 0, 0, 0), List(1, 1, 0, 1, 0)))
                   , List(1, 1, 0, 0, 0) -> AssignmentsNode(List(1, 1, 0, 0, 0),List(List(1, 1, 0, 0, 0), List(1, 1, 0, 0, 1)),List(Ident("e")),List(Ident("e")),List(Ident("exception_desugared")),List(List(1, 1, 0)),List(List(1, 2)))
                   , List(1, 1, 0, 1, 0) -> ThrowNode(List(1, 1, 0, 1, 0),List(),List(Ident("exception_desugared")),List(List(1, 1, 0)),List())
                   , List(1, 2) -> AssignmentsNode(List(1, 2),List(),List(),List(),List(),List(List(1, 0, 0), List(1, 1, 0, 0, 0)),List(List(2)))
                   , List(2) -> ReturnNode(List(2),List(),List(),List(List(1, 2)))
                   )

  test("TestCFG5") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        assert(st.cfg == cfg)
                    }
                }
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


class TestCFG6 extends funsuite.AnyFunSuite with matchers.should.Matchers { // probably a bad test case, we can't handle unchecked exception
  val METHODSTR = """
 public static int abs(int n)
  {
      int a = -n;
      if (a < n){
        a = n;
      } else {}
      return a;
  } 
  """
  import CFGResult.*

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1) -> IfThenElseNode(List(1),List(1, 0),List(1, 1),List(),List(Ident("a"), Ident("n")),List(List(0)),List(List(1, 0, 0), List(1, 1, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("a")),List(Ident("n")),List(List(1)),List(List(2)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("a")),List(Ident("a"), Ident("n")),List(Ident("n")),List(),List(List(1)))
                   , List(2) -> ReturnNode(List(2),List(),List(Ident("a")),List(List(1, 0, 0), List(1, 1, 0)))
                   , List(1, 1, 0) -> AssignmentsNode(List(1, 1, 0),List(List(1, 1, 0)),List(),List(),List(),List(List(1)),List(List(2))))

  test("TestCFG6") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        // println(st.cfg)
                        assert(st.cfg == cfg)
                    }
                }
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



class TestCFG7 extends funsuite.AnyFunSuite with matchers.should.Matchers { // probably a bad test case, we can't handle unchecked exception
  val METHODSTR = """
public static int fun(int n)
{
   bool error = false;
   try {
     if (n > 0){
       throw new Exception();
     }
   } catch(Exception e){
     error = true;
   }
}
  """
  import CFGResult.*

  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map( List(1) -> TryCatchFinallyNode(List(1),List(1, 0),List(1, 1),List(Ident("exception_desugared")),List(1, 2),List(List(0)),List(List(1, 0, 0)))
   , List(1, 2) -> AssignmentsNode(List(1, 2),List(),List(),List(),List(),List(List(1, 0, 0, 1, 0), List(1, 1, 0, 0, 0)),List())
   , List(1, 0, 0, 0, 0) -> ThrowNode(List(1, 0, 0, 0, 0),List(),List(),List(List(1, 0, 0)),List(List(1, 1, 0)))
   , List(1, 1, 0, 1, 0) -> ThrowNode(List(1, 1, 0, 1, 0),List(),List(Ident("exception_desugared")),List(List(1, 1, 0)),List())
   , List(1, 1, 0, 0, 0) -> AssignmentsNode(List(1, 1, 0, 0, 0),List(List(1, 1, 0, 0, 0), List(1, 1, 0, 0, 1)),List(Ident("e")),List(Ident("e"), Ident("error")),List(Ident("exception_desugared")),List(List(1, 1, 0)),List(List(1, 2)))
   , List(1, 0, 0) -> IfThenElseNode(List(1, 0, 0),List(1, 0, 0, 0),List(1, 0, 0, 1),List(),List(Ident("n")),List(List(1)),List(List(1, 0, 0, 0, 0), List(1, 0, 0, 1, 0)))
   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("error")),List(Ident("error"), Ident("n")),List(),List(),List(List(1)))
   , List(1, 1, 0) -> IfThenElseNode(List(1, 1, 0),List(1, 1, 0, 0),List(1, 1, 0, 1),List(),List(Ident("exception_desugared")),List(List(1, 0, 0, 0, 0)),List(List(1, 1, 0, 0, 0), List(1, 1, 0, 1, 0)))
   , List(1, 0, 0, 1, 0) -> AssignmentsNode(List(1, 0, 0, 1, 0),List(List(1, 0, 0, 1, 0)),List(),List(),List(),List(List(1, 0, 0)),List(List(1, 2))))

  test("TestCFG7") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        // println(st.cfg)
                        assert(st.cfg == cfg)
                    }
                }
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



class TestCFG8 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	switch (x) {
        case 1: 
            x++;
            break;
        case 2:
            x--;
            break;
    }
    ;
  return;
}
    """
  import CFGResult.*
  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1) -> SwitchNode(List(1),List(List(1, 0), List(1, 1), List(1, 2)),List(),List(Ident("x")),List(List(0)),List(List(1, 0), List(1, 1), List(1, 2)))
                   , List(1, 1) -> CaseNode(List(1, 1),List(List(1, 1, 0), List(1, 1, 1)),List(List(1)),List(List(1, 1, 0)))
                   , List(1, 2) -> DefaultNode(List(1, 2),List(List(1, 2, 0)),List(List(1)),List(List(1, 2, 0)))
                   , List(1, 0, 1) -> BreakNode(List(1, 0, 1),List(List(1, 0, 0)),List(List(2)))
                   , List(1, 2, 0) -> AssignmentsNode(List(1, 2, 0),List(List(1,2,0)),List(),List(),List(),List(List(1, 2)),List(List(2)))
                   , List(3) -> ReturnNode(List(3),List(),List(),List(List(2)))
                   , List(1, 0) -> CaseNode(List(1, 0),List(List(1, 0, 0), List(1, 0, 1)),List(List(1)),List(List(1, 0, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 0)),List(List(1, 0, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("x"), Ident("args")),List(),List(),List(List(1)))
                   , List(1, 1, 1) -> BreakNode(List(1, 1, 1),List(List(1, 1, 0)),List(List(2)))
                   , List(2) -> AssignmentsNode(List(2),List(List(2)),List(),List(),List(),List(List(1, 2, 0), List(1, 0, 1), List(1, 1, 1)),List(List(3)))
                   , List(1, 1, 0) -> AssignmentsNode(List(1, 1, 0),List(List(1, 1, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 1)),List(List(1, 1, 1))))
  test("TestCFG8") {
    methoddecl match {
      case MemberDecl_(methodDecl @ MethodDecl(_, _, _, _, _, _, _, _)) => {
        Label.labelOps
          .label(methodDecl, None, None)
          .run(Label.initStateInfo) match {
          case LabelResult.LabelError(message) => fail(message)
          case LabelResult.LabelOk((st, methDecl)) => {
            Flatten.flatMethodDecl(methDecl).run(Flatten.initStateInfo) match {
              case FlatResult.FlatError(message) => fail(message)
              case FlatResult.FlatOk((st, f_methdDecl)) => {
                val d_methodDecl = Desugar.dsgOps.desugar(f_methdDecl)
                cfgOps.buildCFG(d_methodDecl, List()).run(CFG.initStateInfo) match {
                    case CFGError(message) => fail(message)
                    case CFGOk((st, unit)) => {
                        assert(st.cfg == cfg)
                    }
                }
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



