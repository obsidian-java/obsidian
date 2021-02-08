package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import com.github.luzhuomi.obsidian._
import com.github.luzhuomi.obsidian.CFG._

import org.scalatest.{FunSuite, Matchers}

class TestCFG1 extends FunSuite with Matchers {
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
  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(3, 0, 0) -> AssignmentsNode(List(3, 0, 0),List(List(3, 0, 0)),List(Ident("t")),List(Ident("f1"), Ident("f2")),List(Ident("t")),List(List(3)),List(List(3, 0, 1)))
                   , List(3) -> WhileNode(List(3),List(3, 0, 0),List(Ident("i"), Ident("n")),List(),List(List(0), List(3, 0, 1)),List(List(3, 0, 0), List(4)))
                   , List(3, 0, 1) -> AssignmentsNode(List(3, 0, 1),List(List(3, 0, 1), List(3, 0, 2), List(3, 0, 3)),List(),List(Ident("f1"), Ident("f2"), Ident("i")),List(Ident("f2"), Ident("t"), Ident("i")),List(List(3, 0, 0)),List(List(3)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0), List(1), List(2)),List(Ident("f1"), Ident("f2"), Ident("i")),List(Ident("f2"), Ident("i"), Ident("n")),List(Ident("f1")),List(),List(List(3)))
                   , List(4) -> ReturnNode(List(4),List(),List(Ident("f2")),List(List(3))))
  test("TestCFG1") {
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
                cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
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


class TestCFG2 extends FunSuite with Matchers {
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
  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1) -> SwitchNode(List(1),List(List(1, 0), List(1, 1), List(1, 2), List(1, 3)),List(),List(Ident("x")),List(List(0)),List(List(1, 0), List(1, 1), List(1, 2), List(1, 3)))
                   , List(1, 1) -> CaseNode(List(1, 1),List(List(1, 1, 0), List(1, 1, 1), List(1, 1, 2)),List(List(1)),List(List(1, 1, 0)))
                   , List(1, 2) -> DefaultNode(List(1, 2),List(List(1, 2, 0), List(1, 2, 1)),List(List(1)),List(List(1, 2, 0)))
                   , List(1, 0, 1) -> BreakNode(List(1, 0, 1),List(List(1, 0, 0)),List(List(2)))
                   , List(1, 2, 0) -> AssignmentsNode(List(1, 2, 0),List(List(1, 2, 0)),List(),List(),List(Ident("x")),List(List(1, 2)),List(List(2)))
                   , List(2) -> ReturnNode(List(2),List(),List(),List(List(1, 2, 0), List(1, 0, 1), List(1, 1, 1)))
                   , List(1, 0) -> CaseNode(List(1, 0),List(List(1, 0, 0), List(1, 0, 1), List(1, 0, 2)),List(List(1)),List(List(1, 0, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 0)),List(List(1, 0, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("args")),List(Ident("x")),List(),List(List(1)))
                   , List(1, 1, 1) -> BreakNode(List(1, 1, 1),List(List(1, 1, 0)),List(List(2)))
                   , List(1, 1, 0) -> AssignmentsNode(List(1, 1, 0),List(List(1, 1, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 1)),List(List(1, 1, 1))))
  test("TestCFG2") {
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
                cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
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


class TestCFG3 extends FunSuite with Matchers {
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
  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1) -> SwitchNode(List(1),List(List(1, 0), List(1, 1), List(1, 2), List(1, 3)),List(),List(Ident("x")),List(List(0)),List(List(1, 0), List(1, 1), List(1, 2), List(1, 3)))
                   , List(1, 1) -> CaseNode(List(1, 1),List(List(1, 1, 0), List(1, 1, 1), List(1, 1, 2)),List(List(1)),List(List(1, 1, 0)))
                   , List(1, 2) -> DefaultNode(List(1, 2),List(List(1, 2, 0), List(1, 2, 1)),List(List(1)),List(List(1, 2, 0)))
                   , List(1, 0, 1) -> BreakNode(List(1, 0, 1),List(List(1, 0, 0)),List())
                   , List(1, 2, 0) -> AssignmentsNode(List(1, 2, 0),List(List(1, 2, 0)),List(),List(),List(Ident("x")),List(List(1, 2)),List())
                   , List(3) -> ReturnNode(List(3),List(),List(),List(List(2)))
                   , List(1, 0) -> CaseNode(List(1, 0),List(List(1, 0, 0), List(1, 0, 1), List(1, 0, 2)),List(List(1)),List(List(1, 0, 0)))
                   , List(1, 0, 0) -> AssignmentsNode(List(1, 0, 0),List(List(1, 0, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 0)),List(List(1, 0, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("args")),List(Ident("x")),List(),List(List(1)))
                   , List(1, 1, 1) -> BreakNode(List(1, 1, 1),List(List(1, 1, 0)),List())
                   , List(2) -> AssignmentsNode(List(2),List(List(2)),List(),List(),List(),List(List(1, 2, 0), List(1, 0, 1), List(1, 1, 1)),List(List(3)))
                   , List(1, 1, 0) -> AssignmentsNode(List(1, 1, 0),List(List(1, 1, 0)),List(),List(Ident("x")),List(Ident("x")),List(List(1, 1)),List(List(1, 1, 1))))
  test("TestCFG3") {
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
                cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
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



class TestCFG4 extends FunSuite with Matchers {
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

  val cfg: CFG = Map(List(1, 1) -> WhileNode(List(1, 1),List(1, 1, 0, 0),List(Ident("i"), Ident("x")),List(),List(List(1, 0), List(1, 1, 0, 0)),List(List(1, 1, 0, 0)))
                   , List(1, 2) -> AssignmentsNode(List(1, 2),List(List(1, 2)),List(),List(),List(),List(List(1, 1)),List(List(2)))
                   , List(1, 1, 0, 0) -> AssignmentsNode(List(1, 1, 0, 0),List(List(1, 1, 0, 0), List(1, 1, 0, 1)),List(),List(Ident("i")),List(Ident("i"), Ident("i")),List(List(1, 1)),List(List(1, 1)))
                   , List(1, 0) -> AssignmentsNode(List(1, 0),List(List(1, 0)),List(Ident("i")),List(),List(Ident("i")),List(List(0)),List(List(1, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("args")),List(Ident("x")),List(),List(List(1, 0)))
                   , List(2) -> ReturnNode(List(2),List(),List(),List(List(1, 2)))) 
  test("TestCFG4") {
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
                cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
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

/*
class TestCFG5 extends FunSuite with Matchers {
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
  val methoddecl: Decl =
    classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

  val cfg: CFG = Map(List(1, 1) -> WhileNode(List(1, 1),List(1, 1, 0, 0),List(Ident("i"), Ident("x")),List(),List(List(1, 0), List(1, 1, 0, 0)),List(List(1, 1, 0, 0)))
                   , List(1, 2) -> AssignmentsNode(List(1, 2),List(List(1, 2)),List(),List(),List(),List(List(1, 1)),List(List(2)))
                   , List(1, 1, 0, 0) -> AssignmentsNode(List(1, 1, 0, 0),List(List(1, 1, 0, 0), List(1, 1, 0, 1)),List(),List(Ident("i")),List(Ident("i"), Ident("i")),List(List(1, 1)),List(List(1, 1)))
                   , List(1, 0) -> AssignmentsNode(List(1, 0),List(List(1, 0)),List(Ident("i")),List(),List(Ident("i")),List(List(0)),List(List(1, 1)))
                   , List(0) -> AssignmentsNode(List(0),List(List(0)),List(Ident("x")),List(Ident("args")),List(Ident("x")),List(),List(List(1, 0)))
                   , List(2) -> ReturnNode(List(2),List(),List(),List(List(1, 2)))) 
  test("TestCFG5") {
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
                cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
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
*/
