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
                   , List(3) -> WhileNode(List(3),List(3, 0),List(Ident("i"), Ident("n")),List(),List(List(0), List(3, 0, 1)),List(List(3, 0), List(3, 0, 0), List(4)))
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
                val d_methodDecl = Desugar.dsgOps.desugar(methodDecl)
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