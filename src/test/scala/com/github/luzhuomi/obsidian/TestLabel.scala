package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import com.github.luzhuomi.obsidian._ 
import com.github.luzhuomi.obsidian.Label._

import org.scalatest.{FunSuite, Matchers}


class TestLabel1 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	while (x > 10) {
        System.out.println(x);
        x++;
    }
}
    """
    val L_METHODSTR = """
 public static void main (String[] args)
{ int x = 0; obsLbl_0: while (x > 10) { System.out.println(x); x++; } }   
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              labelOps.label(methodDecl,None,None).run(Label.initStateInfo) match {
                  case LabelError(message) => fail(message)
                  case LabelOk((st, methDecl)) => {
                      val result:Decl = MemberDecl_(methDecl)
                      // println(prettyPrint(result))
                      assert(result == l_methoddecl)
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestLabel2 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	while (true) {
        System.out.println(x);
        if (x < 10) { 
            x++;
            continue;
        } else {
            break;
        }        
    }
}
    """
    val L_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  obsLbl_0: while (true)
  { System.out.println(x); if (x < 10) { x++; continue obsLbl_0; } else { break obsLbl_0; } }
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel2") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              labelOps.label(methodDecl,None,None).run(Label.initStateInfo) match {
                  case LabelError(message) => fail(message)
                  case LabelOk((st, methDecl)) => {
                      val result:Decl = MemberDecl_(methDecl)
                      println(prettyPrint(result))
                      assert(result == l_methoddecl)
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}


class TestLabel3 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	do {
        System.out.println(x);
        if (x < 10) { 
            x++;
            continue;
        } else {
            break;
        }       
    } while (true);
}
    """
    val L_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  obsLbl_0: do
  {
    System.out.println(x);
    if (x < 10)
    { x++; continue obsLbl_0; }
    else
    { break obsLbl_0; }
  } while (true);
}
    """" 
    //println(classBodyStatement.apply(new Lexer.Scanner(METHODSTR)))
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel3") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              labelOps.label(methodDecl,None,None).run(Label.initStateInfo) match {
                  case LabelError(message) => fail(message)
                  case LabelOk((st, methDecl)) => {
                      val result:Decl = MemberDecl_(methDecl)
                      // println(prettyPrint(result))
                      assert(result == l_methoddecl)
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}