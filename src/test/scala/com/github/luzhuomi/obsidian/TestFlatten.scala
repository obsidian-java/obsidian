package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import com.github.luzhuomi.obsidian._ 
import com.github.luzhuomi.obsidian.Flatten._

import org.scalatest.{FunSuite, Matchers}

class TestFlatten1 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	System.out.println(x++ + x++);
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val F_METHODSTR ="""
public static void main (String[] args)
{
  int x_is__flattened_0;
  int x_is__flattened_1;
  int x = 0;
  x_is__flattened_0 = x++;
  x_is__flattened_1 = x++;
  System.out.println((x_is__flattened_0 + x_is__flattened_1));
  return;
}    
    """
    val f_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(F_METHODSTR)).get.get 
    test("TestFlatten1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                  case FlatError(message) => fail(message)
                  case FlatOk((st, f_methodDecl)) => {
                      val result:Decl = MemberDecl_(f_methodDecl)
                      // println(prettyPrint(result))
                      assert(result == f_methoddecl)
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestFlatten2 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	System.out.println(++x + ++x);
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val F_METHODSTR ="""
public static void main (String[] args)
{
  int x_is__flattened_0;
  int x_is__flattened_1;
  int x = 0;
  x_is__flattened_0 = ++x;
  x_is__flattened_1 = ++x;
  System.out.println((x_is__flattened_0 + x_is__flattened_1));
  return;
}    
    """
    val f_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(F_METHODSTR)).get.get 
    test("TestFlatten2") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                  case FlatError(message) => fail(message)
                  case FlatOk((st, f_methodDecl)) => {
                      val result:Decl = MemberDecl_(f_methodDecl)
                      // println(prettyPrint(result))
                      assert(result == f_methoddecl)
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}


class TestFlatten3 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main(String [] args) {
    int x = 0;
    int y = 1;
	System.out.println(x = (y = (x++) + 1));
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val F_METHODSTR ="""
public static void main (String[] args)
{
  int x_is__flattened_0;
  int y_is__flattened_1;
  int x_is__flattened_2;
  int x = 0;
  int y = 1;
  x_is__flattened_0 = x++;
  y_is__flattened_1 = y = x_is__flattened_0 + 1;
  x_is__flattened_2 = x = y_is__flattened_1;
  System.out.println(x_is__flattened_2);
  return;
} 
    """
    val f_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(F_METHODSTR)).get.get 
    test("TestFlatten3") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                  case FlatError(message) => fail(message)
                  case FlatOk((st, f_methodDecl)) => {
                      val result:Decl = MemberDecl_(f_methodDecl)
                      // println(prettyPrint(result))
                      assert(result == f_methoddecl)
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}