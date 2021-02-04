package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import com.github.luzhuomi.obsidian._ 
import com.github.luzhuomi.obsidian.Desugar._


import org.scalatest.{FunSuite, Matchers}


class TestDesugar1 extends FunSuite with Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int x = 0;
	if (x > 0) {x = x + 1;}
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; if (x > 0) { x = x + 1; } else   ; }
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                val desugared = dsgOps.desugar(methodDecl) 
                val result:Decl = MemberDecl_(desugared)
                // println(prettyPrint(result))
                assert(result == d_methoddecl)
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestDesugar2 extends FunSuite with Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int x = 0;
	x++;
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0;  x = x + 1;  }
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar2") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                val desugared = dsgOps.desugar(methodDecl) 
                val result:Decl = MemberDecl_(desugared)
                // println(prettyPrint(result))
                assert(result == d_methoddecl)
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}


class TestDesugar3 extends FunSuite with Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int x = 0;
	for (int i = 0; i < x; i++) { System.out.println(i); };
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; { int i = 0; while (i < x) { System.out.println(i); i = i + 1; } } ; }
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar3") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                val desugared = dsgOps.desugar(methodDecl) 
                val result:Decl = MemberDecl_(desugared)
                println(prettyPrint(result))
                assert(result == d_methoddecl)
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestDesugar4 extends FunSuite with Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int x = 0;
	for (int i = 0; i < x; i++) { System.out.println(i); };
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; { int i = 0; while (i < x) { System.out.println(i); i = i + 1; } } ; }
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar4") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                val desugared = dsgOps.desugar(methodDecl) 
                val result:Decl = MemberDecl_(desugared)
                println(prettyPrint(result))
                assert(result == d_methoddecl)
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}