package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.obsidian._ 
import com.github.luzhuomi.obsidian.ASTPath._

import org.scalatest.{FunSuite, Matchers}

class TestMethodASTPathQuery1 extends FunSuite with Matchers {
    /**
      * public static void main() {
          System.out.println("Hello World!");
        }
      */
    
    val METHOD:MemberDecl = MethodDecl(List(Public, Static),List(),None,Ident("main"),List(),List(),None,MethodBody(Some(Block(List(BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!"))))))))))))
    val STATEMENT:BlockStmt = BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))))
    val path = List(0)
    test("TestMethodASTPathQuery1") {
        val result:Option[BlockStmt] = queryOps.query(METHOD,path)
        assert((!result.isEmpty) && (result.get == STATEMENT))
    }
}


class TestMethodASTPathQuery2 extends FunSuite with Matchers {
  val METHODSTRING = """
int get (int x) {
  int i = lpos;  // 0
  int r = -1;    // 1
  try            // 2
  {              // 2,0 a block stmt
    if (x < i)   // 2,0,0
    {            // 2,0,0,0 a block stmt
       throw  new Exception(); // 2,0,0,0,0
    } 
    else 
    {            // 2,0,0,1 a block stmt
       while (i < x)   // 2,0,0,1,0 
       {               // 2,0,0,1,0,0
          int t = f1 + f2;   // 2,0,0,1,0,0,0
          f1 = f2;           // 2,0,0,1,0,0,1
          f2 = t;            // 2,0,0,1,0,0,2
          i = i + 1;         // 2,0,0,1,0,0,3 
       }
       lpos = i;       // 2,0,0,1,1
       r = f2;         // 2,0,0,1,2
    }
  } 
  catch (Exception e)
  {              // 2,1 a block stmt
     println("..."); // 2,1,0
  }
  return r;      // 3 
}
  """
  val STMTSTRNG = "int r = -1;"
  val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTRING)).get.get
  val bStmt:BlockStmt = blockStmt.apply(new Lexer.Scanner(STMTSTRNG)).get
  test("TestMethodASTPathQuery2") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(1)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
          
      }
  }
}


class TestMethodASTPathQuery3 extends FunSuite with Matchers {
  val METHODSTRING = """
int get (int x) {
  int i = lpos;  // 0
  int r = -1;    // 1
  try            // 2
  {              // 2,0 a block stmt
    if (x < i)   // 2,0,0
    {            // 2,0,0,0 a block stmt
       throw  new Exception(); // 2,0,0,0,0
    } 
    else 
    {            // 2,0,0,1 a block stmt
       while (i < x)   // 2,0,0,1,0 
       {               // 2,0,0,1,0,0
          int t = f1 + f2;   // 2,0,0,1,0,0,0
          f1 = f2;           // 2,0,0,1,0,0,1
          f2 = t;            // 2,0,0,1,0,0,2
          i = i + 1;         // 2,0,0,1,0,0,3 
       }
       lpos = i;       // 2,0,0,1,1
       r = f2;         // 2,0,0,1,2
    }
  } 
  catch (Exception e)
  {              // 2,1 a block stmt
     println("..."); // 2,1,0
  }
  return r;      // 3 
}
  """
  val STMTSTRNG = "throw  new Exception();"
  val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTRING)).get.get
  val bStmt:BlockStmt = blockStmt.apply(new Lexer.Scanner(STMTSTRNG)).get
  test("TestMethodASTPathQuery3") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(2,0,0,0,0)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
      }
  }
}


class TestMethodASTPathQuery4 extends FunSuite with Matchers {
  val METHODSTRING = """
int get (int x) {
  int i = lpos;  // 0
  int r = -1;    // 1
  try            // 2
  {              // 2,0 a block stmt
    if (x < i)   // 2,0,0
    {            // 2,0,0,0 a block stmt
       throw  new Exception(); // 2,0,0,0,0
    } 
    else 
    {            // 2,0,0,1 a block stmt
       while (i < x)   // 2,0,0,1,0 
       {               // 2,0,0,1,0,0
          int t = f1 + f2;   // 2,0,0,1,0,0,0
          f1 = f2;           // 2,0,0,1,0,0,1
          f2 = t;            // 2,0,0,1,0,0,2
          i = i + 1;         // 2,0,0,1,0,0,3 
       }
       lpos = i;       // 2,0,0,1,1
       r = f2;         // 2,0,0,1,2
    }
  } 
  catch (Exception e)
  {              // 2,1 a block stmt
     println("..."); // 2,1,0
  }
  return r;      // 3 
}
  """
  val STMTSTRNG = "i = i + 1;"
  val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTRING)).get.get
  val bStmt:BlockStmt = blockStmt.apply(new Lexer.Scanner(STMTSTRNG)).get
  test("TestMethodASTPathQuery4") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(2,0,0,1,0,0,3)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
      }
  }
}


class TestMethodASTPathQuery5 extends FunSuite with Matchers {
  val METHODSTRING = """
int get (int x) {
  int i = lpos;  // 0
  int r = -1;    // 1
  try            // 2
  {              // 2,0 a block stmt
    if (x < i)   // 2,0,0
    {            // 2,0,0,0 a block stmt
       throw  new Exception(); // 2,0,0,0,0
    } 
    else 
    {            // 2,0,0,1 a block stmt
       while (i < x)   // 2,0,0,1,0 
       {               // 2,0,0,1,0,0
          int t = f1 + f2;   // 2,0,0,1,0,0,0
          f1 = f2;           // 2,0,0,1,0,0,1
          f2 = t;            // 2,0,0,1,0,0,2
          i = i + 1;         // 2,0,0,1,0,0,3 
       }
       lpos = i;       // 2,0,0,1,1
       r = f2;         // 2,0,0,1,2
    }
  } 
  catch (Exception e)
  {              // 2,1 a block stmt
     println("..."); // 2,1,0
  }
  return r;      // 3 
}
  """
  val STMTSTRNG = "println(\"...\");"
  val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTRING)).get.get
  val bStmt:BlockStmt = blockStmt.apply(new Lexer.Scanner(STMTSTRNG)).get
  test("TestMethodASTPathQuery5") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(2,1,0)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
      }
  }
}