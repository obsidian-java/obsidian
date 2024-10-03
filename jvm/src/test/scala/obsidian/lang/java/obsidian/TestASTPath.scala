package obsidian.lang.java.obsidian

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.obsidian.*
import obsidian.lang.java.obsidian.ASTPath.*

import org.scalatest.{funsuite, matchers}

class TestASTPath1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    /**
      * public static void main() {
          System.out.println("Hello World!");
        }
      */
    
    val METHOD:MemberDecl = MethodDecl(List(Public, Static),List(),None,Ident("main"),List(),List(),None,MethodBody(Some(Block(List(BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!"))))))))))))
    val STATEMENT:BlockStmt = BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))))
    val path = List(0)
    test("TestASTPath1") {
        val result:Option[BlockStmt] = queryOps.query(METHOD,path)
        assert((!result.isEmpty) && (result.get == STATEMENT))
    }
}


class TestASTPath2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
  test("TestASTPath2") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(1)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
          case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")          
      }
  }
}


class TestASTPath3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
  test("TestASTPath3") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(2,0,0,0,0)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
          case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")
      }
  }
}


class TestASTPath4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
  test("TestASTPath4") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(2,0,0,1,0,0,3)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          }
          case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")
      }
  }
}


class TestASTPath5 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
  test("TestASTPath5") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(2,1,0)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
          case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")
      }
  }
}



class TestASTPath6 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val METHODSTRING =  """
public static void main (String[] args)
{
  int x = 0; // 0
  try        // 1
  { // 1,0
    x = x / 0; // 1,0,0 
  }  
  catch (Exception exception_desugared) 
  { // 1,1
    if (exception_desugared instanceof ArrayIndexOutOfBoundsException) // 1,1,0
    {
      ArrayIndexOutOfBoundsException e = (ArrayIndexOutOfBoundsException) exception_desugared;
      System.out.println("arrayout of bound");
    }
    else
      throw exception_desugared;
  }
  finally {  }
}
    """
  val STMTSTRNG = """
  if (exception_desugared instanceof ArrayIndexOutOfBoundsException)
    {
      ArrayIndexOutOfBoundsException e = (ArrayIndexOutOfBoundsException) exception_desugared;
      System.out.println("arrayout of bound");
    }
    else
      throw exception_desugared;
  """
  val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTRING)).get.get
  val bStmt:BlockStmt = blockStmt.apply(new Lexer.Scanner(STMTSTRNG)).get
  test("TestASTPath6") {
      methoddecl match {
          case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
              val path = List(1,1,0)
              val result = queryOps.query(methodDecl, path)
              assert((!result.isEmpty) && (result.get == bStmt ))
          } 
          case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")
      }
  }
}
