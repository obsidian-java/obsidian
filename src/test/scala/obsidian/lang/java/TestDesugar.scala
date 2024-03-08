package obsidian.lang.java

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.*
import obsidian.lang.java.Desugar.*


import org.scalatest.{funsuite, matchers}


class TestDesugar1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int x = 0;
	if (x > 0) {x = x + 1;}
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; if (x > 0) { x = x + 1; } else {;} }
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



class TestDesugar2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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


class TestDesugar3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
                // println(prettyPrint(result))
                assert(result == d_methoddecl)
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestDesugar4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int [] a = {0, 1, 2, 3, 4};
	for (int i : a) { System.out.println(i); };
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{
  int[] a = { 0, 1, 2, 3, 4 };
  {
    int idx_loop_i = 0;
    while (idx_loop_i < a.length)
    { int i = a[idx_loop_i]; System.out.println(i); idx_loop_i = idx_loop_i + 1; }
  }
  ;
}
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar4") {
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




class TestDesugar5 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	ArrayList<Integer> a = new ArrayList<Integer>();
	for (Integer i : a) { System.out.println(i.toString()); };
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{
  ArrayList<Integer> a = new ArrayList<Integer>()
  ;
  {
    java.util.Iterator<Integer> itr_loop_l_i = a.iterator();
    while (itr_loop_l_i.hasNext())
    { Integer i = itr_loop_l_i.next(); System.out.println(i.toString()); }
  }
  ;
}

    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar5") {
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


class TestDesugar6 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void main(String [] args) {
    int x = 0;
    int y = 1;
	x = y--;
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; int y = 1; { x = y; y = y - 1; } }


    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar6") {
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




class TestDesugar7 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void main(String [] args) {
    int x = 0;
    int y = 1;
	x = ++y;
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; int y = 1; {  y = y + 1; x = y; } }


    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar7") {
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



class TestDesugar8 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void main(String [] args) {
    int x = 0;
    int y = 1;
	x = y = y + 1;
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0; int y = 1; {  y = y + 1; x = y; } }


    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar8") {
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



class TestDesugar9 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  try
  { x = x / 0; }
  catch (Exception exception_desugared)
  {
    if (exception_desugared instanceof ArrayIndexOutOfBoundsException)
    {
      ArrayIndexOutOfBoundsException e = (ArrayIndexOutOfBoundsException) exception_desugared;
      System.out.println("arrayout of bound");
    }
    else
    { 
      throw exception_desugared;
    }
  }
}
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar9") {
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



class TestDesugar10 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
 public static void main(String [] args) {
	int x = 0;
	x += 1;
}      
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main (String[] args)
{ int x = 0;  x = x + 1;  }
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar10") {
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



class TestDesugar11 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val D_METHODSTR = """
public static void main(String [] args) {
	int x = 0;
	switch (x) {
        case 1: 
            x = x + 1;
            break;
        case 2:
            x = x - 1;
            break;
        default:
            ;
    }
    ;
  return;
}
    """
    val d_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(D_METHODSTR)).get.get
    test ("TestDesugar11") {
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