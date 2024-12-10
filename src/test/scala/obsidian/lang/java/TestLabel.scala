package obsidian.lang.java

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import obsidian.lang.java._ 
import obsidian.lang.java.Label._

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
                      // println(prettyPrint(result))
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



class TestLabel4 extends FunSuite with Matchers {
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
    };
}
    """
    val L_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  obsLbl_0: switch (x)
  {
    case 1:
      x++;
      break obsLbl_0;
    case 2:
      x--;
      break obsLbl_0;
    default:
      System.out.println(x);
  }
  ;
}
    """" 
    //println(classBodyStatement.apply(new Lexer.Scanner(METHODSTR)))
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel4") {
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


class TestLabel5 extends FunSuite with Matchers {
    val METHODSTR = """
 public static void main (String[] args)
{ int x = 0; l_0: while (x > 10) { System.out.println(x); x++; } }   
    """
    val L_METHODSTR = """
 public static void main (String[] args)
{ int x = 0; l_0: while (x > 10) { System.out.println(x); x++; } }   
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel5") {
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


class TestLabel6 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  l_0: while (true)
  { System.out.println(x); if (x < 10) { x++; continue l_0; } else { break l_0; } }
}

    """
    val L_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  l_0: while (true)
  { System.out.println(x); if (x < 10) { x++; continue l_0; } else { break l_0; } }
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel6") {
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


class TestLabel7 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  l_0: do
  {
    System.out.println(x);
    if (x < 10)
    { x++; continue l_0; }
    else
    { break l_0; }
  } while (true);
}
    """
    val L_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  l_0: do
  {
    System.out.println(x);
    if (x < 10)
    { x++; continue l_0; }
    else
    { break l_0; }
  } while (true);
}
    """
    //println(classBodyStatement.apply(new Lexer.Scanner(METHODSTR)))
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel7") {
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

class TestLabel8 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  l_0: switch (x)
  {
    case 1:
      x++;
      break l_0;
    case 2:
      x--;
      break l_0;
    default:
      System.out.println(x);
  }
  ;
}
    """
    val L_METHODSTR = """
public static void main (String[] args)
{
  int x = 0;
  l_0: switch (x)
  {
    case 1:
      x++;
      break l_0;
    case 2:
      x--;
      break l_0;
    default:
      System.out.println(x);
  }
  ;
}
    """" 
    //println(classBodyStatement.apply(new Lexer.Scanner(METHODSTR)))
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel8") {
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



class TestLabel9 extends FunSuite with Matchers {
    val METHODSTR = """
public static void main (String[] args)
{
  int x = 10;
  for (int i = 0; i < x; i++) {
      System.out.println(i);
  }
}
    """
    val L_METHODSTR = """
public static void main (String[] args)
{ int x = 10; obsLbl_0: for (int i = 0 ; i < x ; i++) { System.out.println(i); } }
    """" 
    //println(classBodyStatement.apply(new Lexer.Scanner(METHODSTR)))
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get 
    val l_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(L_METHODSTR)).get.get 
    test("TestLabel9") {
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