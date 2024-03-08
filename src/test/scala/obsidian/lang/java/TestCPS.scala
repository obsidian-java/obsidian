package obsidian.lang.java

import org.scalatest.{funsuite, matchers}

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.* 

import obsidian.lang.java.MinSSA.*
import obsidian.lang.java.CPS.*



class TestCPS1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void main(String [] args) {
	int x; 
    int s;
    x = 0;
    s = 0;
    while (x < 10) {
        s = x + s;
    }
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

    test("TestCPS1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case Flatten.FlatResult.FlatError(message) => fail(message)
                    case Flatten.FlatResult.FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.initState) match {
                            case MinSSA.SSAResult.SSAError(message) => fail(message)
                            case MinSSA.SSAResult.SSAOk((st, ssa_methodDecl)) => { 
                                // println(ssa_methodDecl)
                                // assert(ssa == ssa_methodDecl)
                                val charcodes = getCharCodes(st)
                                CPS.cpsmethoddecl(ssa_methodDecl).run(CPS.initState(charcodes)) match {
                                    case CPSResult.CPSError(message) => fail(message)
                                    case CPSResult.CPSOk((st, cps_methodDecl)) => 
                                        println(prettyPrint(cps_methodDecl))
                                }
                            }
                        }
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestCPS2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
    public static int fib(int n)
    {
        int f1;
        int f2;
        int i;
        int t;
        f1 =1;
        f2 =1;
        i = 0;
        t = 0;
        while(i<n) {
            t = f1 + f2;
            f1 = f2;
            f2 = t;
            i = i +1;
        }
        return f2;
    }
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

    test("TestCPS2") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case Flatten.FlatResult.FlatError(message) => fail(message)
                    case Flatten.FlatResult.FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.initState) match {
                            case MinSSA.SSAResult.SSAError(message) => fail(message)
                            case MinSSA.SSAResult.SSAOk((st, ssa_methodDecl)) => { 
                                // println(ssa_methodDecl)
                                // assert(ssa == ssa_methodDecl)
                                val charcodes = getCharCodes(st)
                                CPS.cpsmethoddecl(ssa_methodDecl).run(CPS.initState(charcodes)) match {
                                    case CPS.CPSResult.CPSError(message) => fail(message)
                                    case CPS.CPSResult.CPSOk((st, cps_methodDecl)) => 
                                        println(prettyPrint(cps_methodDecl))
                                }
                            }
                        }
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}
