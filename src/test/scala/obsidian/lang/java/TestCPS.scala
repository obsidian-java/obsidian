package obsidian.lang.java

import org.scalatest.{FunSuite, Matchers}

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import obsidian.lang.java._ 

import obsidian.lang.java.MinSSA._
import obsidian.lang.java.CPS._



class TestCPS1 extends FunSuite with Matchers {
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
                    case Flatten.FlatError(message) => fail(message)
                    case Flatten.FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.initState) match {
                            case MinSSA.SSAError(message) => fail(message)
                            case MinSSA.SSAOk((st, ssa_methodDecl)) => { 
                                // println(ssa_methodDecl)
                                // assert(ssa == ssa_methodDecl)
                                val charcodes = getCharCodes(st)
                                CPS.cpsmethoddecl(ssa_methodDecl).run(CPS.initState(charcodes)) match {
                                    case CPS.CPSError(message) => fail(message)
                                    case CPS.CPSOk((st, cps_methodDecl)) => 
                                        println(cps_methodDecl)
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
