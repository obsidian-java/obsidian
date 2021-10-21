package obsidian.lang.java

import org.scalatest.{FunSuite, Matchers}

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import obsidian.lang.java._ 
import obsidian.lang.java.Label._
import obsidian.lang.java.SSADL._

class TestSSADL1 extends FunSuite with Matchers {
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
    val F_METHODSTR ="""

    """
    // val f_methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(F_METHODSTR)).get.get 
    test("TestFlatten1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case Flatten.FlatError(message) => fail(message)
                    case Flatten.FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        SSADL.kmethodDecl(d_methodDecl).run(SSADL.initState) match {
                            case SSADL.SSAError(message) => fail(message)
                            case SSADL.SSAOk((st, ssa_methodDecl)) => println(ssa_methodDecl)
                        }
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}
