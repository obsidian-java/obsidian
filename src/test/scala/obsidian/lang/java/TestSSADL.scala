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
    val ssa = SSAMethodDecl(
                List(Public, Static),List(),None,Ident("main"),
                List(FormalParam(List(),RefType_(ArrayType(RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))))),false,VarId(Ident("args")))),List(),None,
                SSAMethodBody(
                    List(
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_2")),None)))),
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_4_")),None)))),
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_4___")),None)))),
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_3")),None)))),
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_4_")),None)))),
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_4___")),None)))),
                    SSABlock(SSADL.Label(List(0),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_0")),None)))),
                    SSABlock(SSADL.Label(List(1),None),SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_1")),None)))),
                    SSABlock(SSADL.Label(List(2),None),SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_2")))),EqualA,Lit(IntLit(0))))))),
                    SSABlock(SSADL.Label(List(3),None),SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_3")))),EqualA,Lit(IntLit(0))))))),
                    SSABlock(SSADL.Label(List(4),None),SSAWhile(List(Phi(Name(List(Ident("x"))),Name(List(Ident("x_4_"))),
                                                                        Map(SSADL.Label(List(3),None) -> Name(List(Ident("x_2"))),
                                                                            SSADL.Label(List(4, 0, 0),None) -> Name(List(Ident("x_4_"))))),
                                                                    Phi(Name(List(Ident("s"))),Name(List(Ident("s_4_"))),
                                                                        Map(SSADL.Label(List(3),None) -> Name(List(Ident("s_3"))),
                                                                            SSADL.Label(List(4, 0, 0),None) -> Name(List(Ident("s_4_0_0")))))),
                                                                BinOp(ExpName(Name(List(Ident("x_2")))),LThan,Lit(IntLit(10))),
                                                                List(
                                                                    SSABlock(SSADL.Label(List(4, 0, 0),None),SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_4_0_0")))),EqualA,BinOp(ExpName(Name(List(Ident("x_4_")))),Add,ExpName(Name(List(Ident("s_4_"))))))))))),
                                                                    List(Phi(Name(List(Ident("x"))),Name(List(Ident("x_4___"))),Map(SSADL.Label(List(4),Some(Pre)) -> Name(List(Ident("x_4_"))))), 
                                                                        Phi(Name(List(Ident("s"))),Name(List(Ident("s_4___"))), Map(SSADL.Label(List(4),Some(Pre)) -> Name(List(Ident("s_4_")))))))), SSABlock(SSADL.Label(List(5),None),SSAReturn(None)))))
    test("TestSSADL1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case Flatten.FlatError(message) => fail(message)
                    case Flatten.FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        SSADL.kmethodDecl(d_methodDecl).run(SSADL.initState) match {
                            case SSADL.SSAError(message) => fail(message)
                            case SSADL.SSAOk((st, ssa_methodDecl)) => { 
                                // println(ssa_methodDecl)
                                assert(ssa == ssa_methodDecl)
                            }
                        }
                  }
              }
            }
            case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
        }
    }
}



class TestSSADL2 extends FunSuite with Matchers {
    val METHODSTR = """
public static boolean add(int v) {
    int [] new_vals=null; 
    int i=0; 
    boolean res=false; 
    try {
      if (this.cap < 1){throw new Exception();}
      else {
        if (this.size < this.cap) {
          this.vals[this.size] = v; this.size = this.size + 1;
        } else {
          new_vals = new int[this.cap];
          i = 0;
          while (i < this.cap-1) {
            new_vals[i] = this.vals[i+1];
            i = i + 1;
          }
          new_vals[this.cap-1] = v;
          this.vals = new_vals;
        }
      }
      res = true;
    } catch (Exception e) {
      println("Failed with zero capacity.");
    }
    return res;
}
    """
    // val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get

    test("TestSSADL2") {
        classBodyStatement.apply(new Lexer.Scanner(METHODSTR)) match {
            case Error(msg, next) => fail(msg)
            case Failure(msg, next) => fail(msg)
            case Success(result, next) => result match {
                case None => fail("parsing successful but no result.")
                case Some(methoddecl) => methoddecl match {
                    case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                        Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                            case Flatten.FlatError(message) => fail(message)
                            case Flatten.FlatOk((st, f_methodDecl)) => {
                                val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                                SSADL.kmethodDecl(d_methodDecl).run(SSADL.initState) match {
                                    case SSADL.SSAError(message) => fail(message)
                                    case SSADL.SSAOk((st, ssa_methodDecl)) => { 
                                        println(ssa_methodDecl)
                                        // assert(ssa == ssa_methodDecl)
                                    }
                                }
                        }
                    }
                    }
                    case _ => fail("It is supposed to be a MethodDecl member, but some other type is encountered.")           
                }
            }
            case _: NoSuccess => fail("parsing failed")
        }
    }
}