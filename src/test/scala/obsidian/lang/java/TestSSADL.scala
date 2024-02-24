package obsidian.lang.java

import org.scalatest.{funsuite, matchers}

import com.github.luzhuomi.scalangj.Lexer
import com.github.luzhuomi.scalangj.Parser.*
import com.github.luzhuomi.scalangj.Syntax.*
import com.github.luzhuomi.scalangj.Pretty.*
import obsidian.lang.java.*
import obsidian.lang.java.Label.*
/*
import obsidian.lang.java.SSADL.*

class TestSSADL1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
                List(Public, Static),List(),None,Ident("main"), //TODO maybe we should store formal parameter somewhere separately from the VarMap
                List(FormalParam(List(),RefType_(ArrayType(RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))))),false,VarId(Ident("args")))),List(),None,
                SSAMethodBody(
                    List(
                    SSABlock(SSADL.Label(List(0),None),List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_2")),None))),
                                                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_4_")),None))),
                                                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_4___")),None))),
                                                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_3")),None))),
                                                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_4_")),None))),
                                                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_4___")),None))),
                                                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_0")),None))))),
                    SSABlock(SSADL.Label(List(1),None),List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_1")),None))))),
                    SSABlock(SSADL.Label(List(2),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_2")))),EqualA,Lit(IntLit(0)))))))),
                    SSABlock(SSADL.Label(List(3),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_3")))),EqualA,Lit(IntLit(0)))))))),
                    SSABlock(SSADL.Label(List(4),None),List(SSAWhile(List(
                                                                    Phi(Name(List(Ident("args"))),Name(List(Ident("args_4_"))),Map(SSADL.Label(List(3),None) -> Name(List(Ident("args"))), SSADL.Label(List(4, 0, 0),None) -> Name(List(Ident("args_4_"))))),
                                                                    Phi(Name(List(Ident("x"))),Name(List(Ident("x_4_"))),
                                                                        Map(SSADL.Label(List(3),None) -> Name(List(Ident("x_2"))),
                                                                            SSADL.Label(List(4, 0, 0),None) -> Name(List(Ident("x_4_"))))),
                                                                    Phi(Name(List(Ident("s"))),Name(List(Ident("s_4_"))),
                                                                        Map(SSADL.Label(List(3),None) -> Name(List(Ident("s_3"))),
                                                                            SSADL.Label(List(4, 0, 0),None) -> Name(List(Ident("s_4_0_0")))))),
                                                                BinOp(ExpName(Name(List(Ident("x_2")))),LThan,Lit(IntLit(10))),
                                                                List(
                                                                    SSABlock(SSADL.Label(List(4, 0, 0),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_4_0_0")))),EqualA,BinOp(ExpName(Name(List(Ident("x_4_")))),Add,ExpName(Name(List(Ident("s_4_")))))))))))),
                                                                    List(
                                                                    Phi(Name(List(Ident("args"))),Name(List(Ident("args_4___"))),Map(SSADL.Label(List(4),Some(Pre)) -> Name(List(Ident("args_4_"))))),
                                                                    Phi(Name(List(Ident("x"))),Name(List(Ident("x_4___"))),Map(SSADL.Label(List(4),Some(Pre)) -> Name(List(Ident("x_4_"))))), 
                                                                        Phi(Name(List(Ident("s"))),Name(List(Ident("s_4___"))), Map(SSADL.Label(List(4),Some(Pre)) -> Name(List(Ident("s_4_"))))))))), 
                    SSABlock(SSADL.Label(List(5),None),List(SSAReturn(None))))))
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



class TestSSADL2 extends funsuite.AnyFunSuite with matchers.should.Matchers { // todo, declaration with init does not work yet!
    val METHODSTR = """
public static boolean add(int v) {
    int [] nvals; 
    int i; 
    boolean res;
    nvals = null;
    i=0;
    res=false;
    try {
      if (this.cap < 1){throw new Exception();}
      else {
        if (this.size < this.cap) {
          this.vals[this.size] = v; this.size = this.size + 1;
        } else {
          nvals = new int[this.cap];
          i = 0;
          while (i < this.cap-1) {
            nvals[i] = this.vals[i+1];
            i = i + 1;
          }
          nvals[this.cap-1] = v;
          this.vals = nvals;
        }
      }
      res = true;
    } catch (Exception e) {
      println("Failed with zero capacity.");
    }
    return res;
}
    """
    val ssa = 
SSAMethodDecl(List(Public, Static),List(),Some(PrimType_(BooleanT)),Ident("add"),List(FormalParam(List(),PrimType_(IntT),false,VarId(Ident("v")))),List(),None,SSAMethodBody(
     List(
       SSABlock(SSADL.Label(List(0),None),List(
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6_0_0_1_0_1_2_")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6_0_0_1_0___")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_4")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6_0_0_1_0_1_2___")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6_0_0___")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6_1_0___")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6__")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6_0_0_1_0_1_1")),None))),
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_6___")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6_0_0_1_0_1_2_")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_3")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6_0_0_1_0___")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6_0_0_1_0_1_0")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6_0_0_1_0_1_2___")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6_0_0___")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6_1_0___")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6__")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_6___")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6_0_0_1_0_1_2_")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6_0_1")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6_0_0_1_0___")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_5")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6_0_0_1_0_1_2___")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6_0_0___")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6_1_0___")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6__")),None))),
            SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_6___")),None))),
            SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_0")),None))))),
        SSABlock(SSADL.Label(List(1),None),List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_1")),None))))),
        SSABlock(SSADL.Label(List(2),None),List(SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_2")),None))))),
        SSABlock(SSADL.Label(List(3),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("nvals_3")))),EqualA,Lit(NullLit))))))),
        SSABlock(SSADL.Label(List(4),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("i_4")))),EqualA,Lit(IntLit(0)))))))),
        SSABlock(SSADL.Label(List(5),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("res_5")))),EqualA,Lit(BooleanLit(false)))))))),
        SSABlock(SSADL.Label(List(6),None),List(SSATry(List(SSABlock(SSADL.Label(List(6, 0, 0),None),
            List(
              SSAIf(BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("cap"))),LThan,Lit(IntLit(1))),
                   List(SSABlock(SSADL.Label(List(6, 0, 0, 0, 0),None),List(SSAThrow(InstanceCreation(List(),TypeDeclSpecifier_(ClassType(List((Ident("Exception"),List())))),List(),None))))),
                   List(SSABlock(SSADL.Label(List(6, 0, 0, 1, 0),None),List(SSAIf(BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("size"))),LThan,FieldAccess_(PrimaryFieldAccess(This,Ident("cap")))),
                        List(
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 0, 0),None),List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(FieldAccess_(PrimaryFieldAccess(This,Ident("vals"))),List(FieldAccess_(PrimaryFieldAccess(This,Ident("size")))))),EqualA,ExpName(Name(List(Ident("v")))))))))),
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 0, 1),None),List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(This,Ident("size"))),EqualA,BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("size"))),Add,Lit(IntLit(1)))))))))),
                        List(
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 0),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("nvals_6_0_0_1_0_1_0")))),EqualA,ArrayCreate(PrimType_(IntT),List(FieldAccess_(PrimaryFieldAccess(This,Ident("cap")))),0))))))),
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 1),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("i_6_0_0_1_0_1_1")))),EqualA,Lit(IntLit(0)))))))),
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2),None),List(
                               SSAWhile(List(
                                 Phi(Name(List(Ident("v"))),Name(List(Ident("v_6_0_0_1_0_1_2_"))),
                                   Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 1),None) -> Name(List(Ident("v"))),
                                       SSADL.Label(List(6, 0, 0, 1, 0, 1, 2, 0, 1),None) -> Name(List(Ident("v_6_0_0_1_0_1_2_"))))),
                                 Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6_0_0_1_0_1_2_"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 1),None) -> Name(List(Ident("nvals_6_0_0_1_0_1_0"))),
                                       SSADL.Label(List(6, 0, 0, 1, 0, 1, 2, 0, 1),None) -> Name(List(Ident("nvals_6_0_0_1_0_1_2_"))))),
                                 Phi(Name(List(Ident("i"))),Name(List(Ident("i_6_0_0_1_0_1_2_"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 1),None) -> Name(List(Ident("i_6_0_0_1_0_1_1"))),
                                        SSADL.Label(List(6, 0, 0, 1, 0, 1, 2, 0, 1),None) -> Name(List(Ident("i_6_0_0_1_0_1_2_0_1"))))),
                                 Phi(Name(List(Ident("res"))),Name(List(Ident("res_6_0_0_1_0_1_2_"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 1),None) -> Name(List(Ident("res_5"))),
                                        SSADL.Label(List(6, 0, 0, 1, 0, 1, 2, 0, 1),None) -> Name(List(Ident("res_6_0_0_1_0_1_2_")))))),
                                 BinOp(BinOp(ExpName(Name(List(Ident("i_6_0_0_1_0_1_1")))),LThan,FieldAccess_(PrimaryFieldAccess(This,Ident("cap")))),Sub,Lit(IntLit(1))),
                                 List(
                                 SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2, 0, 0),None),List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(ExpName(Name(List(Ident("nvals_6_0_0_1_0_1_2_")))),List(ExpName(Name(List(Ident("i_6_0_0_1_0_1_2_"))))))),EqualA,ArrayAccess(ArrayIndex(FieldAccess_(PrimaryFieldAccess(This,Ident("vals"))),List(BinOp(ExpName(Name(List(Ident("i_6_0_0_1_0_1_2_")))),Add,Lit(IntLit(1)))))))))))),
                                 SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2, 0, 1),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("i_6_0_0_1_0_1_2_0_1")))),EqualA,BinOp(ExpName(Name(List(Ident("i_6_0_0_1_0_1_2_")))),Add,Lit(IntLit(1)))))))))),
                                 List(
                                 Phi(Name(List(Ident("v"))),Name(List(Ident("v_6_0_0_1_0_1_2___"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2),Some(Pre)) -> Name(List(Ident("v_6_0_0_1_0_1_2_"))))),
                                 Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6_0_0_1_0_1_2___"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2),Some(Pre)) -> Name(List(Ident("nvals_6_0_0_1_0_1_2_"))))),
                                 Phi(Name(List(Ident("i"))),Name(List(Ident("i_6_0_0_1_0_1_2___"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2),Some(Pre)) -> Name(List(Ident("i_6_0_0_1_0_1_2_"))))),
                                 Phi(Name(List(Ident("res"))),Name(List(Ident("res_6_0_0_1_0_1_2___"))),
                                    Map(SSADL.Label(List(6, 0, 0, 1, 0, 1, 2),Some(Pre)) -> Name(List(Ident("res_6_0_0_1_0_1_2_"))))))))),
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 3),None),List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(ExpName(Name(List(Ident("nvals_6_0_0_1_0_1_2___")))),List(BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("cap"))),Sub,Lit(IntLit(1)))))),EqualA,ExpName(Name(List(Ident("v_6_0_0_1_0_1_2___")))))))))),
                           SSABlock(SSADL.Label(List(6, 0, 0, 1, 0, 1, 4),None),List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(This,Ident("vals"))),EqualA,ExpName(Name(List(Ident("nvals_6_0_0_1_0_1_2___"))))))))))),
                     List(
                       Phi(Name(List(Ident("v"))),Name(List(Ident("v_6_0_0_1_0___"))),
                          Map(SSADL.Label(List(6, 0, 0, 1, 0, 0, 1),None) -> Name(List(Ident("v"))), SSADL.Label(List(6, 0, 0, 1, 0, 1, 4),None) -> Name(List(Ident("v_6_0_0_1_0_1_2___"))))),
                       Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6_0_0_1_0___"))),
                          Map(SSADL.Label(List(6, 0, 0, 1, 0, 0, 1),None) -> Name(List(Ident("nvals_3"))), SSADL.Label(List(6, 0, 0, 1, 0, 1, 4),None) -> Name(List(Ident("nvals_6_0_0_1_0_1_2___"))))),
                       Phi(Name(List(Ident("i"))),Name(List(Ident("i_6_0_0_1_0___"))),
                          Map(SSADL.Label(List(6, 0, 0, 1, 0, 0, 1),None) -> Name(List(Ident("i_4"))), SSADL.Label(List(6, 0, 0, 1, 0, 1, 4),None) -> Name(List(Ident("i_6_0_0_1_0_1_2___"))))),
                       Phi(Name(List(Ident("res"))),Name(List(Ident("res_6_0_0_1_0___"))),
                          Map(SSADL.Label(List(6, 0, 0, 1, 0, 0, 1),None) -> Name(List(Ident("res_5"))), SSADL.Label(List(6, 0, 0, 1, 0, 1, 4),None) -> Name(List(Ident("res_6_0_0_1_0_1_2___")))))))))),
           List(
                Phi(Name(List(Ident("v"))),Name(List(Ident("v_6_0_0___"))),
                   Map(SSADL.Label(List(6, 0, 0, 1, 0),None) -> Name(List(Ident("v"))))),
                Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6_0_0___"))),
                   Map(SSADL.Label(List(6, 0, 0, 1, 0),None) -> Name(List(Ident("nvals_3"))))),
                Phi(Name(List(Ident("i"))),Name(List(Ident("i_6_0_0___"))),
                   Map(SSADL.Label(List(6, 0, 0, 1, 0),None) -> Name(List(Ident("i_4"))))),
                Phi(Name(List(Ident("res"))),Name(List(Ident("res_6_0_0___"))),
                   Map(SSADL.Label(List(6, 0, 0, 1, 0),None) -> Name(List(Ident("res_5"))))))))),
      SSABlock(SSADL.Label(List(6, 0, 1),None),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("res_6_0_1")))),EqualA,Lit(BooleanLit(true))))))))),
      List(
         Phi(Name(List(Ident("v"))),Name(List(Ident("v_6__"))),Map(SSADL.Label(List(6, 0, 0, 0, 0),None) -> Name(List(Ident("v"))))),
         Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6__"))),Map(SSADL.Label(List(6, 0, 0, 0, 0),None) -> Name(List(Ident("nvals_3"))))),
         Phi(Name(List(Ident("i"))),Name(List(Ident("i_6__"))),Map(SSADL.Label(List(6, 0, 0, 0, 0),None) -> Name(List(Ident("i_4"))))),
         Phi(Name(List(Ident("res"))),Name(List(Ident("res_6__"))),Map(SSADL.Label(List(6, 0, 0, 0, 0),None) -> Name(List(Ident("res_5")))))),
      FormalParam(List(),RefType_(ClassRefType(ClassType(List((Ident("Exception"),List()))))),false,VarId(Ident("exception_desugared"))),
      List(
         SSABlock(SSADL.Label(List(6, 1, 0),None),
           List(
              SSAIf(InstanceOf(ExpName(Name(List(Ident("exception_desugared")))),ClassRefType(ClassType(List((Ident("Exception"),List()))))),
                  List(
                     SSABlock(SSADL.Label(List(6, 1, 0, 0, 0),None),List(SSAVarDecls(List(),RefType_(ClassRefType(ClassType(List((Ident("Exception"),List()))))),List(VarDecl(VarId(Ident("e_6_1_0_0_0")),Some(InitExp(Cast(RefType_(ClassRefType(ClassType(List((Ident("Exception"),List()))))),ExpName(Name(List(Ident("exception_desugared")))))))))))),
                     SSABlock(SSADL.Label(List(6, 1, 0, 0, 1),None),List(SSAExps(List(ExpStmt(MethodInv(MethodCall(Name(List(Ident("println"))),List(Lit(StringLit("Failed with zero capacity."))))))))))),
                  List(SSABlock(SSADL.Label(List(6, 1, 0, 1, 0),None),List(SSAThrow(ExpName(Name(List(Ident("exception_desugared")))))))),
                  List(
                     Phi(Name(List(Ident("exception_desugared"))),Name(List(Ident("exception_desugared_6_1_0___"))),Map(SSADL.Label(List(6, 1, 0, 0, 1),None) -> Name(List(Ident("exception_desugared"))))),
                     Phi(Name(List(Ident("i"))),Name(List(Ident("i_6_1_0___"))),Map(SSADL.Label(List(6, 1, 0, 0, 1),None) -> Name(List(Ident("i_6__"))))),
                     Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6_1_0___"))),Map(SSADL.Label(List(6, 1, 0, 0, 1),None) -> Name(List(Ident("nvals_6__"))))),
                     Phi(Name(List(Ident("res"))),Name(List(Ident("res_6_1_0___"))),Map(SSADL.Label(List(6, 1, 0, 0, 1),None) -> Name(List(Ident("res_6__"))))),
                     Phi(Name(List(Ident("v"))),Name(List(Ident("v_6_1_0___"))),Map(SSADL.Label(List(6, 1, 0, 0, 1),None) -> Name(List(Ident("v_6__")))))))))),
             List(
               Phi(Name(List(Ident("v"))),Name(List(Ident("v_6___"))),Map(SSADL.Label(List(6, 0, 1),None) -> Name(List(Ident("v_6_0_0___"))), SSADL.Label(List(6, 1, 0),None) -> Name(List(Ident("v_6__"))))),
               Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_6___"))),Map(SSADL.Label(List(6, 0, 1),None) -> Name(List(Ident("nvals_6_0_0___"))), SSADL.Label(List(6, 1, 0),None) -> Name(List(Ident("nvals_6__"))))),
               Phi(Name(List(Ident("i"))),Name(List(Ident("i_6___"))),Map(SSADL.Label(List(6, 0, 1),None) -> Name(List(Ident("i_6_0_0___"))), SSADL.Label(List(6, 1, 0),None) -> Name(List(Ident("i_6__"))))),
               Phi(Name(List(Ident("res"))),Name(List(Ident("res_6___"))),Map(SSADL.Label(List(6, 0, 1),None) -> Name(List(Ident("res_6_0_1"))), SSADL.Label(List(6, 1, 0),None) -> Name(List(Ident("res_6__"))))))))),
      SSABlock(SSADL.Label(List(7),None),List(SSAReturn(Some(ExpName(Name(List(Ident("res_6___")))))))))))


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
    }
}
*/