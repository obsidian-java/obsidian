package obsidian.lang.java.obsidian


import org.scalatest.{funsuite, matchers}

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.obsidian.*
import obsidian.lang.java.obsidian.Flatten.*
import obsidian.lang.java.obsidian.MinSSA.*


class TestMinSSA1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
    
    val ssa = SSAMethodDecl(List(Public, Static),List(),None,Ident("main"),
                List(FormalParam(List(),RefType_(ArrayType(RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))))),false,VarId(Ident("args")))),List(),None,
                SSAMethodBody(List(
                    SSABlock(THead(TBox),
                        List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TBox)))")),None))), 
                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(TTail(TTail(THead(TWhilePrePhi(1))))))")),None))), 
                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_TTail(TTail(TTail(THead(TBox))))")),None))), 
                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_TTail(TTail(TTail(TTail(THead(TWhilePrePhi(1))))))")),None))), 
                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox)))))))")),None))), 
                            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_THead(TBox)")),None))))), 
                    SSABlock(TTail(THead(TBox)),
                        List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_TTail(THead(TBox))")),None))))), 
                    SSABlock(TTail(TTail(THead(TBox))),
                        List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_TTail(TTail(THead(TBox)))")))),EqualA,Lit(IntLit(0)))))))), 
                    SSABlock(TTail(TTail(TTail(THead(TBox)))),
                        List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_TTail(TTail(TTail(THead(TBox))))")))),EqualA,Lit(IntLit(0)))))))), 
                    SSABlock(TTail(TTail(TTail(TTail(THead(TBox))))),
                        List(SSAWhile(
                            List(Phi(Name(List(Ident("s"))),Name(List(Ident("s_TTail(TTail(TTail(TTail(THead(TWhilePrePhi(1))))))"))) // phi assignment
                                    ,List((TTail(TTail(TTail(THead(TBox)))),Name(List(Ident("s_TTail(TTail(TTail(THead(TBox))))")))), 
                                          (TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox))))))),Name(List(Ident("s_TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox)))))))"))))))),
                            BinOp(ExpName(Name(List(Ident("x_TTail(TTail(THead(TBox)))")))),LThan,Lit(IntLit(10))),
                            List(
                                SSABlock(TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox))))))),
                                    List(
                                        SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox)))))))")))),EqualA,
                                                            BinOp(ExpName(Name(List(Ident("x_TTail(TTail(THead(TBox)))")))),Add,
                                                             ExpName(Name(List(Ident("s_TTail(TTail(TTail(TTail(THead(TWhilePrePhi(1))))))"))))))))))))))), 
                                SSABlock(TTail(TTail(TTail(TTail(TTail(TLast(TBox)))))),List(SSAReturn(None))))))
    import FlatResult.*
    import SSAResult.*
    test("TestMinSSA1") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case FlatError(message) => fail(message)
                    case FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.debugInitState) match {
                            case SSAError(message) => fail(message)
                            case SSAOk((st, ssa_methodDecl)) => { 
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


/* v should not be in the phi list */


class TestMinSSA2 extends funsuite.AnyFunSuite with matchers.should.Matchers { // todo, declaration with init does not work yet!
    val METHODSTR = """
public static boolean add(int v) {
    int [] nvals; 
    int i; 
    boolean res;
    nvals = null;
    i=0;
    res=false;
    if (this.cap < 1){res = false;}
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
        res = true;
    }
    return res;
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val ssa = SSAMethodDecl(List(Public, Static),List(),Some(PrimType_(BooleanT)),Ident("add"),List(FormalParam(List(),PrimType_(IntT),false,VarId(Ident("v")))),List(),None,
        SSAMethodBody(
            List(
                SSABlock(THead(TBox),List(SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))")),None))), 
                                          SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))")),None))), 
                                          SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(THead(TBox)))))))))))")),None))), 
                                          SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_TTail(TTail(TTail(THead(TBox))))")),None))), 
                                          SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(THead(TBox))))))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(TTail(TLast(TBox))))))))))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(TTail(TTail(TTail(THead(TBox)))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(TTail(TTail(TTail(THead(TBox))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox)))))))))")),None))), 
                                          SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox))))))))))")),None))), 
                                          SSAVarDecls(List(),RefType_(ArrayType(PrimType_(IntT))),List(VarDecl(VarId(Ident("nvals_THead(TBox)")),None))))), 
                SSABlock(TTail(THead(TBox)),List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("i_TTail(THead(TBox))")),None))))), 
                SSABlock(TTail(TTail(THead(TBox))),List(SSAVarDecls(List(),PrimType_(BooleanT),List(VarDecl(VarId(Ident("res_TTail(TTail(THead(TBox)))")),None))))), 
                SSABlock(TTail(TTail(TTail(THead(TBox)))),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("nvals_TTail(TTail(TTail(THead(TBox))))")))),EqualA,Lit(NullLit))))))), 
                SSABlock(TTail(TTail(TTail(TTail(THead(TBox))))),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("i_TTail(TTail(TTail(TTail(THead(TBox)))))")))),EqualA,Lit(IntLit(0)))))))), 
                SSABlock(TTail(TTail(TTail(TTail(TTail(THead(TBox)))))),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(THead(TBox))))))")))),EqualA,Lit(BooleanLit(false)))))))), 
                SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TBox))))))),
                    List(SSAIf(BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("cap"))),LThan,Lit(IntLit(1))),
                        List(SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox))))))))), // then
                            List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox)))))))))")))),EqualA,Lit(BooleanLit(false))))))))),
                        List(SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TBox))))))))), // else 
                            List(SSAIf(BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("size"))),LThan,FieldAccess_(PrimaryFieldAccess(This,Ident("cap")))),
                                List(SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TThen(THead(TBox))))))))))), // then
                                        List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(FieldAccess_(PrimaryFieldAccess(This,Ident("vals"))),List(FieldAccess_(PrimaryFieldAccess(This,Ident("size")))))),EqualA,ExpName(Name(List(Ident("v")))))))))), 
                                    SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TThen(TTail(TLast(TBox)))))))))))),
                                        List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(This,Ident("size"))),EqualA,BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("size"))),Add,Lit(IntLit(1)))))))))),
                                List(SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(THead(TBox))))))))))), // else 
                                    List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(THead(TBox)))))))))))")))),EqualA,ArrayCreate(PrimType_(IntT),List(FieldAccess_(PrimaryFieldAccess(This,Ident("cap")))),0))))))), 
                                    SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(THead(TBox)))))))))))),
                                        List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(THead(TBox))))))))))))")))),EqualA,Lit(IntLit(0)))))))), 
                                    SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TBox))))))))))))),
                                        List(SSAWhile(
                                            List(Phi(Name(List(Ident("i"))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))"))), // while phi
                                                    List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(THead(TBox)))))))))))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(THead(TBox))))))))))))")))), 
                                                         (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(TTail(TLast(TBox)))))))))))))))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(TTail(TLast(TBox))))))))))))))))"))))))),
                                            BinOp(BinOp(ExpName(Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))),LThan,FieldAccess_(PrimaryFieldAccess(This,Ident("cap")))),Sub,Lit(IntLit(1))),
                                            List(SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(THead(TBox))))))))))))))),
                                                    List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(ExpName(Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(THead(TBox)))))))))))")))),List(ExpName(Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))"))))))),
                                                        EqualA,ArrayAccess(ArrayIndex(FieldAccess_(PrimaryFieldAccess(This,Ident("vals"))),List(BinOp(ExpName(Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))),Add,Lit(IntLit(1)))))))))))), 
                                                SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(TTail(TLast(TBox)))))))))))))))),
                                                    List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(TTail(TLast(TBox))))))))))))))))")))),EqualA,BinOp(ExpName(Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))),Add,Lit(IntLit(1))))))))))))), 
                                                SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(TTail(THead(TBox)))))))))))))),
                                                    List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(ExpName(Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))),List(BinOp(FieldAccess_(PrimaryFieldAccess(This,Ident("cap"))),Sub,Lit(IntLit(1)))))),EqualA,
                                                        ExpName(Name(List(Ident("v_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))))))))), 
                                                SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(TTail(TTail(TLast(TBox))))))))))))))),
                                                    List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(This,Ident("vals"))),EqualA,ExpName(Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))"))))))))))),
                                List(Phi(Name(List(Ident("i"))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))"))), // end if phi
                                    List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TThen(TTail(TLast(TBox)))))))))))),Name(List(Ident("i_TTail(TTail(TTail(TTail(THead(TBox)))))")))), 
                                         (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(TTail(TTail(TLast(TBox))))))))))))))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhile(TTail(TLast(TBox))))))))))))))))")))))), 
                                    Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))"))),
                                    List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TThen(TTail(TLast(TBox)))))))))))),Name(List(Ident("nvals_TTail(TTail(TTail(THead(TBox))))")))), 
                                         (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(TTail(TTail(TLast(TBox))))))))))))))),Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))))), 
                                    Phi(Name(List(Ident("v"))),Name(List(Ident("v_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))"))),
                                    List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TThen(TTail(TLast(TBox)))))))))))),Name(List(Ident("v")))), 
                                         (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(TTail(TTail(TLast(TBox))))))))))))))),Name(List(Ident("v_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))))), 
                                    Phi(Name(List(Ident("res"))),Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))"))),
                                    List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TThen(TTail(TLast(TBox)))))))))))),Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(THead(TBox))))))")))), 
                                        (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(TTail(TTail(TLast(TBox))))))))))))))),Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TElse(TTail(TTail(THead(TWhilePrePhi(1))))))))))))))")))))))))), 
                            SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox)))))))))),
                                List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox))))))))))")))),EqualA,Lit(BooleanLit(true))))))))),
                    List(Phi(Name(List(Ident("res"))),Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))"))), // end if phi
                        List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox))))))))),Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox)))))))))")))), 
                            (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox)))))))))),Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox))))))))))")))))), 
                        Phi(Name(List(Ident("i"))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))"))),
                            List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox))))))))),Name(List(Ident("i_TTail(TTail(TTail(TTail(THead(TBox)))))")))), 
                                 (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox)))))))))),Name(List(Ident("i_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))")))))), 
                        Phi(Name(List(Ident("nvals"))),Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))"))),
                            List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox))))))))),Name(List(Ident("nvals_TTail(TTail(TTail(THead(TBox))))")))), 
                                 (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox)))))))))),Name(List(Ident("nvals_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))")))))), 
                        Phi(Name(List(Ident("v"))),Name(List(Ident("v_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))"))),
                            List((TTail(TTail(TTail(TTail(TTail(TTail(THead(TThen(TLast(TBox))))))))),Name(List(Ident("v")))), (TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(TTail(TLast(TBox)))))))))),
                                Name(List(Ident("v_TTail(TTail(TTail(TTail(TTail(TTail(THead(TElse(THead(TIfPostPhi)))))))))")))))))))), 
                SSABlock(TTail(TTail(TTail(TTail(TTail(TTail(TTail(TLast(TBox)))))))),
                    List(SSAReturn(Some(ExpName(Name(List(Ident("res_TTail(TTail(TTail(TTail(TTail(TTail(THead(TIfPostPhi)))))))")))))))))))
    import FlatResult.*
    import SSAResult.*

    test("TestMinSSA2") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case FlatError(message) => fail(message)
                    case FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.debugInitState) match {
                            case SSAError(message) => fail(message)
                            case SSAOk((st, ssa_methodDecl)) => { 
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




class TestMinSSA3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void f(int v) {
	int x; 
    x = 0;
    while (x > v) {
        x = x - v;
    }
}
    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    val ssa = SSAMethodDecl(List(Public, Static),List(),None,Ident("f"),List(FormalParam(List(),PrimType_(IntT),false,VarId(Ident("v")))),List(),None,
        SSAMethodBody(
            List(SSABlock(THead(TBox),
                    List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(THead(TBox))")),None))), 
                    SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TWhilePrePhi(1))))")),None))), 
                    SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TWhile(TLast(TBox)))))")),None))), 
                    SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_THead(TBox)")),None))))), 
                SSABlock(TTail(THead(TBox)),
                    List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_TTail(THead(TBox))")))),EqualA,Lit(IntLit(0)))))))), 
                SSABlock(TTail(TTail(THead(TBox))),
                    List(SSAWhile(
                        List(Phi(Name(List(Ident("x"))),Name(List(Ident("x_TTail(TTail(THead(TWhilePrePhi(1))))"))),
                            List((TTail(THead(TBox)),Name(List(Ident("x_TTail(THead(TBox))")))), 
                                (TTail(TTail(THead(TWhile(TLast(TBox))))),Name(List(Ident("x_TTail(TTail(THead(TWhile(TLast(TBox)))))"))))))),
                        BinOp(ExpName(Name(List(Ident("x_TTail(TTail(THead(TWhilePrePhi(1))))")))),GThan,ExpName(Name(List(Ident("v"))))),
                        List(SSABlock(TTail(TTail(THead(TWhile(TLast(TBox))))),
                                List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_TTail(TTail(THead(TWhile(TLast(TBox)))))")))),EqualA,BinOp(ExpName(Name(List(Ident("x_TTail(TTail(THead(TWhilePrePhi(1))))")))),Sub,ExpName(Name(List(Ident("v"))))))))))))))), 
                SSABlock(TTail(TTail(TTail(TLast(TBox)))),List(SSAReturn(None))))))
    import FlatResult.*
    import SSAResult.*

    test("TestMinSSA3") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case FlatError(message) => fail(message)
                    case FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.debugInitState) match {
                            case SSAError(message) => fail(message)
                            case SSAOk((st, ssa_methodDecl)) => { 
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


/*
public static void f(int v) {
	int x; 
    x = 0;
    if (x < 10) {
        x = x + 1;
    } else {
        while (x > v) {
            x = x - v;
        }
    }
}
*/


class TestMinSSA4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR = """
public static void f(int v) {
	int x; 
    x = 0;
    if (x < 10) {
        x = x + 1;
    } else {
        while (x > v) {
            x = x - v;
        }
    }
}    """
    val methoddecl:Decl = classBodyStatement.apply(new Lexer.Scanner(METHODSTR)).get.get
    
    val ssa = SSAMethodDecl(List(Public, Static),List(),None,Ident("f"),List(FormalParam(List(),PrimType_(IntT),false,VarId(Ident("v")))),List(),None,
        SSAMethodBody(List(
        SSABlock(THead(TBox),List(
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TIfPostPhi)))")),None))), 
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox)))))))")),None))), 
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TElse(TLast(TWhilePrePhi(1))))))")),None))), 
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(THead(TBox))")),None))), 
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_TTail(TTail(THead(TThen(TLast(TBox)))))")),None))), 
            SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_THead(TBox)")),None))))), 
        SSABlock(TTail(THead(TBox)),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_TTail(THead(TBox))")))),EqualA,Lit(IntLit(0)))))))), 
        SSABlock(TTail(TTail(THead(TBox))),List(SSAIf(BinOp(ExpName(Name(List(Ident("x_TTail(THead(TBox))")))),LThan,Lit(IntLit(10))),
            List(SSABlock(TTail(TTail(THead(TThen(TLast(TBox))))), // then
                List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_TTail(TTail(THead(TThen(TLast(TBox)))))")))),EqualA,BinOp(ExpName(Name(List(Ident("x_TTail(THead(TBox))")))),Add,Lit(IntLit(1)))))))))),
            List(SSABlock(TTail(TTail(THead(TElse(TLast(TBox))))), // else 
                List(SSAWhile(
                    List(Phi(Name(List(Ident("x"))),Name(List(Ident("x_TTail(TTail(THead(TElse(TLast(TWhilePrePhi(1))))))"))),
                        List((TTail(THead(TBox)),Name(List(Ident("x_TTail(THead(TBox))")))), 
                         (TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox))))))),Name(List(Ident("x_TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox)))))))"))))))),BinOp(ExpName(Name(List(Ident("x_TTail(TTail(THead(TElse(TLast(TWhilePrePhi(1))))))")))),GThan,ExpName(Name(List(Ident("v"))))),
                    List(SSABlock(TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox))))))),
                        List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox)))))))")))),EqualA,BinOp(ExpName(Name(List(Ident("x_TTail(TTail(THead(TElse(TLast(TWhilePrePhi(1))))))")))),Sub,ExpName(Name(List(Ident("v")))))))))))))))),
            List(Phi(Name(List(Ident("x"))),Name(List(Ident("x_TTail(TTail(THead(TIfPostPhi)))"))),
                List((TTail(TTail(THead(TThen(TLast(TBox))))),Name(List(Ident("x_TTail(TTail(THead(TThen(TLast(TBox)))))")))), 
                     (TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox))))))),Name(List(Ident("x_TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox)))))))")))))), 
                Phi(Name(List(Ident("v"))),Name(List(Ident("v_TTail(TTail(THead(TIfPostPhi)))"))),
                List((TTail(TTail(THead(TThen(TLast(TBox))))),Name(List(Ident("v")))), 
                     (TTail(TTail(THead(TElse(TLast(TWhile(TLast(TBox))))))),Name(List(Ident("v_TTail(TTail(THead(TElse(TLast(TWhilePrePhi(1))))))")))))))))), 
        SSABlock(TTail(TTail(TTail(TLast(TBox)))),List(SSAReturn(None))))))

    import FlatResult.*
    import SSAResult.*

    test("TestMinSSA4") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case FlatError(message) => fail(message)
                    case FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.debugInitState) match {
                            case SSAError(message) => fail(message)
                            case SSAOk((st, ssa_methodDecl)) => { 
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


class TestMinSSA5 extends funsuite.AnyFunSuite with matchers.should.Matchers { // same as TestMinSSA1 with default state config
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
    
    val ssa = SSAMethodDecl(List(Public, Static),List(),None,Ident("main"),List(FormalParam(List(),RefType_(ArrayType(RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))))),false,VarId(Ident("args")))),List(),None,
        SSAMethodBody(List(SSABlock(THead(TBox),
            List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_3320")),None))), 
                 SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_333327")),None))), 
                 SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_33320")),None))), 
                 SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_333327")),None))), 
                 SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_33332810")),None))), 
                 SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x_20")),None))))), 
            SSABlock(TTail(THead(TBox)),List(SSAVarDecls(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("s_320")),None))))), 
            SSABlock(TTail(TTail(THead(TBox))),List(
                SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("x_3320")))),EqualA,Lit(IntLit(0)))))))), 
            SSABlock(TTail(TTail(TTail(THead(TBox)))),List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_33320")))),EqualA,Lit(IntLit(0)))))))), 
            SSABlock(TTail(TTail(TTail(TTail(THead(TBox))))),
                List(SSAWhile(
                    List(Phi(Name(List(Ident("s"))),Name(List(Ident("s_333327"))),
                        List((TTail(TTail(TTail(THead(TBox)))),Name(List(Ident("s_33320")))), 
                            (TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox))))))),Name(List(Ident("s_33332810"))))))),
                    BinOp(ExpName(Name(List(Ident("x_3320")))),LThan,Lit(IntLit(10))),
                    List(SSABlock(TTail(TTail(TTail(TTail(THead(TWhile(TLast(TBox))))))),
                        List(SSAAssignments(List(ExpStmt(Assign(NameLhs(Name(List(Ident("s_33332810")))),EqualA,BinOp(ExpName(Name(List(Ident("x_3320")))),Add,ExpName(Name(List(Ident("s_333327"))))))))))))))), 
            SSABlock(TTail(TTail(TTail(TTail(TTail(TLast(TBox)))))),List(SSAReturn(None))))))
    import FlatResult.*
    import SSAResult.*

    test("TestMinSSA5") {
        methoddecl match {
            case MemberDecl_(methodDecl@MethodDecl(_,_,_,_,_,_,_,_)) => {
                Flatten.flatMethodDecl(methodDecl).run(Flatten.initStateInfo) match {
                    case FlatError(message) => fail(message)
                    case FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.initState) match {
                            case SSAError(message) => fail(message)
                            case SSAOk((st, ssa_methodDecl)) => { 
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
