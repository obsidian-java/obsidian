package obsidian.lang.java

import scala.io.*
import com.github.luzhuomi.scalangj.Parser.*
import com.github.luzhuomi.scalangj.Pretty.*
import com.github.luzhuomi.scalangj.Syntax.*
import com.github.luzhuomi.scalangj.Lexer
import obsidian.lang.java.*

import obsidian.lang.java.Flatten.* 
import obsidian.lang.java.MinSSA.*
import obsidian.lang.java.CPS.*
import os.Path



object Main extends App {
    val STRING = """
public class Fib
{


    public static void f() {
        int x; 
        int s;
        x = 0;
        s = 0;
        while (x < 10) {
            s = x + s;
        }
    }

    public static int fib(int n)
    {
        int f1;
        int f2;
        int i;
        int t;
        f1 =1;
        f2 =1;
        i = 0;
        t = 0; // all variables must be initialized
        while(i<n) {
            t = f1 + f2;
            f1 = f2;
            f2 = t;
            i = i +1;
        }
        return f2;
    }

    public static void  main(String argv[]) {
	System.out.println(fib(10));
    }
}
    """
    def run(cu:CompilationUnit):CompilationUnit = cu match {
        case CompilationUnit(pkg_decl, imp_decls, type_decls) => {
            val obs_type_decls = type_decls.map( ty_decl => ty_decl match {
                case InterfaceTypeDecl(iface_decl) => ty_decl
                case ClassTypeDecl(class_decl) => {
                    val obs_class_decl = class_decl match {
                        case EnumDecl(modifiers, id, ref_types, body) => class_decl
                        case ClassDecl_(modifiers, id, type_params, ref_type, ref_types, body) => {
                            val obs_body = body match {
                                case ClassBody(decls) => ClassBody(decls.map( decl => decl match {
                                    case InitDecl(is_static, blk) => decl
                                    case MemberDecl_(member) => {
                                        val obs_member =member match {
                                            case ConstructorDecl(modifiers, type_params, id, formal_parms, ex_types, body) => member
                                            case FieldDecl(modifiers, ty, var_decls) => member
                                            case MemberClassDecl(class_decl) => member // TODO
                                            case MemberInterfaceDecl(iface_decl) => member // TODO
                                            case MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body) if id == Ident("main") => member
                                            case MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body) => {
                                                Flatten.flatMethodDecl(MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body)).run(Flatten.initStateInfo) match {
                                                    case FlatResult.FlatError(message) => {
                                                        println(message);
                                                        member // failed
                                                    } 
                                                    case FlatResult.FlatOk((st, f_methodDecl)) => {
                                                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                                                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.initState) match {
                                                            case MinSSA.SSAResult.SSAError(message) => {
                                                                println(message)
                                                                member // failed
                                                            }
                                                            case MinSSA.SSAResult.SSAOk((st, ssa_methodDecl)) => { 
                                                                val charcodes = getCharCodes(st)
                                                                CPS.cpsmethoddecl(ssa_methodDecl).run(CPS.initState(charcodes)) match {
                                                                    case CPSResult.CPSError(message) => {
                                                                        println(message)
                                                                        member // failed
                                                                    }
                                                                    case CPSResult.CPSOk((st, cps_methodDecl)) => 
                                                                       cps_methodDecl
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        MemberDecl_(obs_member)
                                    }
                                }))
                            }
                            println(id)
                            ClassDecl_(modifiers, id, type_params, ref_type, ref_types, obs_body)
                        }
                    }
                    ClassTypeDecl(obs_class_decl)
                } 
            })
            CompilationUnit(pkg_decl, imp_decls, obs_type_decls)
        }
    } // TODO

    val eCU = parseCompilationUnit(STRING)
    eCU match {
        case Left(error_msg) => println(error_msg)
        case Right(cu) => {
            val path:os.Path = os.pwd / "output" / "output.java"
            val obs_cu = run(cu)
            
            println( prettyPrint(cu))
            os.write(path, prettyPrint(obs_cu)) 
        }
    }
}
