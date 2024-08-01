package obsidian.lang.java

import scala.io.*
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.*

import obsidian.lang.java.Obfuscate.*
import os.Path



object Main extends App {
    val STRING = """
public class Fib
{


    public static int f() {
        int x; 
        int s;
        x = 0;
        s = 0;
        while (x < 10) {
            s = x + s;
            x = x + 1;
        }
        return s;
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
                                        val obs_member = obfMemberMethod(member) match {
                                            case Left(err) => {
                                                println(err) 
                                                member
                                            }
                                            case Right(m) => m
                                        }
                                        MemberDecl_(obs_member)
                                    }
                                }))
                            }
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
