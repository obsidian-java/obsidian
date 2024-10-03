package obsidian.lang.java.obsidian

import scala.io.*
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Pretty.prettyPrint
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.obsidian.*

import obsidian.lang.java.obsidian.Obfuscate.*


object Obsidian {
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

    def generateObfuscatedCode(cu: CompilationUnit): String = prettyPrint(run(cu))
}
