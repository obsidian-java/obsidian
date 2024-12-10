package obsidian.lang.java.obsidian

/**
  * This module contains all the required APIs. 
  */
import obsidian.lang.java.scalangj.Syntax.*

import obsidian.lang.java.obsidian.Flatten.* 
import obsidian.lang.java.obsidian.MinSSA.*
import obsidian.lang.java.obsidian.CPS.*


object Obfuscate {
    type Err = String
    def obfMemberMethod(member:MemberDecl):Either[Err, MemberDecl] = {
        member match {
            case ConstructorDecl(modifiers, type_params, id, formal_parms, ex_types, body) => Left("Constructor Method is not supported.")
            case FieldDecl(modifiers, ty, var_decls) => Left("Field Declaration is not supported.")
            case MemberClassDecl(class_decl) => Left("Member class is not supported.")
            case MemberInterfaceDecl(iface_decl) => Left("Member Interface is not supported.")
            case MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body) if id == Ident("main") => Left("Main method is not supported.")
            case MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body) => {
                Flatten.flatMethodDecl(MethodDecl(modifiers, type_params, ty, id, formal_params, ex_types, exp, body)).run(Flatten.initStateInfo) match {
                    case FlatResult.FlatError(message) => {
                        Left(s"Flattening failed with ${message}");
                    } 
                    case FlatResult.FlatOk((st, f_methodDecl)) => {
                        val d_methodDecl = Desugar.dsgOps.desugar(f_methodDecl)
                        MinSSA.kmethodDecl(d_methodDecl).run(MinSSA.initState) match {
                            case MinSSA.SSAResult.SSAError(message) => {
                                Left(s"SSA construction failed with ${message}");
                            }
                            case MinSSA.SSAResult.SSAOk((st, ssa_methodDecl)) => { 
                                val charcodes = getCharCodes(st)
                                CPS.cpsmethoddecl(ssa_methodDecl).run(CPS.initState(charcodes)) match {
                                    case CPSResult.CPSError(message) => {
                                        Left(s"SSA to CPS conversion failed with ${message}");
                                    }
                                    case CPSResult.CPSOk((st, cps_methodDecl)) => 
                                        Right(cps_methodDecl)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
