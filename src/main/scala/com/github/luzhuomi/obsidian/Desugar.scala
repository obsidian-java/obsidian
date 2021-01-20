package com.github.luzhuomi.obsidian

import scala.collection.Map._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Syntax


object Desugar {
    trait DSG[A] {
        def desugar(a:A):A
    }

    object dsgOps {
        def desugar[A](a:A)(implicit d:DSG[A]):A = {
            d.desugar(a)
        }
    }

    implicit def methodDSGInstance: DSG[MethodDecl] = {
        new DSG[MethodDecl] {
            override def desugar(a:MethodDecl):MethodDecl = a match {
                case MethodDecl(
                modifiers,
                type_params,
                return_ty,
                fname,
                formal_params,
                ex_types,
                exp,
                body
              ) => MethodDecl(
                modifiers,
                type_params,
                return_ty,
                fname,
                formal_params,
                ex_types,
                exp,
                dsgOps.desugar(body)
              )
            }
        }
    }

    implicit def bodyDSGInstance: DSG[MethodBody] = {
        new DSG[MethodBody] {
            override def desugar(a: Syntax.MethodBody): Syntax.MethodBody = a match {
                case MethodBody(Some(blk)) => {
                    val sblk = dsgOps.desugar(blk)
                    MethodBody(Some(sblk))
                }
                case _ => a
            }
        }
    }

    implicit def blockDSGInstance: DSG[Block] = {
        new DSG[Block] {
            override def desugar(a: Syntax.Block): Syntax.Block = a match {
                case Block(stmts) => {
                    val sstmts = stmts.map(stmt => dsgOps.desugar(stmt))
                    Block(sstmts)
                }
            }
        }
    }
    implicit def blockStmtDSGInstance: DSG[BlockStmt] = {
        new DSG[BlockStmt] {
            override def desugar(a: Syntax.BlockStmt): Syntax.BlockStmt = a match {
                case LocalVars(modifiers, ty, var_decls) => a
                case LocalClass(class_decl) => a
                case BlockStmt_(stmt) => BlockStmt_(dsgOps.desugar(stmt))
            }
        }
    }

    implicit def stmtDSGInstance: DSG[Stmt] = {
        new DSG[Stmt] {
            override def desugar(a: Syntax.Stmt): Syntax.Stmt = a match {
                case Assert(exp, msg) => Assert(dsgOps.desugar(exp), msg)
                case BasicFor(init, loop_cond, post_update, stmt) => StmtBlock(Block(List(
                    val pre:List[BlockStmt] = init match {
                        case None => Nil
                        case Some(for_init) => for_init match {
                            case ForLocalVars(modifiers, ty, var_decls) => {
                                List(LocalVars(modifiers, ty, var_decls))
                            }
                            case ForInitExps(exps) => exps.map(exp => {
                                BlockStmt_(ExpStmt(dsgOps.desugar(exp))
                            })

                        } 
                    }
                    val cond = loop_cond match { 
                        case None => Lit(BooleanLit(true))
                        case Some(exp) => dsOps.desugar(exp)
                    }
                    val stmt_post_update = post_update match {
                        case None => dsOps.desugar(stmt)
                        case Some(exps) => {
                            val post_update_blkstmts = exps.map(exp => {
                                BlockStmt_(ExpStmt(dsgOps.desugar(exp))
                            })
                            appBlockStmts(stmt, post_update_blkstmts)
                        }
                    }
                    val loop = While(dsgOps.desugar(cond), dsOps.desugar(stmt))
                    pre ++ List(BlockStmt_(loop))
                )))
                case IfThen(exp,stmt) => IfThenElse(dsgOps.desugar(exp), dsgOps.desugar(stmt), Empty)
                case IfThenElse(exp,th,el) => IfThenElse(dsgOps.desugar(exp), dsOps.desguar(th), dsOps.desguar(el))
                case Do(stmt, exp) => StmtBlock(Block(List( 
                    BlockStmt_(dsgOps.desugar(stmt)), 
                    BlockStmt_(While(dsgOps.desugar(exp),dsgOps.desugar(stmt)))
                )))
                case 
            }
        }
    }
}
