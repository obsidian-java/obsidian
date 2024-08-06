package obsidian.lang.java

import scala.collection.Map.*
import obsidian.lang.java.scalangj.Syntax.*

/**
  * an AST Path is a sequence of integers that represent the hierachical position of a statement
  * in a AST.
  * 
  * Example
  * int get (int x) {
  *   int i = lpos;  // 0
  *   int r = -1;    // 1
  *   try            // 2
  *   {              // 2,0  * a block stmt
  *     if (x < i)   // 2,0,0
  *     {            // 2,0,0,0  * a block stmt
  *        throw  new Exception(); // 2,0,0,0,0
  *     } 
  *     else 
  *     {            // 2,0,0,1   * a block stmt
  *        while (i < x)   // 2,0,0,1,0 
  *        {               // 2,0,0,1,0,0
  *           int t = f1 + f2;   // 2,0,0,1,0,0,0
  *           f1 = f2;           // 2,0,0,1,0,0,1
  *           f2 = t;            // 2,0,0,1,0,0,2
  *           i = i + 1;         // 2,0,0,1,0,0,3 
  *        }
  *        lpos = i;       // 2,0,0,1,1
  *        r = f2;         // 2,0,0,1,2
  *     }
  *   } 
  *   catch (Exception e)
  *   {              // 2,1  * a block stmt
  *      println("..."); // 2,1,0
  *   }
  *   return r;      // 3 
  * }
  */

object ASTPath {
    type ASTPath = List[Int]

    /**
      * The Queryable type class
      * 
      */
    trait Queryable[A] {
        /**
          * 
          *
          * @param a AST object to query
          * @param p AST path
          * @return an optional BlockStmt ß
          * it makes more sense to return BlockStmt instead of Stmt, which include local class and local var, besides stmt
          * the only inconvenience for using BlockStmt instead of Stmt is that, in case of 
          * querying the root block in a method, which is a Block, we have to return a wrapper BlockStmt_(StmtBlock(block))
          * , we argue that such an inconvenience is fine, as the above use case is pretty low.
          */
        def query(a:A, p:ASTPath):Option[BlockStmt] 
    }

    object queryOps {
        def query[A](a:A, p:ASTPath)(using qu:Queryable[A]):Option[BlockStmt] = {
            qu.query(a,p)
        }
    }

    given memberQueryableInstance:Queryable[MemberDecl] = {
        new Queryable[MemberDecl] {
            override def query(
                a : MemberDecl, p : ASTPath 
            ):Option[BlockStmt] = a match {
                case method@MethodDecl( modifiers,
                    type_params,
                    ty,
                    id,
                    formal_params,
                    ex_types,
                    exp,
                    body
                ) => queryOps.query(body,p)
                case _ => None // we only care about method 
            }
        }
    }


    given methodDeclQueryableInstance:Queryable[MethodDecl] = {
        new Queryable[MethodDecl] {
            override def query(
                a : MethodDecl, p : ASTPath 
            ):Option[BlockStmt] = a match {
                case MethodDecl( modifiers,
                    type_params,
                    ty,
                    id,
                    formal_params,
                    ex_types,
                    exp,
                    body
                ) => queryOps.query(body,p)
            }
        }
    }
    
    given bodyQueryableInstance:Queryable[MethodBody] = {
        new Queryable[MethodBody] {
            override def query(a: MethodBody, p:ASTPath):Option[BlockStmt] = a match {
                case MethodBody(None) => None
                case MethodBody(Some(blk)) =>  queryOps.query(blk, p)
            }
        }
    }
    
    given blockQueryableInstance:Queryable[Block] = {
        new Queryable[Block] {
            override def query(a: Block, p:ASTPath): Option[BlockStmt] = p match {
                case Nil => Some(BlockStmt_(StmtBlock(a)))
                case (i::q) => a match {
                    case Block(stmts) if i < stmts.size => {
                        val bstmt = stmts(i)
                        queryOps.query(bstmt,q)
                    }
                    case Block(stmts) => None
                }
            }
        }
    }
    given BlockStmtQueryableInstance:Queryable[BlockStmt] = {
        new Queryable[BlockStmt] {
            override def query(a: BlockStmt, p:ASTPath): Option[BlockStmt] = p match {
                case Nil => Some(a)
                case (i::q) => a match {
                    case LocalVars(modifiers, ty, var_decls) => None
                    case LocalClass(class_decl) => None
                    case BlockStmt_(stmt) => { // BlockStmt is not part of the AST PATH, hence nothing is consumed
                        queryOps.query(stmt, i::q)
                    }
                }
            }
        }
    }
    given StmtQueryableInstance:Queryable[Stmt] = {
        new Queryable[Stmt] {
            override def query(a:Stmt, p:ASTPath): Option[BlockStmt] = p match {
                case Nil => Some(BlockStmt_(a))
                case (i::q) => a match {
                    // case StmtBlock(blk) if (i == 0) => queryOps.query(blk, q)
                    // case StmtBlock(blk) => None
                    // StmtBlock does not consume a path, it will be consumed by the nested block
                    // refer to test case TestASTPath3
                    case StmtBlock(blk) => queryOps.query(blk, p)
                    case IfThen(e, stmt) if i == 0 => queryOps.query(stmt, q)
                    case IfThen(e, stmt) => None
                    case IfThenElse(e, th, el) if i == 0 => queryOps.query(th, q) 
                    case IfThenElse(e, th, el) if i == 1 => queryOps.query(el, q) 
                    case IfThenElse(e, th, el) => None
                    case While(e, stmt) if i == 0 => queryOps.query(stmt, q)
                    case While(exp, stmt) => None
                    case BasicFor(init, loop_cond, post_update, stmt) if i == 0 => queryOps.query(stmt, q)
                    case BasicFor(init, loop_cond, post_update, stmt) => None
                    case EnhancedFor(modifiers, ty, id, exp, stmt) if i == 0 => queryOps.query(stmt, q) 
                    case EnhancedFor(modifiers, ty, id, exp, stmt) => None
                    case Switch(exp, blocks) if i < blocks.size => queryOps.query(blocks(i),q) 
                    case Switch(exp, blocks) => None
                    case Do(stmt, exp) if i == 0 => queryOps.query(stmt,q)
                    case Do(stmt, exp) => None
                    case Synchronized(exp, blk) if i == 0 => queryOps.query(blk, q)
                    case Synchronized(exp, blk) => None
                    case Try(try_blk, catches, finally_blk) if i == 0 => queryOps.query(try_blk, q)
                    case Try(try_blk, catches, finally_blk) if (i-1) < catches.size => queryOps.query(catches(i-1), q)
                    case Try(try_blk, catches, Some(finally_blk)) if i == catches.size + 1 => queryOps.query(finally_blk, q)
                    case Try(try_blk, catches, finally_blk) => None
                    case Labeled(id, stmt) => queryOps.query(stmt,p) // label have the same path as its containing statement, this is for the ease of CFG construction.
                    // i.e. l1: switch (e) { ... }, we can build a mapping from l1 to the path where the switch statement is located.
                    case _ => None
                }
            }
        }
    }
    given switchBlockQueryableInstance:Queryable[SwitchBlock] = {
        new Queryable[SwitchBlock] {
            override def query(a: SwitchBlock, p: ASTPath): Option[BlockStmt] = a match {
                case SwitchBlock(label, blk_stmts) => p match {
                    case Nil => Some(BlockStmt_(StmtBlock(Block(blk_stmts))))
                    case (i::q) if i < blk_stmts.size => queryOps.query(blk_stmts(i), q)
                    case (i::q) => None
                }
            }
        }
    }

    given catchQueryableInstrance:Queryable[Catch] = {
        new Queryable[Catch] {
            override def query(a: Catch, p: ASTPath): Option[BlockStmt] = a match {
                case Catch(params, blk) => queryOps.query(blk,p) 
            }
        }
    }


    // AST Path name constructors (selected)

    val rootPath: ASTPath = List()
    def childOf(p:ASTPath, childIdx:Int): ASTPath = p ++ List(childIdx)
    def thenOf(p:ASTPath): ASTPath = p ++ List(0)
    def elseOf(p:ASTPath): ASTPath = p ++ List(1) 
    def tryOf(p:ASTPath): ASTPath = p ++ List(0)


    def apToStr(p:ASTPath):String = p.mkString("_")
}
