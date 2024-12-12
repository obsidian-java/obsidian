package obsidian.lang.java.obsidian


import cats.kernel.Semilattice
import cats.*
import cats.implicits.*
import cats.data.StateT
import obsidian.lang.java.scalangj.Syntax.*
import scala.collection.immutable

import obsidian.lang.java.obsidian.Common.*
import obsidian.lang.java.obsidian.ASTPath.*
import obsidian.lang.java.obsidian.ASTUtils.*

/**
 * all variables must be declared and initialized in the source program
 * otherwise Rleq fails to find an LUB.
 * */

object MinSSA {
  case class SSAMethodDecl( // method decl
      modifiers: List[Modifier],
      type_params: List[TypeParam],
      ty: Option[Type],
      id: Ident,
      formal_params: List[FormalParam],
      ex_types: List[ExceptionType],
      exp: Option[Exp],
      body: SSAMethodBody
  )

  case class SSAMethodBody( // the body of a method, which is a list of blocks.
      blocks: List[SSABlock]
  )

  case class SSABlock( // an SSA block is a label with a list of statements.
      label: Label,
      stmts: List[SSAStmt]
  )

  type Label = TCtx 

  sealed trait SSAStmt 
  // to handle nested decl, not in the paper
  case class SSAVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl]) extends SSAStmt // TODO: it should contain no change, but double check

  case class SSAAssert(exp:Exp, msg:Option[Exp]) extends SSAStmt   // TODO: it should contain no change, but double check

  case class SSAAssignments(stmts: List[Stmt]) extends SSAStmt // a list of assignment statements, we assume there is no exception occur 

  case class SSAExps(stmts: List[Stmt]) extends SSAStmt // a list of expression statements, we assume there is no exception occur
  
  case class SSAReturn(oexp:Option[Exp]) extends SSAStmt

  case class SSAThrow(exp:Exp) extends SSAStmt

  /* KIV for now, they are not supposed to work.
  case class SSABreak(tlbl:Label) extends SSAStmt

  case class SSAContinue(tlbl:Label) extends SSAStmt
  */ 

  // it defers from the paper here, we don't keep a method invocation as a seperate case
  //      it is combined with the assignments and exps cases, as there might be multiple
  //      function calls within a single statement. 
  //      we keep track of the function call and its context in the state
  // case class SSAMethodInvocation(methodInv:MethodInvocation) extends SSAStmt

  case object SSAEmpty extends SSAStmt // nop

  /**
    * 
    *
    * @param tryStmts: try blocks
    * @param catchParam: parameters for the catch block
    * @param catchPhis: 
    * @param catchStmts: catch blocks
    * // 
    * @param joinPhis:
    * @param joinStmts:
    * @param exceptPhis:
    */
  case class SSATry(
    tryStmts:List[SSABlock],    // the first B bar
    catchParam: FormalParam,    // the ex
    catchPhis: List[Phi],       // the phi assignments before the catch block
    catchStmts:List[SSABlock],  // the 2nd B bar (i.e. the catch block)
    joinPhis: List[Phi],        // the phi assignments before the join block
    joinStmts: List[SSABlock],  // the 3rd B bar 
      // (i.e. the continuation after the try catch block,
      //  if 1) none from tryStmts throws or 
      //  2)  and some statement from tryStmts throws and none from catchStmts throws)
    exceptPhis: List[Phi]       // the phi assignments for the except 
  ) extends SSAStmt

  
  /**
    * 
    *
    * @param joinPhis: Phis at the entry of the while stmt and joinStmts
    * @param exp: boolean expression
    * @param stmts: the body
    * @param joinStmts: the the continuation after while loop, if no exception occurs
    * @param exceptPhis: Phis assignments for the except
    */
  case class SSAWhile(
    joinPhis: List[Phi],
    exp: Exp,
    stmts:List[SSABlock],
    joinStmts: List[SSABlock],
    exceptPhis: List[Phi] 
  ) extends SSAStmt


  /**
    * 
    * @param exp boolean expression
    * @param thenStmts then blocks
    * @param elseStmts else blocks
    * @param joinPhis: Phis at the exit of the while stmt
    * @param joinStmts: the the continuation after if-else, if no exception occurs
    * @param exceptPhis: Phis assignments for the except
    */
  case class SSAIf(
    exp:Exp,
    thenStmts:List[SSABlock],
    elseStmts:List[SSABlock],
    joinPhis: List[Phi], 
    joinStmts: List[SSABlock],
    exceptPhis: List[Phi]
  ) extends SSAStmt

  case class SSAAttempt( // attempt E.m(E) as x { \bar{B} }
      methodInv:MethodInvocation, // E.m(E) 
      lhs: Lhs, // as x 
      stmts: List[SSABlock]
  ) extends SSAStmt

  // type Label = ASTPath 

  /**
    * A phi assignment
    *
    * @param srcVar 
    * @param renVar
    * @param rhs
    */
  case class Phi(
    srcVar:Name,
    renVar:Name, 
    // rhs:Map[Label, Name] // changed back to List[(Label, Name)], so that we can find the entry will min label w/o re-sorting
    rhs:List[(Label, Name)]
  )

  ////////////////////////////////////
  // REWRITING FRONTIER
  ////////////////////////////////////


  trait SubstName[A] {
    def appSubst(subst:Map[Name,Name], a:A):A
  }
  object snOps {
    def appSubst[A](subst:Map[Name,Name], a:A)(using sn:SubstName[A]):A = sn.appSubst(subst, a)
  }
  
  given appSubstList[A](using sna:SubstName[A]):SubstName[List[A]] = new SubstName[List[A]] {
    override def appSubst(subst:Map[Name, Name], l:List[A]):List[A] = l.map(sna.appSubst(subst,_))
  }

  given appSubstOption[A](using sna:SubstName[A]):SubstName[Option[A]]  = new SubstName[Option[A]] {
    override def appSubst(subst:Map[Name, Name], o:Option[A]):Option[A] = o.map(sna.appSubst(subst,_))
  }

  given appSusbtMap[K,A](using sna:SubstName[A]):SubstName[Map[K,A]] = new SubstName[Map[K,A]] {
    override def appSubst(subst:Map[Name, Name], m:Map[K, A]):Map[K, A] = m.map( { case (k,v) => (k, sna.appSubst(subst,v)) })
  }


  given appSusbtList[K,A](using sna:SubstName[A]):SubstName[List[(K,A)]] = new SubstName[List[(K,A)]] {
    override def appSubst(subst:Map[Name, Name], m:List[(K, A)]):List[(K, A)] = m.map( { case (k,v) => (k, sna.appSubst(subst,v)) })
  }


  given appSubstSSABlock:SubstName[SSABlock] = new SubstName[SSABlock] { 
    override def appSubst(subst:Map[Name,Name], b:SSABlock): SSABlock = b match {
      case SSABlock(lbl, stmts) => SSABlock(lbl, snOps.appSubst(subst, stmts))
    }
  }

  given appSubstSSAStmt:SubstName[SSAStmt] = new SubstName[SSAStmt] {
    override def appSubst(subst:Map[Name,Name], s:SSAStmt): SSAStmt = s match {
      case SSAVarDecls(mods, ty, varDecls) => s // nothing to substitute since vardecls appear at the start of a method
      case SSAAssert(e, msg) => SSAAssert(snOps.appSubst(subst, e), snOps.appSubst(subst,msg))
      case SSAAssignments(stmts) => SSAAssignments(snOps.appSubst(subst, stmts))
      case SSAExps(es) => SSAExps(snOps.appSubst(subst, es))
      case SSAReturn(o) => SSAReturn(snOps.appSubst(subst, o))
      case SSAThrow(e) => SSAThrow(snOps.appSubst(subst,e))
      /* 
      case SSABreak(lbl) => s
      case SSAContinue(lbl) => s
      */ 
      case SSAEmpty => s 
      case SSATry(tryStmts, catchParam, catchPhis, catchStmts, joinPhis, joinStmts, exceptPhis) => {
        SSATry(snOps.appSubst(subst, tryStmts), 
            catchParam, 
            snOps.appSubst(subst, catchPhis), 
            snOps.appSubst(subst, catchStmts), 
            snOps.appSubst(subst, joinPhis),
            snOps.appSubst(subst, joinStmts), 
            snOps.appSubst(subst, exceptPhis))
      }
      case SSAWhile(joinPhis, exp, stmts, joinStmts, exceptPhis) => {
        SSAWhile(snOps.appSubst(subst, joinPhis), 
          snOps.appSubst(subst,exp),
          snOps.appSubst(subst,stmts),
          snOps.appSubst(subst,joinStmts),
          snOps.appSubst(subst,exceptPhis)
        )
      }
      case SSAIf(exp, thenStmts, elseStmts, joinPhis, joinStmts, exceptPhis) => {
        SSAIf(snOps.appSubst(subst, exp),
          snOps.appSubst(subst, thenStmts), 
          snOps.appSubst(subst, elseStmts), 
          snOps.appSubst(subst, joinPhis),
          snOps.appSubst(subst, joinStmts), 
          snOps.appSubst(subst, exceptPhis))
      }
      case SSAAttempt(methodInv, lhs, stmts) => {
        SSAAttempt(snOps.appSubst(subst,methodInv), 
          snOps.appSubst(subst, lhs),
          snOps.appSubst(subst, stmts))
      }
    }
  }

  given appSubstStmt:SubstName[Stmt] = new SubstName[Stmt] {
    override def appSubst(subst:Map[Name, Name], stmt:Stmt):Stmt = stmt match { 
      // we only need to apply subst to expression statment and assignment statement, 
      // the rest are either desugared away or not stratefied into the SSA AST
      case Assert(exp, msg) => stmt
      case BasicFor(init, loop_cond, post_update, stmt1) => stmt
      case Break(id) => stmt
      case Continue(id) => stmt 
      case Do(stmt1, exp) => stmt 
      case Empty => stmt
      case EnhancedFor(modifiers, ty, id, exp, stmt1) => stmt 
      case ExpStmt(exp) => ExpStmt(snOps.appSubst(subst, exp))
      case IfThen(exp, stmt1) => stmt 
      case IfThenElse(exp, then_stmt, else_stmt) => stmt 
      case Labeled(id, stmt1) => stmt 
      case Return(exp) => stmt 
      case StmtBlock(blk) => stmt 
      case Switch(exp, blocks) => stmt 
      case Synchronized(exp, blk) => stmt 
      case Throw(exp) => stmt 
      case Try(try_blk, catches, finally_blk) => stmt 
      case While(exp, stmt1) => stmt 
    }
  }

  given appSubstPhi:SubstName[Phi] = new SubstName[Phi] {
    override def appSubst(subst:Map[Name,Name], p:Phi):Phi = p match {
      case Phi(srcVar, renVar, rhs) => Phi(srcVar, renVar, snOps.appSubst(subst, rhs))
    }
  }

  given appSubstExp:SubstName[Exp] = new SubstName[Exp] {
    override def appSubst(subst:Map[Name,Name], e:Exp):Exp = e match {
      case ArrayAccess(ArrayIndex(e,es)) => ArrayAccess(ArrayIndex(snOps.appSubst(subst,e), snOps.appSubst(subst,es)))
      case Cast(ty, e) => Cast(ty, snOps.appSubst(subst, e))
      case ArrayCreate(ty, exps, num_dims) => ArrayCreate(ty, snOps.appSubst(subst, exps), num_dims)
      case ArrayCreateInit(ty, size, ArrayInit(v_inits)) => ArrayCreateInit(ty, size, ArrayInit(snOps.appSubst(subst, v_inits)))
      case Assign(lhs, op, rhs) => Assign(snOps.appSubst(subst, lhs), op, snOps.appSubst(subst, rhs))
      case BinOp(e1, op, e2) => BinOp(snOps.appSubst(subst, e1), op, snOps.appSubst(subst,e2))
      case ClassLit(ty) => ClassLit(ty)
      case Cond(cond, true_exp, false_exp) => Cond(snOps.appSubst(subst,cond), snOps.appSubst(subst,true_exp), snOps.appSubst(subst, false_exp))
      case ExpName(name) => ExpName(snOps.appSubst(subst,name))
      case FieldAccess_(access) => FieldAccess_(snOps.appSubst(subst, access))
      case InstanceCreation(type_args, type_decl, args, body) => e // no substitution
      case InstanceOf(e, ref_type) => InstanceOf(snOps.appSubst(subst, e), ref_type)
      case Lambda(params, body) => e // no substitution
      case Lit(lit) => e 
      case MethodInv(methodInv) => MethodInv(snOps.appSubst(subst, methodInv))
      case MethodRef(name, id) => MethodRef(snOps.appSubst(subst, name), id)
      case PostDecrement(exp) => PostDecrement(snOps.appSubst(subst, exp))
      case PostIncrement(exp) => PostIncrement(snOps.appSubst(subst, exp))
      case PreBitCompl(exp) => PreBitCompl(snOps.appSubst(subst, exp))
      case PreDecrement(exp) => PreDecrement(snOps.appSubst(subst, exp))
      case PreIncrement(exp) => PreIncrement(snOps.appSubst(subst, exp))
      case PreMinus(exp) => PreMinus(snOps.appSubst(subst, exp))
      case PreNot(exp) => PreNot(snOps.appSubst(subst,exp))
      case PrePlus(exp) => PrePlus(snOps.appSubst(subst, exp))
      case QualInstanceCreation(exp, type_args, id, args, body) => QualInstanceCreation(snOps.appSubst(subst, exp), type_args, id, args, body)
      case ThisClass(name) => ThisClass(snOps.appSubst(subst, name))
      case This => This    
    }
  }

  given appSubstMethodInv:SubstName[MethodInvocation] = new SubstName[MethodInvocation] {
    override def appSubst(subst:Map[Name,Name], methodInv:MethodInvocation):MethodInvocation = methodInv match {
      case ClassMethodCall(name, ref_types, id, args) => ClassMethodCall(snOps.appSubst(subst, name), ref_types, id, snOps.appSubst(subst, args))
      case MethodCall(name, args) => MethodCall(snOps.appSubst(subst, name), snOps.appSubst(subst, args))
      case PrimaryMethodCall(e, ref_types, id, args) => PrimaryMethodCall(snOps.appSubst(subst, e), ref_types, id, snOps.appSubst(subst, args))
      case SuperMethodCall(ref_types, id, args) => SuperMethodCall(ref_types, id, snOps.appSubst(subst, args))
      case TypeMethodCall(name, ref_types, id, args) => TypeMethodCall(snOps.appSubst(subst, name), ref_types, id, snOps.appSubst(subst, args))
    }
  }

  given appSubstVarInit:SubstName[VarInit] = new SubstName[VarInit] { 
    override def appSubst(subst:Map[Name, Name], v_init:VarInit):VarInit = v_init match {
      case InitExp(e) => InitExp(snOps.appSubst(subst, e))
      case InitArray(array_init) => InitArray(snOps.appSubst(subst, array_init)) 
    }
  }

  given appSubstArrayInit:SubstName[ArrayInit] = new SubstName[ArrayInit] {
    override def appSubst(subst:Map[Name, Name], array_init:ArrayInit):ArrayInit = array_init match {
      case ArrayInit(var_inits) => ArrayInit(snOps.appSubst(subst, var_inits))
    }
  }

  given appSubstLhs:SubstName[Lhs] = new SubstName[Lhs] {
    override def appSubst(subst:Map[Name, Name], lhs:Lhs):Lhs = lhs match {
      case NameLhs(n) => NameLhs(snOps.appSubst(subst, n))
      case FieldLhs(field_access) => FieldLhs(snOps.appSubst(subst, field_access))
      case ArrayLhs(array_idx) => ArrayLhs(snOps.appSubst(subst, array_idx))
    }
  }

  given appSubstFieldAccess:SubstName[FieldAccess] = new SubstName[FieldAccess] {
    override def appSubst(subst:Map[Name, Name], fieldAccess:FieldAccess):FieldAccess = fieldAccess match {
      case ClassFieldAccess(name, id) => ClassFieldAccess(snOps.appSubst(subst, name), id)
      case PrimaryFieldAccess(e, id) => PrimaryFieldAccess(snOps.appSubst(subst, e), id)
      case SuperFieldAccess(id) => SuperFieldAccess(id)
    }
  }

  given appSubstArrayIdx:SubstName[ArrayIndex] = new SubstName[ArrayIndex] {
    override def appSubst(subst: Map[Name,Name], a: ArrayIndex): ArrayIndex = a match {
      case ArrayIndex(e,es) => ArrayIndex(snOps.appSubst(subst, e), snOps.appSubst(subst, es))
    }
  }


  given appSubstName:SubstName[Name] = new SubstName[Name] {
    override def appSubst(subst:Map[Name,Name], n:Name):Name = subst.get(n) match {
      case None => n
      case Some(rn) => rn
    }
  }

  /**
    * Source language Context
    * ctx ::= Box | ctx; | ctx; \overline{s} | s; ctx | if e {ctx} else {\overline{s}} | 
    *    if e {\overline{s}} else {ctx}  |  while e {ctx} | try {ctx} catch (T x) (\overline{s}) | 
    *     try {\overline{s}} catch (T x) {ctx}  
    */
  sealed trait SCtx 
  
  case object SBox extends SCtx

  case class SLast(ctx: SCtx) extends SCtx 

  case class SHead(ctx: SCtx) extends SCtx

  case class STail(ctx: SCtx) extends SCtx
  
  case class SThen(ctx: SCtx) extends SCtx

  case class SElse(ctx: SCtx) extends SCtx

  case class SWhile(ctx: SCtx) extends SCtx
  
  case class STry(ctx: SCtx) extends SCtx
  
  case class SCatch(ctx: SCtx) extends SCtx

  // substitute the SBox from the outter context by the inner. 
  // putSCtx(SLast(SBox), SHead(SBox)) --> SLast(SHead(SBox))
  def putSCtx(outter:SCtx, inner:SCtx): SCtx = outter match {
    case SBox => inner 
    case SLast(o) => SLast(putSCtx(o, inner))
    case SHead(o) => SHead(putSCtx(o, inner))
    case STail(o) => STail(putSCtx(o, inner))
    case SThen(o) => SThen(putSCtx(o, inner))
    case SElse(o) => SElse(putSCtx(o, inner))
    case SWhile(o) => SWhile(putSCtx(o, inner))
    case STry(o) => STry(putSCtx(o, inner))
    case SCatch(o) => SCatch(putSCtx(o, inner))
  }

  /**
    * Target language context (SSA)

    * CTX ::= Box | nop; | CTX; \overline{B} | B; CTX | 
    *     if E {CTX}          else {\overline{B}} join {\overline{\phi}} next {\overline{B}} except {\overline{\phi}} | 
    *     if E {\overline{B}} else {CTX}          join {\overline{\phi}} next {\overline{B}} except {\overline{\phi}}  | 
    *     if E {\overline{B}} else {\overline{B}} join {BBox}            next {\overline{B}} except {\overline{\phi}}  | 
    *     if E {\overline{B}} else {\overline{B}} join {\overline{\phi}} next {CTX} except {\overline{\phi}}  | 
    *     if E {\overline{B}} else {\overline{B}} join {\overline{\phi}} next {\overline{B}} except {BBox}  | 
    *     join {BBox}           while E {\overline{B}} next {\overline{B}} except {\overline{phi}} |  
    *     join {\overline{phi}} while E {CTX}          next {\overline{B}} except {\overline{phi}} |  
    *     join {\overline{phi}} while E {\overline{B}} next {CTX}          except {\overline{phi}} | 
    *     join {\overline{phi}} while E {\overline{B}} next {\overline{B}} except {BBox} | 
    *     try {CTX}          catch (T x) {\overline{\phi}} handle {\overline{B}} join {\overline{\phi}} next {\overline{B}} except {\overline{\phi}} |
    *     try {\overline{B}} catch (T x) {BBox}            handle {\overline{B}} join {\overline{\phi}} next {\overline{B}} except {\overline{\phi}} |
    *     try {\overline{B}} catch (T x) {\overline{\phi}} handle {CTX}          join {\overline{\phi}} next {\overline{B}} except {\overline{\phi}} |
    *     try {\overline{B}} catch (T x) {\overline{\phi}} handle {\overline{B}} join {BBox}            next {\overline{B}} except {\overline{\phi}} |
    *     try {\overline{B}} catch (T x) {\overline{\phi}} handle {\overline{B}} join {\overline{\phi}} next {CTX}          except {\overline{\phi}} |
    *     try {\overline{B}} catch (T x) {\overline{\phi}} handle {\overline{B}} join {\overline{\phi}} next {\overline{B}} except {BBox} |
    *     attempt BBox next {\overline{B}} | 
    *     attempt E.m(E) as x next {CTX}  | 
    *     throw 
    */
  sealed trait TCtx 

  case object TBox extends TCtx               // 0

  // we don't need TLast, the last statement must be either 
  //   a nop, return, throw, if-else-join, try-catch-join, while-join
  // case class TLast(ctx:TCtx) extends TCtx   // 1

  case object TNop extends TCtx // 1 

  case class THead(ctx:TCtx) extends TCtx     // 2

  case class TTail(ctx:TCtx) extends TCtx     // 3

  case class TThen(ctx:TCtx) extends TCtx     // 4

  case class TElse(ctx:TCtx) extends TCtx     // 5

  case object TIfJoin extends TCtx            // 6

  case class TIfNext(ctx:TCtx) extends TCtx   // 7  

  case object TIfExcept extends TCtx        // 8


  case class TWhileJoin(b:Int) extends TCtx // 9 

  case class TWhile(ctx:TCtx) extends TCtx     // a

  case class TWhileNext(ctx:TCtx) extends TCtx // b 

  case object TWhileExcept extends TCtx        // c 


  case class TTry(ctx:TCtx) extends TCtx       // d

  case object TCatch extends TCtx              // e

  case class TCatchHandle(ctx:TCtx) extends TCtx     // f

  case object TTryJoin extends TCtx         // g

  case class TTryNext(ctx:TCtx) extends TCtx   // h

  case object TTryExcept extends TCtx       // i

  case object TAttempt extends TCtx         // j 

  case class TAttemptNext(ctx:TCtx) extends TCtx // k 

  case object TThrow extends TCtx // l 

  
  val chararray:List[Char] = "0123456789abcdefghi".toList


  // homework 1)
  // character coding for the Target Ctx // this mapping can be randomized 
  def charcode(ctx:TCtx, arr:List[Char]):List[Char] = ctx match {
    case TBox => List(arr(0))
    case TNop => List(arr(1))
    case THead(ctx2) => List(arr(2)) ++ charcode(ctx2, arr)
    case TTail(ctx2) => List(arr(3)) ++ charcode(ctx2, arr)
    case TThen(ctx2) => List(arr(4)) ++ charcode(ctx2, arr)
    case TElse(ctx2) => List(arr(5)) ++ charcode(ctx2, arr)
    case TIfJoin  => List(arr(6))
    case TIfNext(ctx2) => List(arr(7)) ++ charcode(ctx2, arr)
    case TIfExcept => List(arr(8))
    case TWhileJoin(_) => List(arr(9))
    case TWhile(ctx2) => List(arr(10)) ++ charcode(ctx2, arr)
    case TWhileNext(ctx2) => List(arr(11)) ++ charcode(ctx2, arr)
    case TWhileExcept => List(arr(12))
    case TTry(ctx2) => List(arr(13)) ++ charcode(ctx2, arr)
    case TCatch => List(arr(14))
    case TCatchHandle(ctx2) => List(arr(15)) ++ charcode(ctx2, arr)
    case TTryJoin => List(arr(16))
    case TTryNext(ctx2) => List(arr(17)) ++ charcode(ctx2, arr)
    case TTryExcept => List(arr(18))
    case TAttempt => List(arr(19))
    case TAttemptNext(ctx2) => List(arr(20)) ++ charcode(ctx2, arr)
    case TThrow => List(arr(21))
  }


  // homework 2)
  def putTCtx(outer:TCtx, inner:TCtx): TCtx = outer match {
    case TBox => inner 
    case THead(o) => THead(putTCtx(o, inner))
    case TTail(o) => TTail(putTCtx(o, inner))
    case TThen(o) => TThen(putTCtx(o, inner))
    case TElse(o) => TElse(putTCtx(o, inner))
    case TIfNext(o) => TIfNext(putTCtx(o, inner))
    case TWhile(o) => TWhile(putTCtx(o, inner))
    case TWhileNext(o) => TWhileNext(putTCtx(o, inner))
    case TTry(o) => TTry(putTCtx(o, inner))
    case TCatchHandle(o) => TCatchHandle(putTCtx(o, inner))
    case TTryNext(o) => TTryNext(putTCtx(o, inner))
    case TAttemptNext(o) => TAttemptNext(putTCtx(o, inner))
    case _ => outer
  }


  // variable mapping 

  // beta
  // type VarMap = Map[Name, Map[TCtx, (SCtx, Name)]]
  type VarMap = Map[Name, Map[TCtx, Name]] // updated, we dont need the SCtx, which is only required for correctness proof.
  // SCTx can be derived from TCtx, TCtx |-> SCtx mapping is a functional mapping, but SCtx |-> TCtx might not be.
 
  
  def unionVarMap(vm1:VarMap, vm2:VarMap):VarMap = vm2.toList.foldLeft(vm1)( (vm, kv) => kv match {
    case (name, m) => vm.get(name) match {
      case None => vm + (name -> m)
      case Some(m2) => vm + (name -> (m ++ m2))
    }
  })

  def diffVarMap(vm1:VarMap, vm2:VarMap):VarMap = listToVarMap(varMapToList(vm1).toSet.diff(varMapToList(vm2).toSet).toList)

  def listToVarMap(l:List[(Name, (TCtx, Name))]):VarMap = l.foldLeft(Map():VarMap)( (vm, kv) => kv match {
    case (name, (tctx, tname)) => vm.get(name) match {
      case None => vm + (name -> Map(tctx -> tname))
      case Some(m2) => m2.get(tctx) match {
        case None => vm + (name ->  (m2 + (tctx -> (tname))))
        case Some(name1) => vm // duplicate?
      }
    }
  })

  def varMapToList(vm:VarMap):List[(Name, (TCtx, Name))] = vm.toList.flatMap( { case (n, m) => {
    m.toList.map( { case (tctx, n2) => (n, (tctx, n2)) }) 
  }} )


  /**
  * A state object for the conversion function
  *
  * @param varMap - the variable mapping, i.e. the beta
  * @param okCtx - the exit context from the last block, i.e. the "ok" context
  * @param aenv - this the list of all "ok" context we encountered so far, (it is used in computing the Last(.))
  * @param eenv - the list of contexts that throw exception, i.e. the set of "err" contexts
  * @param nestedDecls - the list of nested declared variables, we need to move their declarations to the outter most level of the method body.
  * @param methodInvs - the list of method invocations
  * @param srcLabelEnv - the list of labels existing in the src code and their contexts, TODO: double check, we probably don't need this until we know how to handle continue and break
  * @param config - configuration
  */
  case class State(
    varMap: VarMap, 
    okCtx: TCtx,
    aenv: AEnv, 
    eenv: EEnv, 
    nestedDecls: List[(TCtx, Ident, Type, List[Modifier])],
    methodInvs: List[(TCtx, MethodInvocation)],
    srcLabelEnv: Map[Ident, SCtx],
    config: SSAEnvConfig
  )

  /** 
   * extract the charcodes from the environment
   * */
  def getCharCodes(st:State):List[Char] = st match {
    case State(varMap, okCtx, aenv, eenv, nestedDecls, methodInvs, srcLabelEnv, config) => 
      config match {
        case SSAEnvConfig(ctxtAsID, charcodes) => charcodes
      }
  }


  /**
  * A configurartion for the conversion function state
  * @param ctxtAsID - when this flag is set to True, whenever mkName() is called,  tctxt.toString() is used to generate new ID
  *                   when this flag is set to False, we use the mapping between tctxt and a charcodes.
  * @param charcodes - the character code array
  * */
  case class SSAEnvConfig(ctxtAsID:Boolean, charcodes:List[Char]) 

  val debugConfig = SSAEnvConfig(true, List())
  val defaultConfig = SSAEnvConfig(false, chararray) // TODO: randomly shuffle chararray

  // when using the default config, ctxt as part of the variable name will be mapped to character array through charcode. 
  // the labels/contexts in the phi statement remain unchanged.
  val initState:State = State(Map(), TBox, List(), List(), List(), List(), Map(), defaultConfig)

  val debugInitState:State = State(Map(), TBox, List(), List(), List(), List(), Map(), debugConfig)





  def eenvFromState(st:State):EEnv = st match {
    case State(_, _, _, eenv, _, _, _, _ ) => eenv
  }

  def okCtxFromState(st:State):TCtx = st match {
    case State(_, okCtx, _, _, _, _, _,_) => okCtx
  }

  def srcLabelEnvFromState(st:State):Map[Ident,SCtx] = st match {
    case State(_, _, _, _, _, _, srcLblEnv,_) => srcLblEnv
  }

  type AEnv = List[TCtx] // TODO: give a better type name to this environment
  type EEnv = List[TCtx] // TODO: give a better type name to this environment

  type ErrorM = String

  enum SSAResult[+A] {
    case SSAError(msg:ErrorM) extends SSAResult[Nothing]
    
    case SSAOk[A](result:A) extends SSAResult[A]
  }

  import SSAResult.*

  given ssaResultFunctor: Functor[SSAResult] =
    new Functor[SSAResult] {
      override def map[A, B](fa: SSAResult[A])(f: A => B): SSAResult[B] =
        fa match {
          case SSAError(s) => SSAError(s)
          case SSAOk(a) => SSAOk(f(a))
        }
    }

  given ssaResultApplicative: ApplicativeError[SSAResult, ErrorM] = 
    new ApplicativeError[SSAResult, ErrorM] {
      override def ap[A, B](ff: SSAResult[A => B])(fa: SSAResult[A]): SSAResult[B] =
        ff match {
          case SSAOk(f) =>
            fa match {
              case SSAOk(a) => SSAOk(f(a))
              case SSAError(s) => SSAError(s)
            }
          case SSAError(s) => SSAError(s)
        }

      override def pure[A](a: A): SSAResult[A] = SSAOk(a)

      override def raiseError[A](e: ErrorM): SSAResult[A] = SSAError(e)

      override def handleErrorWith[A](fa: SSAResult[A])(f: ErrorM => SSAResult[A]): SSAResult[A] =
        fa match {
          case SSAError(s) => f(s)
          case SSAOk(a) => SSAOk(a)
        }
    }

  given ssaResultMonadError(using app:ApplicativeError[SSAResult, ErrorM]):MonadError[SSAResult, ErrorM] = {
    new MonadError[SSAResult, ErrorM] {
      override def raiseError[A](e: ErrorM): SSAResult[A] = app.raiseError(e)

      override def handleErrorWith[A](fa: SSAResult[A])(f: ErrorM => SSAResult[A]): SSAResult[A] = app.handleErrorWith(fa)(f)

      override def flatMap[A, B](fa: SSAResult[A])(f: A => SSAResult[B]): SSAResult[B] =
        fa match {
          case SSAOk(a) => f(a)
          case SSAError(s) => SSAError(s)
        }

      override def pure[A](a: A): SSAResult[A] = app.pure(a)

      @annotation.tailrec
      def tailRecM[A, B](init: A)(fn: A => SSAResult[Either[A, B]]): SSAResult[B] =
        fn(init) match {
          case SSAError(msg) => SSAError(msg)
          case SSAOk(Right(b)) => SSAOk(b)
          case SSAOk(Left(a)) => tailRecM(a)(fn)
        }
    }
  }

  type SState[S,A] = StateT[SSAResult, S, A]
  type SSAState[A] = SState[State, A]


  def get:SState[State, State] = StateT { state => SSAOk((state, state))} 

  def put(st:State):SState[State, Unit] = StateT { _ => SSAOk((st,()))} 

  /**
    * setOkCtx - setting the Ok exiting context in the state
    *
    * @param tctx
    * @param m
    * @return
    */
  def setOkCtx(tctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(vm, _, aenv, eenv, nestedDecls, methInvs, srcLabelEnv, conf) => State(vm, tctx, aenv, eenv, nestedDecls, methInvs, srcLabelEnv,conf)
    })
    _   <- put(st1)
  } yield ()


  /**
    * setVarMap - set the given VarMap in the state
    *
    * @param vm
    * @param m
    * @return
    */
  def setVarMap(vm:VarMap)(using m:MonadError[SSAState, ErrorM]): SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(_, okCtx, aenv, eenv, nestedDecls, methInvs, srcLabelEnv,conf) => State(vm, okCtx, aenv, eenv, nestedDecls, methInvs,srcLabelEnv,conf)
    })
    _   <- put(st1)
  } yield ()
  
  def removeVarFromVarMap(v:Name)(using m:MonadError[SSAState, ErrorM]): SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(vm ,okCtx, aenv, eenv, nestedDecls, methInvs, srcLabelEnv,conf) => State(vm - v, okCtx, aenv, eenv, nestedDecls, methInvs,srcLabelEnv,conf)
    })
    _ <- put(st1)
  } yield ()

  /**
    * addNestedVarDecls - add an entry to the nested var decls in the state
    *
    * @param tctx
    * @param id
    * @param ty
    * @param mods
    * @param m
    * @return
    */
  def addNestedVarDecls(tctx:TCtx, id:Ident, ty:Type, mods:List[Modifier])(using m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1  <- m.pure(st match {
      case State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => {
        val nDecls1 = (nDecls.toSet + ((tctx, id, ty, mods))).toList
        State(varMap, okCtx, aenv, eenv, nDecls1, methInvs,srcLblEnv,conf)
      }
    })
    _  <- put(st1)
  } yield ()

  /**
    * addMethodInv - add an entry of the method invocatoin in the state
    *
    * @param tctx
    * @param methinv
    * @param m
    * @return
    */
  def addMethodInv(tctx:TCtx, methinv:MethodInvocation)(using m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => {
        val methInvs1 = (methInvs.toSet + ((tctx, methinv))).toList
        State(varMap, okCtx, aenv, eenv, nDecls, methInvs1, srcLblEnv,conf)
      }
    })
    _  <- put(st1)
  } yield ()

  def addSrcLabel(label:Ident, ctx:SCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get
    st1 <- m.pure(st match {
      case State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => {
        val srcLblEnv1 = srcLblEnv + (label -> ctx) 
        State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv1,conf)
      }
    })
  } yield ()


  /**
    * addToAEnv - add an context to the list of all program context env
    *
    * @param tctx 
    * @param m
    * @return
    */
  def addToAEnv(tctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => {
        val aenv1 = (aenv.toSet + tctx).toList
        State(varMap, okCtx, aenv1, eenv, nDecls, methInvs, srcLblEnv,conf)
      }
    })
    _  <- put(st1)
  } yield ()


  /**
    * addToEEnv - add the given context to the list of throwing context in the state
    *
    * @param tctx
    * @param m
    * @return
  */
  

  def addToEEnv(tctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, Unit] = for {
    st <- get 
    st1  <- m.pure(st match {
      case State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => {
        val eenv1 = (eenv.toSet + tctx).toList
        State(varMap, okCtx, aenv, eenv1, nDecls, methInvs, srcLblEnv,conf)
      }
    })
    _  <- put(st1)
  } yield ()

  /**
    * check whetehr the current configuration is using config as ID
    *
    * @param m
    * @return
    */
  def usingCtxtAsID(using m:MonadError[SSAState, ErrorM]):SState[State, Boolean] = for {
    st <- get
  } yield st match {
    case State(varMap, okCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,SSAEnvConfig(ctxtAsID, _)) => ctxtAsID
  }

  /**
    * retreive the character array for generating "legal" id from context
    *
    * @param m
    * @return
    */
  def getCharArray(using m:MonadError[SSAState, ErrorM]):SState[State, List[Char]] = for {
    st <- get
  } yield st match {
    case State(varMap, eCtx, aenv, eenv,  nDecls, methInvs, srcLblEnv,SSAEnvConfig(ctxtAsID, arr)) => arr
  }

  /**
   * mergeState(st1, st2, st3)
   * */

  def mergeState(st1:State, st2:State, st3:State):State = {
    val st12 = mergeState(st1, st2)
    mergeState(st12, st3)  
  }
  

  /** 
   * mergeState - merge two states by taking the vm and ectx from st1,
   * and union the eenv and nDecls
   * */
  def mergeState(st1:State, st2:State):State = (st1, st2) match {
    case (State(vm1, okCtx1, aenv1, eenv1, nDecls1, methInvs1, srcLblEnv1,conf1), State(vm2, okCtx2, aenv2, eenv2,  nDecls2, methInvs2, srcLblEnv2,conf2)) => 
      State(unionVarMap(vm1, vm2), okCtx1, (aenv1++aenv2).toSet.toList, (eenv1++eenv2).toSet.toList, (nDecls1 ++ nDecls2).toSet.toList, (methInvs1 ++ methInvs2).toSet.toList, (srcLblEnv1 ++ srcLblEnv2),conf1)
  }

  
  def extendVarsWithContextAndLabel(vars: List[Name], tctx:TCtx, lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1 <- st match {
      case State(vm0, okCtx, aenv, eenv,  nDecls, methInvs, srcLblEnv,conf) => for {
        entries <- vars.traverse(v => for {
          v_lbl <- mkName(v, lbl)
        } yield (v, tctx, v_lbl ))
      } yield State(entries.foldLeft(vm0)((vm, ent) => ent match {
        case (v, tctx, v_lbl) => vm.get(v) match {
          case None => vm
          case Some(m) => vm + (v -> (m + (tctx -> v_lbl)))
        }
      }), okCtx, aenv, eenv,  nDecls, methInvs, srcLblEnv,conf)
    }
    _ <- put(st1)
  } yield ()

  // do we still need this? 
  // very likely we should remove this!!! it is not in used.
  def extendAllVarsWithContextAndLabel(tctx:TCtx, lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State,Unit] = for {
    st <- get
    st1 <- st match {
      case State(vm0, eCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => for {
        entries <- vm0.keySet.toList.traverse(v => for {
          v_lbl <- mkName(v, lbl)
        } yield (v, tctx, v_lbl ))
      } yield State(entries.foldLeft(vm0)((vm, ent) => ent match {
        case (v, tctx, v_lbl) => vm.get(v) match {
          case None => vm
          case Some(m) => vm + (v -> (m + (tctx -> v_lbl)))
        }
      }), eCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf)
    }
    _ <- put(st1)
  } yield ()



  /**
  * mkName - create a new name from an existing name and a label.
  *
  * @param n
  * @param lbl
  * @param m
  * @return
  */
  def mkName(n:Name, lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State, Name] = { 
    n match {
      case Name(Nil) => m.raiseError("SSA construction failed, mkName is applied to an empty name.")
      case Name(ids) => for {
        pre <- m.pure(ids.init)
        x   <- m.pure(ids.last)
        s   <- lblToStr(lbl)
        y   <- m.pure(appIdStr(x, s))
      } yield Name(pre++List(y))
    }
  }
  
  /**
    * convert a label (TCtxt) to a string
    *
    * @param lbl
    * @param m
    * @return
    */
  def lblToStr(lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State, String] = for {
    flag <- usingCtxtAsID
    res <- 
      if (flag) { m.pure(lbl.toString()) }
      else { for {
        charCodes <- getCharArray
      } yield (charcode(lbl, charCodes).mkString)
    }
  } yield res

  given eqTCtx:Eq[TCtx] = new Eq[TCtx]{
    override def eqv(x: TCtx, y: TCtx): Boolean = (x,y) match {
      case (TBox, TBox) => true
      // case (TLast(ctx1), TLast(ctx2)) => eqv(ctx1, ctx2)
      case (TNop, TNop) => true
      case (THead(ctx1), THead(ctx2)) => eqv(ctx1, ctx2)
      case (TTail(ctx1), TTail(ctx2)) => eqv(ctx1, ctx2)

      case (TThen(ctx1), TThen(ctx2))     => eqv(ctx1, ctx2)
      case (TElse(ctx1), TElse(ctx2))     => eqv(ctx1, ctx2)
      case (TIfJoin, TIfJoin)             => true
      case (TIfNext(ctx1), TIfNext(ctx2)) => eqv(ctx1, ctx2)
      case (TIfExcept, TIfExcept)         => true

      case (TWhileJoin(b1), TWhileJoin(b2))     => b1 == b2
      case (TWhile(ctx1), TWhile(ctx2))         => eqv(ctx1, ctx2)
      case (TWhileNext(ctx1), TWhileNext(ctx2)) => eqv(ctx1, ctx2)
      case (TWhileExcept, TWhileExcept)         => true 

      case (TTry(ctx1), TTry(ctx2))                 => eqv(ctx1, ctx2)
      case (TCatch, TCatch)                         => true
      case (TCatchHandle(ctx1), TCatchHandle(ctx2)) => eqv(ctx1, ctx2)
      case (TTryJoin, TTryJoin)                     => true
      case (TTryNext(ctx1), TTryNext(ctx2))         => eqv(ctx1, ctx2)
      case (TTryExcept, TTryExcept)                 => true

      case (TAttempt, TAttempt)                     => true
      case (TAttemptNext(ctx1), TAttemptNext(ctx2)) => eqv(ctx1, ctx2)

      case (TThrow, TThrow)  => true

      case (_,_) => false 
    }
  }


  // ****************************** implementing isLast start *********************************************************************
  // check whether a context is the last of a sequence, w.r.t. to the list all program contexts in the same immediate lexical scope

  // TODO: we need to get rid of SLast and TLast, TLast is generated by conversion from SLast, 
  // SLast was created by kblkStmts as the last statement context, we should replace it by specific
  //   nop, if-else, while or try catch
  

  def isLast(tctx:TCtx):Boolean = tctx match {
    case TNop => true
    case TTail(tctx1)   => isLast(tctx1)
    case TIfNext(tctx1) => isLast(tctx1)
    case TTryNext(tctx1) => isLast(tctx1)
    case TAttemptNext(tctx1) => isLast(tctx1)
    case TWhileNext(tctx1) => isLast(tctx1)
    case _  => false
  }

  def isErr(tctx:TCtx):Boolean = tctx match { 
    case TThrow   => true
    case TAttempt => true 
    case TIfExcept    => true
    case TWhileExcept => true
    case TTryExcept   => true

    case THead(tctx1) => isErr(tctx1)
    case TTail(tctx1) => isErr(tctx1)
    // is this check needed?
    case TThen(tctx1) => isErr(tctx1)
    case TElse(tctx1) => isErr(tctx1)

    case TIfNext(tctx1) => isErr(tctx1)

    // is this check needed?
    case TWhile(tctx1) => isErr(tctx1)

    case TWhileNext(tctx1) => isErr(tctx1)

    // is this check needed?
    case TTry(tctx1) => isErr(tctx1)
    case TCatchHandle(tctx1) => isErr(tctx1)

    case TTryNext(tctx1) => isErr(tctx1)
    case TAttemptNext(tctx1) => isErr(tctx1)
    case _ => false
  }

  /*
  def isLast(tctx:TCtx, aenv:AEnv):Boolean = follow(tctx,aenv) match {
    case None => true 
    case Some(_) => false
  }


  def ifElseEnv(aenv:AEnv):Boolean = aenv match {
    case (TThen(_)) :: tl => true 
    case (TElse(_)) :: tl => true
    case (TIfJoinPhi :: tl ) => true
    case _ => false
  }

  def whileEnv(aenv:AEnv):Boolean = aenv match {
    case (TWhile(_)) :: tl => true
    case TWhileJoinPhi(_) :: tl => true
    case _ => false 
  }

  def tryEnv(aenv:AEnv):Boolean = aenv match {
    case TTry(_) :: tl => true
    case TTryPeriPhi :: tl => true
    case TTryPostPhi :: tl => true
    case TCatch(_) :: tl => true 
    case _ => false
  }

  def seqEnv(aenv:AEnv):Boolean = aenv match {
    case THead(_) :: tl => true
    case TTail(_) :: tl => true 
    case _ => false
  }

  def lastEnv(aenv:AEnv):Boolean = aenv match {
    case TLast(_) :: tl => true
    case _ => false
  }

  */

  // isLast(c) == true iff follow(c) == None
  /** follow - get the following program context 
   *
   * follow is called when tctx is not in eenv 
   */
  
  // homework 3
  // let's ponder about what is the follow(tctx) computing? 
  /** follow returns a list of contexts in the current lexical scope and might be reachable from tctx via an "ok" flow

   * @param - tctx, the input context
   * @param - aenv, the list of known contexts in the same lexical scope as tctx
   * 
   * @return - a list of contexts
   *    
   * */


  def follow(tctx:TCtx, aenv:AEnv):List[TCtx] = tctx match {
    /*
     since allCtxs are in the same scope as Box
     and Box <_ok Ctx for any Ctx
     -------------------------------(FollowBox)
     allCtxs |- follow(Box) -> allCtxs
    */
    case TBox => aenv

    case TNop => Nil 
    /*
     allCtxs |- follow(B;Box) -> Ctxs
     --------------------------------------------(FollowHead)
     allCtxs |- follow(Ctx;\bar{B}) -> {B;Box} \cup Ctxs
     */
    case THead(c) => TTail(TBox)::follow(TTail(TBox), aenv) // building the closure
    /*
     allTailCtxs = [ Ctx | (B;Ctx) <- allCtxs ]
     allTailCtxs |- follow(Ctx) -> Ctxs'
     Ctxs'' = [ (B;Ctx') | Ctx' \in Ctxs' ]
     --------------------------------------------(FollowTail)
     allCtxs |- follow(B;Ctx) -> Ctxs''
     */
    case TTail(c) => {
      val daenv = appDec(unTTail, aenv)
      follow(c, daenv).map(TTail(_)) 
    }
    /*
     allThenCtxs =  [ Ctx | if E {Ctx} else {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} <- allCtxs ]
     allThenCtxs |- follow(Ctx) -> Ctxs'
     \exists Ctx' \in Ctxs': Last(Ctx')
     Ctxs'' = [  if E {Ctx'} else {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} | Ctx' \in Ctxs' ]
     allCtxs |- follow(if E {\bar{B}} else {\bbar{B}} join \bar{phi} next {Box} except \bar{phi}) -> Ctxs'''
     -------------------------------------------------------------------------------------------------------------(FollowThen1)
     allCtxs |- follow(if E {Ctx} else {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi}) ->
                Ctxs'' \cup { if E {\bar{B}} else {\bbar{B}} join \bar{phi} next {Box} except \bar{phi}} \cup Ctxs'''
     */

     /*
     allThenCtxs =  [ Ctx | if E {Ctx} else {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} <- allCtxs ]
     allThenCtxs |- follow(Ctx) -> Ctxs'
     \forall Ctx' \in Ctxs': not(Last(Ctx'))
     Ctxs'' = [  if E {Ctx'} else {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} | Ctx' \in Ctxs' ]
     -------------------------------------------------------------------------------------------------------------(FollowThen2)
     allCtxs |- follow(if E {Ctx} else {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi}) -> Ctxs'' 
     */

    case TThen(c) => {
      val daenv = appDec(unTThen, aenv)
      val ctxsp = follow(c, daenv)
      if (ctxsp.exists(isLast(_))) {
        ctxsp.map(TThen(_)) ++ List(TIfNext(TBox)) ++ follow(TIfNext(TBox), aenv)
      } else {
        ctxsp.map(TThen(_))
      }
    }
    // deduction rules omitted, similar to the Then cases.
    case TElse(c) => {
      val daenv = appDec(unTElse, aenv)
      val ctxsp = follow(c, daenv)
      if (ctxsp.exists(isLast(_))) {
        ctxsp.map(TElse(_)) ++ List(TIfNext(TBox)) ++ follow(TIfNext(TBox), aenv)
      } else {
        ctxsp.map(TElse(_))
      }
    }
     /*
     allCtxs |- follow((if E {\bar{B}} else {\bar{B}} join \bar{phi} next {Box} except \bar{phi}) -> Ctxs
     -------------------------------------------------------------------------------------------------------------(FollowIfJoin)
     allCtxs |- follow(if E {\bar{B}} else {\bar{B}} join BBox next {\bar{B}} except \bar{phi}) ->
                 {if E {\bar{B}} else {\bar{B}} join \bar{phi} next {Box} except \bar{phi} } \cup Ctxs 
    */   
    case TIfJoin => TIfNext(TBox)::follow(TIfNext(TBox), aenv) // building the closure

    /*
     allNextCtxs = [ Ctx | (if E {\bar{B}} else {\bar{B}} join \bar{phi} next {Ctx} except \bar{phi}) <- allCtxs ]
     allNextCtxs |- follow(Ctx) -> Ctxs'
     Ctxs'' = [ (if E {\bar{B}} else {\bar{B}} join \bar{phi} next {Ctx'} except \bar{phi})  | Ctx' \in Ctxs' ]
     ------------------------------------------------------------------------------------------------------------(FollowIfNext)
     allCtxs |- follow(if E {\bar{B}} else {\bar{B}} join \bar{phi} next {Ctx} except \bar{phi}) -> Ctxs''
     */

    case TIfNext(c) => {
      val daenv = appDec(unTIfNext, aenv)
      follow(c, daenv).map(TIfNext(_))
    }

    case TIfExcept => Nil



    /*
     allTryCtxs =  [ Ctx | try {Ctx} catch (ex) \bar{phi}  handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} <- allCtxs ]
     allTryCtxs |- follow(Ctx) -> Ctxs'
     \exists Ctx' \in Ctxs': Last(Ctx')
     Ctxs'' = [ try {Ctx'} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} | Ctx' \in Ctxs' ]
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Box} except \bar{phi} ) -> Ctxs'''
     -------------------------------------------------------------------------------------------------------------(FollowTry1)
     allCtxs |- follow(try {Ctx} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi}) ->
                Ctxs'' \cup {try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Box} except \bar{phi}  } \cup Ctxs'''
     */

    /*
     allTryCtxs =  [ Ctx | try {Ctx} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} <- allCtxs ]
     allTryCtxs |- follow(Ctx) -> Ctxs'
     \forall Ctx' \in Ctxs': not(Last(Ctx'))
     Ctxs'' = [  try {Ctx'} catch (ex) handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} | Ctx' \in Ctxs' ]
     -------------------------------------------------------------------------------------------------------------(FollowTry2)
     allCtxs |- follow(try {Ctx} catch (ex) \bar{phi}  handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi})  -> Ctxs'' 
     */
      
    case TTry(c) => {
      val daenv = appDec(unTTry, aenv)
      val ctxsp = follow(c, daenv)
      if (ctxsp.exists(isLast(_))) {
        ctxsp.map(TTry(_)) ++ List(TTryNext(TBox)) ++ follow(TTryNext(TBox), aenv)
      } else {
        ctxsp.map(TTry(_))
      }
    }

    /*
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {Box} join \bar{phi} next {\bar{B}} except \bar{phi}) -> Ctxs'
     ----------------------------------------------------------------------------------------------------------------(FollowCatch)
     allCtxs |- follow(try {\bar{B}} catch (ex) BBox handle {\bar{B}} join \bar{phi} next {\bar{B}} except \bar{phi} ) ->
      {try {\bar{B}} catch (ex) \bar{phi} handle {Box} join \bar{phi} next {\bar{B}} except \bar{phi} } \cup Ctxs'
     */
    case TCatch => TCatchHandle(TBox)::follow(TCatchHandle(TBox), aenv) // building the closure



    /*
     allCatchHandleCtxs =  [ Ctx | try {\bar{B}} catch (ex) \bar{phi}  handle {Ctx} join \bar{phi} next {\bar{B}} except \bar{phi} <- allCtxs ]
     allCatchHandleCtxs |- follow(Ctx) -> Ctxs'
     \exists Ctx' \in Ctxs': Last(Ctx')
     Ctxs'' = [ try {\bar{B}} catch (ex) \bar{phi} handle {Ctx'} join \bar{phi} next {\bar{B}} except \bar{phi} | Ctx' \in Ctxs' ]
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Box} except \bar{phi} ) -> Ctxs'''
     -------------------------------------------------------------------------------------------------------------(FollowCatchHandle1)
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {Ctx} join \bar{phi} next {\bar{B}} except \bar{phi}) ->
                Ctxs'' \cup {try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Box} except \bar{phi}  } \cup Ctxs'''
     */

    /*
     allCatchHandleCtxs =  [ Ctx | try {\bar{B}} catch (ex) \bar{phi}  handle {Ctx} join \bar{phi} next {\bar{B}} except \bar{phi} <- allCtxs ]
     allCatchHandleCtxs |- follow(Ctx) -> Ctxs'
     \forall Ctx' \in Ctxs': not(Last(Ctx'))
     Ctxs'' = [ try {\bar{B}} catch (ex) \bar{phi} handle {Ctx'} join \bar{phi} next {\bar{B}} except \bar{phi} | Ctx' \in Ctxs' ]
     -------------------------------------------------------------------------------------------------------------(FollowCatchHandleTry2)
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi}  handle {Ctx} join \bar{phi} next {\bar{B}} except \bar{phi})  -> Ctxs'' 
     */


    case TCatchHandle(c) => {
      val daenv = appDec(unTCatchHandle, aenv)
      val ctxsp = follow(c, daenv)
      if (ctxsp.exists(isLast(_))) {
        ctxsp.map(TCatchHandle(_)) ++ List(TTryNext(TBox)) ++ follow(TTryNext(TBox), aenv)
      } else {
        ctxsp.map(TCatchHandle(_))
      }
    }

    /*
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Box} except \bar{phi}) -> Ctxs'
     ----------------------------------------------------------------------------------------------------------------(FollowTryJoin)
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join BBox next {\bar{B}} except \bar{phi} ) ->
      {try {\bar{B}} catch (ex) \bar{phi} handle {\bar{phi}} join \bar{phi} next {Box} except \bar{phi} } \cup Ctxs'
     */      
    case TTryJoin => TTryNext(TBox)::follow(TTryNext(TBox), aenv) // building the closure


    /*
     allNextCtxs = [ Ctx | (try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Ctx} except \bar{phi}) <- allCtxs ]
     allNextCtxs |- follow(Ctx) -> Ctxs'
     Ctxs'' = [ (try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B} join \bar{phi} next {Ctx'} except \bar{phi})  | Ctx' \in Ctxs' ]
     ------------------------------------------------------------------------------------------------------------(FollowTryNext)
     allCtxs |- follow(try {\bar{B}} catch (ex) \bar{phi} handle {\bar{B}} join \bar{phi} next {Ctx} except \bar{phi}) -> Ctxs''
     */


    case TTryNext(c) => {
      val daenv = appDec(unTTryNext, aenv)
      follow(c, daenv).map(TTryNext(_))
    }

    case TTryExcept => Nil



    /*
     allCtxs |- follow(join \bar{\phi} while {Box} next {\bar{B}} except \bar{phi}) -> Ctxs' 
     ------------------------------------------------------------------------------------------------------------(FollowWhileJoin0)
     allCtxs |- follow(join BBox^{0} while {\bar{B}} next {\bar{B}} except \bar{phi}) ->
     { join \bar{\phi} while {Box} next {\bar{B}} except \bar{phi} \cup  Ctxs' 
     */
     /*
     allCtxs |- follow(join \bar{\phi} while {\bar{B}} next {Box} except \bar{phi}) -> Ctxs'
     ------------------------------------------------------------------------------------------------------------(FollowWhileJoin1)
     allCtxs |- follow(join BBox^{1} while {\bar{B}} next {\bar{B}} except \bar{phi}) ->
     { join \bar{\phi} while {\bar{B}} next {Box} except \bar{phi}
     } \cup  Ctxs'
     */
    case TWhileJoin(b) if b == 0 =>
      TWhile(TBox)::follow(TWhile(TBox), aenv)
    case TWhileJoin(b) =>
      TWhileNext(TBox)::follow(TWhileNext(TBox), aenv)


    /*
     allWhileCtxs = [ Ctx | (join \bar{\phi} while {Ctx} next {\bar{B}} except \bar{phi}) <- aenv ]
     allWhileCtxs |- follow(Ctx) -> Ctxs'
     \exists Ctx' \in Ctxs': Last(Ctx')
     Ctxs'' = [ (join \bar{\phi} while {Ctx'} next {\bar{B}} except \bar{phi}) | Ctx' \in Ctxs' ]
     allCtxs |- follow(join BBox^{1} while {\bar{B}} next {\bar{B}} except \bar{phi}) -> Ctxs'''     
     ------------------------------------------------------------------------------------------------------------(FollowWhile1)
     allCtxs |- follow(join \bar{\phi} while {Ctx} next {\bar{B}} except \bar{phi}) ->
      Ctxs'' \cup {join BBox^{1} while {\bar{B}} next {\bar{B}} except \bar{phi}} \cup Ctxs'''
     */

    /*
     allWhileCtxs = [ Ctx | (join \bar{\phi} while {Ctx} next {\bar{B}} except \bar{phi}) <- aenv ]
     allWhileCtxs |- follow(Ctx) -> Ctxs'
     \forall Ctx' \in Ctxs': not(Last(Ctx'))
     Ctxs'' = [ (join \bar{\phi} while {Ctx'} next {\bar{B}} except \bar{phi}) | Ctx' \in Ctxs' ]
     ------------------------------------------------------------------------------------------------------------(FollowWhile2)
     allCtxs |- follow(join \bar{\phi} while {Ctx} next {\bar{B}} except \bar{phi}) -> Ctxs''
     */

    case TWhile(c) => {
      val daenv = appDec(unTWhile, aenv)
      val ctxsp = follow(c, daenv)
      if (ctxsp.exists(isLast(_))) {
        ctxsp.map(TWhile(_)) ++ List(TWhileJoin(1)) ++ follow(TWhileJoin(1), aenv)
      } else {
        ctxsp.map(TWhile(_))
      }
    }

    /*
     allNextCtxs = [ Ctx | (join \bar{\phi} while {\bar{B}} next {Ctx} except \bar{phi}) <- allCtxs ]
     allNextCtxs |- follow(Ctx) -> Ctxs'
     Ctxs'' = [ (join \bar{\phi} while {\bar{B}} next {Ctx'} except \bar{phi}) | Ctx' \in Ctxs' ]
     ------------------------------------------------------------------------------------------------------------(FollowWhileNext)
     allCtxs |- follow(join \bar{\phi} while {\bar{B}} next {Ctx} except \bar{phi}) -> Ctxs''
     */
      
    case TWhileNext(c) => {
      val daenv = appDec(unTWhileNext, aenv)
      follow(c, daenv).map(TWhileNext(_))
    }
    case TWhileExcept => Nil


    /*
     allCtxs |- follow(attempt E.m(E) as x next {Box}) -> Ctxs'' 
     ------------------------------------------------------------------------------------------------------------(FollowAttempt)
     allCtxs |- follow(attempt BBox next {\bar{B}}) -> {attempt E.m(E) as x next {Box}} \cup Ctxs''     
    */
    case TAttempt => TAttemptNext(TBox)::follow(TAttemptNext(TBox), aenv)

    /*
     allNextCtxs = [ Ctx | (attempt E.m(E) as x next {Ctx}) <- allCtxs ]
     allNextCtxs |- follow(Ctx) -> Ctxs'
     Ctxs'' = [ (attempt E.m(E) as x next {Ctx'}) | Ctx' \in Ctxs' ]
     ------------------------------------------------------------------------------------------------------------(FollowAttempt)
     allCtxs |- follow(attempt E.m(E) as x next {Ctx}) -> \cup Ctxs''     
    */

    case TAttemptNext(c) => {
      val daenv = appDec(unTAttemptNext, aenv)
      follow(c, daenv).map(TAttemptNext(_))      
    }

    case TThrow => Nil
  } 

  // list of extractors

  
  def unTHead(tctx:TCtx):Option[TCtx] = tctx match {
    case THead(c) => Some(c)
    case _        => None
  }

  def unTTail(tctx:TCtx):Option[TCtx] = tctx match {
    case TTail(c) => Some(c)
    case _        => None
  }

  def unTThen(tctx:TCtx):Option[TCtx] = tctx match {
    case TThen(c) => Some(c)
    case _        => None
  } 
   
  def unTElse(tctx:TCtx):Option[TCtx] = tctx match {
    case TElse(c) => Some(c)
    case _        => None
  }

  def unTIfNext(tctx:TCtx):Option[TCtx] = tctx match {
    case TIfNext(c) => Some(c)
    case _          => None
  }

  def unTWhile(tctx:TCtx):Option[TCtx] = tctx match {
    case TWhile(c) => Some(c)
    case _         => None
  }

  def unTWhileNext(tctx:TCtx):Option[TCtx] = tctx match {
    case TWhileNext(c) => Some(c)
    case _             => None
  }


  def unTTry(tctx:TCtx):Option[TCtx] = tctx match {
    case TTry(c) => Some(c) 
    case _       => None
  }

  def unTCatchHandle(tctx:TCtx):Option[TCtx] = tctx match {
    case TCatchHandle(c) => Some(c)
    case _               => None
  }
  

  def unTTryNext(tctx:TCtx):Option[TCtx] = tctx match {
    case TTryNext(c) => Some(c) 
    case _          => None
  }


  def unTAttemptNext(tctx:TCtx):Option[TCtx] = tctx match {
    case TAttemptNext(c) => Some(c) 
    case _               => None
  }

  // apply the deconstructor to the list of contexts; remove those return None
  def appDec(dec:TCtx => Option[TCtx], ts:List[TCtx]):List[TCtx] = ts.map(dec(_)).filter( x => !x.isEmpty).flatMap(x => x match {
    case Some(c) => List(c)
    case None => Nil
  })

  // ****************************** implementing isLast end *********************************************************************



  // ****************************** implementing partial order begin ************************************************************
  // return the domain of a mapping

  def dom[A,B](m:List[(A,B)]):List[A] = m.map(x => x match { case (a,b) => a } )


  def partialOrderTCtx(aenv:AEnv, eenv:EEnv):PartialOrder[TCtx] = new PartialOrder[TCtx]{
    // TODO, we need two versions of partialCompare,
    // 1) for ok flow
    // 2) for err flow
    override def partialCompare(x: TCtx, y: TCtx): Double = 
    { 
      (x,y) match {
        case (_,_) if (eqTCtx.eqv(x,y)) => 0.0
        // CtxOrdHole
        case (TBox, _) => -1.0 
        case (_, TBox) => 1.0

        // CtxOrdInd specialized for Head
        case (THead(ctx1), THead(ctx2)) => {
          val daenv = appDec(unTHead, aenv)
          val deenv = appDec(unTHead, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        }

        // CtxOrdSeq
        // CTX;\bar{B} contains no exception
        // otherwise, we would have use attempt 
        case (THead(_), TTail(_))  => -1.0 

        // CtxOrdInd specialized for TTail
        case (TTail(ctx1), TTail(ctx2)) =>  {
          val daenv = appDec(unTTail, aenv)
          val deenv = appDec(unTTail, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        }

        // CtxOrdSeq - dual 
        case (TTail(_), THead(_))  => -partialCompare(y,x) 

        // CtxOrdInd specialized for TThen
        case (TThen(ctx1), TThen(ctx2)) => {
          val daenv = appDec(unTThen, aenv)
          val deenv = appDec(unTThen, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        }
        
        // CtxOrdThen 
        // is the check for !(eenv.contains(x)) still needed? 
        // it seems not needed, if isLast(c) ==> isErr(c) is false ==> c can't be an error context.
        // case (TThen(c), TIfJoin) if isLast(c) && !(eenv.contains(x)) => -1.0
        // case (TThen(c), TIfJoin) if isLast(c, appDec(unTThen, aenv)) && (eenv.contains(x)) => Double.NaN
        
        case (TThen(c), TIfJoin) if isLast(c) => -1.0
        case (TThen(c), TIfJoin) if isErr(c) => Double.NaN
        // if not last, we need to apply the transtivity
        case (TThen(c), TIfJoin) if follow(c, appDec(unTThen, aenv)).exists(isLast(_)) => -1.0
        case (TThen(c), TIfJoin) => Double.NaN

        
        // CtxOrdInd specialized for TElse
        case (TElse(ctx1), TElse(ctx2)) => {
          val daenv = appDec(unTElse, aenv)
          val deenv = appDec(unTElse, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        }
        
        // CtxOrdElse 
        case (TElse(c), TIfJoin) if isLast(c) => -1.0
        case (TElse(c), TIfJoin) if isErr(c) => Double.NaN
        // if not last, we need to apply the transtivity
        case (TElse(c), TIfJoin) if follow(c, appDec(unTElse, aenv)).exists(isLast(_)) => -1.0
        case (TElse(c), TIfJoin) => Double.NaN

        // CtxOrdThen - dual 
        case (TIfJoin, TThen(c)) => -partialCompare(y,x) 
        // CtxOrdElse - dual 
        case (TIfJoin, TElse(c)) => -partialCompare(y,x) 

        // CtxOrdWhileEntry1 
        case (TWhileJoin(0), TWhile(_)) => -1.0 
        case (TWhile(_), TWhileJoin(0)) => 1.0

        // CtxOrdWhileEntry2
        case (TWhile(c), TWhileJoin(1)) if isLast(c) => -1.0
        case (TWhile(c), TWhileJoin(1)) if isErr(c)  => Double.NaN
        case (TWhile(c), TWhileJoin(1)) if follow(c, appDec(unTWhile, aenv)).exists(isLast(_)) => -1.0
        case (TWhile(c), TWhileJoin(1)) => Double.NaN 

        // CtxOrdInd specialized for TWhile
        case (TWhile(ctx1), TWhile(ctx2)) => {
          val daenv = appDec(unTWhile, aenv)
          val deenv = appDec(unTWhile, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        }

        case (TWhileJoin(1), TWhile(c)) => -partialCompare(y,x) 


        // CtxOrdInd specialized for TTry
        case (TTry(ctx1), TTry(ctx2)) => {
          val daenv = appDec(unTTry, aenv)
          val deenv = appDec(unTTry, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        } 
        // we don't need to handle this in the ok flow, right?
        // case (TTry(c), TCatch) if isErr(c) => -1.0 // (CtxOrdTry1)

        case (TTry(c), TTryJoin) if isLast(c) => -1.0
        case (TTry(c), TTryJoin) if isErr(c) => Double.NaN 
        case (TTry(c), TTryJoin) if follow(c, appDec(unTTry, aenv)).exists(isLast(_)) => -1.0 
        case (TTry(c), TTryJoin) => Double.NaN 
        case (TTryJoin, TTry(c)) => -partialCompare(y,x) 

        case (TTry(ctx1), TTryNext(ctx2)) => partialCompare(TTry(ctx1), TTryJoin)
        case (TTryNext(ctx1), TTry(ctx2)) => -partialCompare(y,x) 

        case (TCatch, TCatchHandle(_)) => -1.0 
        case (TCatchHandle(_), TCatch) => 1.0
        case (TCatch, TTryJoin) => partialCompare(TCatchHandle(TBox), TTryJoin)
        case (TTryJoin, TCatch) => -partialCompare(y,x)
        case (TCatch, TTryNext(ctxt))  => partialCompare(TCatchHandle(TBox), TTryNext(ctxt))
        case (TTryNext(ctxt), TCatch)  => -partialCompare(y,x)

        case (TCatchHandle(ctx1), TCatchHandle(ctx2)) => {
          val daenv = appDec(unTCatchHandle, aenv)
          val deenv = appDec(unTCatchHandle, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        }

        case (TCatchHandle(c), TTryJoin) if isLast(c) => -1.0 
        case (TCatchHandle(c), TTryJoin) if isErr(c) => Double.NaN
        case (TCatchHandle(c), TTryJoin) if follow(c, appDec(unTTry, aenv)).exists(isLast(_)) => -1.0 
        case (TCatchHandle(c), TTryJoin) => Double.NaN
        case (TTryJoin, TCatchHandle(c)) => -partialCompare(y,x)

        case (TCatchHandle(ctx1), TTryNext(ctx2)) => partialCompare(TCatchHandle(ctx1), TTryJoin) 
        case (TTryNext(ctx1), TCatchHandle(ctx2)) => -partialCompare(y,x)

        case (TTryNext(ctx1), TTryNext(ctx2)) => {
          val daenv = appDec(unTTryNext, aenv)
          val deenv = appDec(unTTryNext, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        } 

        case (TAttempt, TAttemptNext(c)) => -1.0
        case (TAttemptNext(c), TAttempt) => 1.0

        case (TAttemptNext(ctx1), TAttemptNext(ctx2)) => {
          val daenv = appDec(unTAttemptNext, aenv)
          val deenv = appDec(unTAttemptNext, eenv)
          partialOrderTCtx(daenv, deenv).partialCompare(ctx1,ctx2)
        } 

        case _ => Double.NaN
      }
    }
  }



  // ****************************** implementing partial order end ************************************************************


  /**
  * combine - return the lub from the set
  *
  * @param cs
  * @return
  */
  def combine[A](cs:List[(TCtx,A)], aenv:AEnv, eenv:EEnv) :List[(TCtx,A)] = cs match {
    case Nil => Nil
    case x::Nil => x::Nil
    case (x::xs) => {
      val ys = xs.filter(  y => !(partialOrderTCtx(aenv,eenv).partialCompare(y._1,x._1) == -1.0))
      if (ys.exists( y => partialOrderTCtx(aenv,eenv).partialCompare(x._1,y._1) == -1.0))
      { 
        combine(ys, aenv, eenv) 
      }
      else {
        x::combine(ys, aenv, eenv)
      }
      
    }
  }
  
  
  def Rlt(aenv:AEnv, eenv:EEnv, ctx:TCtx, vm:VarMap, x:Name):List[(TCtx,Name)] = { 
    def cmp(p:(TCtx,TCtx)):Boolean = p match {
      case (tctx1, tctx2)  => (partialOrderTCtx(aenv, eenv).partialCompare(tctx1, tctx2) == -1.0)
    }
    R(aenv, eenv, ctx, vm, x, cmp)
  }

  def Rleq(aenv:AEnv, eenv:EEnv, ctx:TCtx, vm:VarMap, x:Name):List[(TCtx,Name)] = {
    def cmp(p:(TCtx,TCtx)):Boolean = p match {
        case ((tctx1, tctx2))  => { 
          val pot = partialOrderTCtx(aenv, eenv)
          ((pot.partialCompare(tctx1, tctx2) == -1.0) || (pot.partialCompare(tctx1, tctx2) == 0.0))
        }
      }
    R(aenv, eenv, ctx, vm, x, cmp)
  }

  /**
    * Compute the name from the lub of all the reachable context until ctx
    *  it defers from the paper, which takes in a default value, we return None in case the set of contexts an empty set. The defaulting should be handled at the call site.
    *
    * @param eenv - exception throwing program contexts
    * @param ctx
    * @param vm
    * @param x
    * @param cmp - modifier to switch between leq or lt
    * @return - return the name of the variable that is the most recent dominator of x
    */
  def R(aenv:AEnv, eenv:EEnv, ctx:TCtx, vm:VarMap, x:Name, cmp:((TCtx,TCtx)) => Boolean):List[(TCtx,Name)] = vm.get(x) match { // perhaps we should report the error properly
    case None => Nil
    case Some(trs) => {
      val tcvs = for { 
        (tctx, tx) <- trs.toList
        if (cmp(tctx, ctx))
      } yield (tctx, tx)

      /* not using the builtin semilattice class
      // partial function, but lub should be in the set.
      def comb(px:(TCtx,Name), py:(TCtx,Name)):(TCtx, Name) = (px, py) match {
        case ((cx, vx), (cy, vy)) if (semilatticeTCtx(aenv, eenv, benv, cenv).combine(cx, cy) == cx) => (cx, vx)
        case ((cx, vx), (cy, vy)) if (semilatticeTCtx(aenv, eenv, benv, cenv).combine(cx, cy) == cy) => (cy, vy)
        
      }

      tcvs match {
        case Nil => None
        case (tcv::tcvs) => tcvs.foldLeft(tcv)((x,y) => comb(x,y)) match {
          case (_, vx) => Some(vx)
        }
      }      
      */
      combine(tcvs, aenv,eenv) 
    }
  }

  /** data type to keep track of the 
      exception-throwing method ids
    */
  type PkgName = Name
  type ClsName = Name
  type MethodName = Ident
  type ExceptionThrowers = Map[PkgName, Map[ClsName, MethodName]]

  /**
    * kmethodDecl - converts a method to SSA method declaration
    *
    * @param md
    * @param m
    * @return
    */
  def kmethodDecl(md:MethodDecl)(using m:MonadError[SSAState, ErrorM]):SState[State, SSAMethodDecl] = md match {
    case MethodDecl(modifiers, type_params, return_ty, fname, formal_params, ex_types, exp, body) => body match {
      case MethodBody(None) => m.pure(SSAMethodDecl(modifiers, type_params, return_ty, fname, formal_params, ex_types, exp, SSAMethodBody(Nil)))
      case MethodBody(Some(block)) => {
        val vm = formal_params.foldLeft(Map():VarMap)((vm, param) => {
          vm + ((paramIdtoName(param)) -> Map(TBox -> paramIdtoName(param)))
        })
        val lbl = TBox
        for {
          _ <- setVarMap(vm)
          blocks <- kBlock(block, TBox) 
          _ <- addToAEnv(TBox)
          // lbl <- toLbl(TBox) 
          varDeclsStmts <- genVarDecls 
          /*
          varDecls <- m.pure(varDeclsStmts match {
            case Nil => Nil
            case (s::ss) => List(SSABlock(lbl, varDeclsStmts))
            })
            */
        } yield SSAMethodDecl(modifiers, type_params, return_ty, fname, formal_params, ex_types, exp, SSAMethodBody(prependVarDecls(lbl, varDeclsStmts, blocks)))
      }
    }
  }


  /**
    * kstmtBlock - a special version just to handle StmtBlock, when the statement encloses a block of statements, we convert them using kBlock
    *               otherwise, we call kstmt
    *
    * @param stmt
    * @param ctx
    * @param st
    * @return
    */
  
  def kstmtBlock(stmt:Stmt, ctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State,List[SSABlock]] = stmt match {
    // case StmtBlock(Block(blkStmts)) => kblkStmts(blkStmts,ctx)
    case StmtBlock(blk) => kBlock(blk,ctx)
    case _ => for {
      b <- kstmt(stmt, ctx)
    } yield List(b)
  }
  

  /**
    * kBlock - convert a block of statements
    *
    * @param blk
    * @param ctx
    * @param m
    * @return
    */
  def kBlock(blk:Block, ctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State,List[SSABlock]] = blk match {
    case Block(blkStmts) => kblkStmts(blkStmts,ctx)
  }


  /**
    * kblkStmts - convert a list of block stmts. It is correspondent to the \overline{KS} in the paper.
    *
    * @param blkStmts
    * @param ctx
    * @param m
    * @return
    */

  def kblkStmts(blkStmts:List[BlockStmt], ctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, List[SSABlock]] = blkStmts match {
    case Nil => m.pure(Nil)
    case (bstmt::Nil) => for {
      _ <- addToAEnv(ctx)
      b <- kblkStmt(bstmt,putTCtx(ctx, TNop))
    } yield List(b)
    case (bstmt::rest) => for {
      _ <- addToAEnv(ctx)
      b <- kblkStmt(bstmt,putTCtx(ctx, THead(TBox)))
      bs <- kblkStmts(rest, putTCtx(ctx, TTail(TBox)))
    } yield (b::bs)
  }


  /**
    * kblkStmt - convert a block statement, it does not handle local class. LocalVars are converted into a SSAVarDecls which is not mentioned in the paper.
    *
    * @param blkStmt
    * @param ctx
    * @param m
    * @return
    */
  def kblkStmt(blkStmt:BlockStmt, ctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State,SSABlock] = blkStmt match {
    case BlockStmt_(stmt) => kstmt(stmt, ctx) 
    case LocalClass(_) => m.raiseError("SSA construction failed, local class is not supported.")
    case LocalVars(mods, ty, varDecls) => kVarDecls(mods, ty, varDecls, ctx)
  }


  /**
    * kVarDecls - converts a var declaration, we convert the initialization like other statement / expression. We also keep track of the declration in the state
    *
    * @param mods
    * @param ty
    * @param varDecls
    * @param ctx
    * @param m
    * @return
    */
  def kVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl], tctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, SSABlock] = for {
    // a new case, combining KVD and KSTMT assignment
    // we first record all the variable name and type
    _ <- recordVarDecls(mods, ty, varDecls, tctx)
    // we then convert varDecls to SSAVarDecl, hm... what about array init?
    // array id should not be renamed and should not be merged in phis, we keep them inplace as 
    // lbl <- toLbl(tctx)
    varDecls1 <- kVarDecls(varDecls, tctx)
  } yield SSABlock(tctx, List(SSAVarDecls(mods, ty, varDecls1)))
  
  def recordVarDecls(mods:List[Modifier], ty:Type, varDecls:List[VarDecl], tCtx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State,Unit] = varDecls match {
    case Nil => m.pure(())
    case (varDecl::rest) => varDecl match {
      case VarDecl(VarId(id), v_init) => for {
        _ <- addNestedVarDecls(tCtx, id, ty, mods)
        _ <- recordVarDecls(mods, ty, rest, tCtx)
      } yield ()
      case VarDecl(_, v_init) => // it is an array declaration, we don't need to record it as array id will never be merged in phis.
        recordVarDecls(mods, ty, rest, tCtx)
    }
  }

  def kVarDecls(varDecls:List[VarDecl], tCtx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State,List[VarDecl]] = varDecls match {
    case Nil => m.pure(Nil)
    case (varDecl::rest) => varDecl match {
      case VarDecl(VarId(id), ov_init) => for {
        // lbl <- toLbl(tCtx)
        id1 <- mkId(id, tCtx)
        ov_init1 <- ov_init match {
          case None => m.pure(None)
          case Some(v_init) => for { 
            vi1 <- kVarInit(v_init,tCtx)
          } yield Some(vi1)
        }
        rest1 <- kVarDecls(rest, tCtx)
      } yield (VarDecl(VarId(id1), ov_init1)::rest1)
      // array init, we do not rename the lhs ID, we only convert the referenced ids on the RHS
      case VarDecl(VarDeclArray(var_decl_id), ov_init) => for {
        ov_init1 <- ov_init match {
          case None => m.pure(None)
          case Some(v_init) => for {
            vi1 <- kVarInit(v_init, tCtx)
          } yield Some(vi1)
        }
        rest1 <- kVarDecls(rest, tCtx)
      } yield (VarDecl(VarDeclArray(var_decl_id), ov_init1)::rest1)
    }
  }

  def kVarInit(vInit:VarInit, tCtx:TCtx)(using m:MonadError[SSAState, ErrorM]): SState[State, VarInit] = vInit match {
    case InitExp(exp) => for {
      exp1 <- kexp(exp, tCtx) 
    } yield InitExp(exp1)
    case InitArray(ArrayInit(var_inits)) => for {
      var_inits1 <- var_inits.traverse(vi => kVarInit(vi, tCtx))
    } yield InitArray(ArrayInit(var_inits1))
  }


  /**
    * kexp - converts an expression, correspondent to the KE function in the paper
    *
    * @param e
    * @param ctx - for keeping track of method invocation location. not used in resolving the most recent variable definition
    * @param m
    * @return
    */

  def kexp(e:Exp, ctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, Exp] = e match {
    case ArrayAccess(ArrayIndex(e, es)) => for {
      e1 <- kexp(e, ctx)
      es1 <- es.traverse(kexp(_, ctx))
    } yield ArrayAccess(ArrayIndex(e1, es1))
    case Cast(ty, exp) => for {
      exp1 <- kexp(exp, ctx) 
    } yield Cast(ty,exp1)
    case ArrayCreate(ty, exps, num_dims) => for {
      exps1 <- exps.traverse(kexp(_, ctx))
    } yield ArrayCreate(ty, exps1, num_dims)
    
    case ArrayCreateInit(ty, size, ArrayInit(v_inits)) => for {
      v_inits1 <- v_inits.traverse(kVarInit(_, ctx))
    } yield ArrayCreateInit(ty, size, ArrayInit(v_inits1))
    case Assign(lhs, op, rhs) => m.raiseError("SSA construction failed, assignment expression should be handled in kstmt.") // might not be true, it does not work with x = (y = 1) + 1 // maybe it should have been flattened or desugared? todo: double check
    case BinOp(e1, op, e2) => for {
      e1p <- kexp(e1, ctx)
      e2p <- kexp(e2, ctx)
    } yield BinOp(e1p, op, e2p)
  
    case ClassLit(ty) => m.pure(e) 
    case Cond(cond, true_exp, false_exp) => for {
      cond1 <- kexp(cond,ctx)
      true_exp1 <- kexp(true_exp, ctx)
      false_exp1 <- kexp(false_exp, ctx)
    } yield Cond(cond1, true_exp1, false_exp1)
    case ExpName(name) => for {
      st <- get
      exp1 <- st match {
        case State(vm, eCtx, aenv, eenv, nDecls, methInvs, srcLblEnv,conf) => Rleq(aenv, eenv, eCtx, vm, name) match {
          case Nil => m.raiseError(s"SSA construction failed, Rlt failed to find a lub for ${name} during expression conversion. None exists. ${eCtx}, ${vm.toList}")
          case (c,name1)::Nil => m.pure(ExpName(name1))
          case _::_ => m.raiseError(s"SSA construction failed, Rlt failed to find a lub for ${name} during expression conversion. More than one candidates found. ${eCtx}, ${vm.toList}")
        }
      }
    } yield exp1
    
    case FieldAccess_(access) => access match {
      case PrimaryFieldAccess(e, id) => for {
        e1 <- kexp(e, ctx)
      } yield FieldAccess_(PrimaryFieldAccess(e1, id))
      case SuperFieldAccess(id) => m.pure(e)
      case ClassFieldAccess(n, id) => m.pure(e)
    }
    case InstanceCreation(type_args, type_decl, args, body) => 
     
      for {
        args1 <- args.traverse(arg => kexp(arg, ctx))
        // not modifying the body. 
      } yield InstanceCreation(type_args, type_decl, args1, body)
    case InstanceOf(e, ref_type) => for {
      e1 <- kexp(e, ctx)
    } yield InstanceOf(e1, ref_type) 
    case Lambda(params, body) => m.raiseError("SSA construction failed, lambda expression is not supported.")
    case Lit(lit) => m.pure(e)
    case MethodInv(methodInv) => methodInv match {
      // method name does not need to be convert
      case MethodCall(name, args) => for {
        args1 <- args.traverse(arg => kexp(arg, ctx))
        _ <- addMethodInv(ctx, MethodCall(name, args1))
      } yield MethodInv(MethodCall(name, args1))

      case PrimaryMethodCall(e, ref_types, id, args) => for {
        args1 <- args.traverse(arg => kexp(arg, ctx))
        e1 <- kexp(e, ctx)
        _ <- addMethodInv(ctx, PrimaryMethodCall(e1, ref_types, id, args1))
      } yield MethodInv(PrimaryMethodCall(e1, ref_types, id, args1))

      case ClassMethodCall(name, ref_types, id, args) => for {
        args1 <- args.traverse(arg => kexp(arg, ctx))
        _ <- addMethodInv(ctx, ClassMethodCall(name, ref_types, id, args1))
      } yield MethodInv(ClassMethodCall(name, ref_types, id, args1))

      case SuperMethodCall(ref_types, id, args) => for {
        args1 <- args.traverse(arg => kexp(arg, ctx))
        _ <- addMethodInv(ctx, SuperMethodCall(ref_types, id, args1))
      } yield MethodInv(SuperMethodCall(ref_types, id, args1))

      case TypeMethodCall(name, ref_types, id, args) => for {
        args1 <- args.traverse(arg => kexp(arg, ctx))
        _ <- addMethodInv(ctx, TypeMethodCall(name, ref_types, id, args1))
      } yield MethodInv(TypeMethodCall(name, ref_types, id, args1))
    }
    case MethodRef(name, id) => m.pure(e) 
    case PostDecrement(exp) => m.raiseError("SSA construction failed, PostDecrement expression should have been desugared.")
    case PostIncrement(exp) => m.raiseError("SSA construction failed, PostIncrement expression should have been desugared.")
    case PreBitCompl(exp) => for {
      e1 <- kexp(exp, ctx) 
    } yield PreBitCompl(e1) 
    case PreDecrement(exp) => m.raiseError("SSA construction failed, PreDecrement expression should have been desugared.")
    case PreIncrement(exp) => m.raiseError("SSA construction failed, PreIncrement expression should have been desugared.")
    case PreMinus(exp) => for {
      exp1 <- kexp(exp, ctx)
    } yield PreMinus(exp1) 
    case PreNot(exp) => for {
      exp1 <- kexp(exp, ctx)
    } yield PreNot(exp1) 
    case PrePlus(exp) => for {
      exp1 <- kexp(exp, ctx)
    } yield PrePlus(exp1)
    case QualInstanceCreation(exp, type_args, id, args, body) => m.raiseError("SSA construction failed, Qualified Instance creation expression is not supported.")
    case This => m.pure(e) 
    case ThisClass(name) => m.pure(e) 
  }



  /**
    * kstmt - convert a statement, correspond to KS in the paper.
    *
    * @param stmt - the Java statement to be converted
    * @param ctx  - the current SSA Context
    * @param m    - Monad object
    * @return     - an SSA Block
    */
  

  def kstmt(stmt:Stmt, tctx:TCtx)(using m:MonadError[SSAState, ErrorM]):SState[State, SSABlock] = {
    val lbl = tctx
    stmt match {
      case Assert(exp, msg) => for {
        _    <- addToAEnv(tctx)
        exp1 <- kexp(exp, tctx)
        msg1 <- msg.traverse( m => kexp(m, tctx))
        _    <- setOkCtx(tctx)
      } yield SSABlock(lbl, List(SSAAssert(exp1, msg1)))

      case BasicFor(init, loop_cond, post_update, stmt) => m.raiseError("SSA construction failed, BasicFor should have been desugared.")

      case EnhancedFor(modifiers, ty, id, exp, stmt) => m.raiseError("SSA construction failed, EnhancedFor should have been desugared.")

      case Break(_) => m.raiseError("SSA construction failed, Break is not supported.")
      case Continue(_) => m.raiseError("SSA construction failed, Continue is not supported.") 
      /* break and continue are not supported yet.
      case Break(None) => m.raiseError("SSA construction failed, Break statement is associated with no label. It should have been pre-processed.")
      case Break(Some(id)) => for {
        st <- get
        r <- st match {
          case State(vm, eCtx, aenv, eenv, nestedDecls, metodInvs, srcLabelEnvs,conf) => srcLabelEnvs.get(id) match {         
            case None  => m.raiseError("SSA construction failed. Break statement is associated with a undefined label.")
            case Some(sctx) => for {
              tctx  <- m.pure(kctx(ctx))
              target_tctx <- m.pure(kctx(sctx))
            } yield SSABlock(lbl, List(SSABreak(target_tctx)))
          }
        }
      } yield r

      case Continue(None) => m.raiseError("SSA construction failed, Continue statement is associated with no label. It should have been pre-processed.")
      case Continue(Some(id)) => for {
        st <- get
        r <- st match {
          case State(vm, eCtx, aenv, eenv, nestedDecls, metodInvs, srcLabelEnvs, conf) => srcLabelEnvs.get(id) match {         
            case None  => m.raiseError("SSA construction failed. Continue statement is associated with a undefined label.")
            case Some(sctx) => for {
              tctx  <- m.pure(kctx(ctx))
              target_tctx <- m.pure(kctx(sctx))
            } yield SSABlock(tctx, List(SSAContinue(target_tctx)))
          }
        }
      } yield r
      */

      case Do(stmt, exp) => m.raiseError("SSA construction failed, Do should have been desguared.")

      case Empty => for {
        _    <- addToAEnv(tctx)
        _   <- setOkCtx(tctx)
      } yield (SSABlock(lbl, List(SSAEmpty)))

      case ExpStmt(Assign(lhs, op, rhs)) => lhs match {
        case NameLhs(x) => for {
          st <- get
          _  <- addToAEnv(tctx)
          b <- st match {
            case State(vm, eCtx, aenv, eenv, nDecls, methInvs, srcLblEnv, conf) => for {
              rhs1 <- kexp(rhs, tctx)
              xlbl <- mkName(x,lbl)
              vm1 <- m.pure(vm.get(x) match {
                case None => vm + (x -> Map(tctx -> xlbl))
                case Some(im) => vm + (x -> (im + (tctx -> xlbl)))
              })
              _ <- setVarMap(vm1)
            } yield SSABlock(lbl, List(SSAAssignments(List(ExpStmt(Assign(NameLhs(xlbl), op, rhs1))))))
          }
          _    <- setOkCtx(tctx)        
        } yield b

        case FieldLhs(fa) => fa match {
          case PrimaryFieldAccess(e1, id) => for {
            _  <- addToAEnv(tctx)
            rhs1 <- kexp(rhs, tctx)
            e2  <- kexp(e1, tctx)
            _    <- setOkCtx(tctx)
          } yield SSABlock(lbl, List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(PrimaryFieldAccess(e2, id)), op, rhs1))))))
          case SuperFieldAccess(id) => for {
            _  <- addToAEnv(tctx)
            rhs1 <- kexp(rhs, tctx)
            _    <- setOkCtx(tctx)
          } yield SSABlock(lbl, List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(SuperFieldAccess(id)), op, rhs1))))))
          case ClassFieldAccess(name, id) => for {
            _  <- addToAEnv(tctx)
            rhs1 <- kexp(rhs, tctx)
            _    <- setOkCtx(tctx)
          } yield SSABlock(lbl, List(SSAAssignments(List(ExpStmt(Assign(FieldLhs(ClassFieldAccess(name,id)), op, rhs1))))))
        }

        case ArrayLhs(ArrayIndex(e,es)) => for {
          _  <- addToAEnv(tctx)
          rhs1 <- kexp(rhs, tctx)
          e1   <- kexp(e, tctx)
          es1 <- es.traverse( e => kexp(e, tctx))
          _    <- setOkCtx(tctx)
        } yield SSABlock(lbl, List(SSAAssignments(List(ExpStmt(Assign(ArrayLhs(ArrayIndex(e1,es1)), op, rhs1))))))
        
      }

      
      /* we move it to kexp, see commented SSAMethodInvcation for details 
      case ExpStmt(MethodInv(methodInv)) => methodInv match {
        // method name does not need to be convert
        case MethodCall(name, args) => for {
          lbl <- toLbl(tctx)
          args1 <- args.traverse(arg => kexp(arg, tctx))
        } yield SSABlock(lbl, SSAMethodInvocation(MethodCall(name, args1)))

        case PrimaryMethodCall(e, ref_types, id, args) => for {
          lbl <- toLbl(tctx)
          args1 <- args.traverse(arg => kexp(arg, tctx))
          e1 <- kexp(e, tctx)
        } yield SSABlock(lbl, SSAMethodInvocation(PrimaryMethodCall(e1, ref_types, id, args1)))

        case ClassMethodCall(name, ref_types, id, args) => for {
          lbl <- toLbl(tctx)
          args1 <- args.traverse(arg => kexp(arg, tctx))
        } yield SSABlock(lbl, SSAMethodInvocation(ClassMethodCall(name, ref_types, id, args1)))

        case SuperMethodCall(ref_types, id, args) => for {
          lbl <- toLbl(tctx)
          args1 <- args.traverse(arg => kexp(arg, tctx))
        } yield SSABlock(lbl, SSAMethodInvocation(SuperMethodCall(ref_types, id, args1)))

        case TypeMethodCall(name, ref_types, id, args) => for {
          lbl <- toLbl(tctx)
          args1 <- args.traverse(arg => kexp(arg, tctx))
        } yield SSABlock(lbl, SSAMethodInvocation(TypeMethodCall(name, ref_types, id, args1)))
      }*/

      case ExpStmt(exp) => for {
        _  <- addToAEnv(tctx)
        exp1 <- kexp(exp, tctx)
        _    <- setOkCtx(tctx)
      } yield SSABlock(lbl, List(SSAExps(List(ExpStmt(exp1)))))

      case IfThen(exp, stmt) => m.raiseError("SSA construction failed, If then statment should have been desugared.")

      
      case IfThenElse(exp, then_stmt, else_stmt) => for {
        _          <- addToAEnv(tctx)
        exp1       <- kexp(exp, tctx)
        // reset the eenv in the state 
        st         <- get
        stThenIn   <- st match {
          case State(vm, eCtx, aenv, eenv, nDecls, methInvs, srcLblEnv, conf) => m.pure(State(vm, eCtx, aenv, Nil, nDecls, methInvs, srcLblEnv, conf))
        }
        _          <- put(stThenIn)
        then_ctx   <- m.pure(putTCtx(tctx, TThen(TBox)))
        then_stmts <- kstmtBlock(then_stmt, then_ctx)
        stThenOut  <- get
        stElseIn   <- st match {
          case State(vm, eCtx, aenv, eenv, nDecls, methInvs, srcLblEnv, conf) => m.pure(State(vm, eCtx, aenv, Nil, nDecls, methInvs, srcLblEnv, conf))
        }
        _          <- put(stElseIn)
        else_ctx   <- m.pure(putTCtx(tctx,TElse(TBox)))
        else_stmts <- kstmtBlock(else_stmt, else_ctx)
        stElseOut  <- get
        stMerged   <- m.pure(mergeState(st, stThenOut, stElseOut)) 
        _          <- put(stMerged)

        tctx2      <- m.pure(putTCtx(tctx, TIfJoin))
        lbl2       <- m.pure(tctx2)

        phis       <- mkPhi(st, stThenOut, stElseOut, lbl2)
        _          <- extendVarsWithContextAndLabel(phis.map( ph => ph match {case Phi(n, renamed, m) => n }), tctx2, lbl2) 
        _          <- setOkCtx(tctx2)
      } yield SSABlock(lbl, List(SSAIf(exp1, then_stmts, else_stmts, phis)))
      
      case Labeled(id, stmt) => for {
        _ <- addSrcLabel(id, ctx)
        r <- kstmt(stmt,ctx) 
      } yield r

      case Return(oexp) => oexp match {
        case Some(exp) => for {
          _  <- addToAEnv(tctx)
          // lbl  <- m.pure(tctx)
          exp1 <- kexp(exp,tctx) 
          _    <- setOkCtx(tctx)

        } yield SSABlock(lbl, List(SSAReturn(Some(exp1))))
        case None => for {
          _  <- addToAEnv(tctx)
          // lbl  <- m.pure(tctx)
          _    <- setOkCtx(tctx)
        } yield SSABlock(lbl, List(SSAReturn(None)))
      }

      case StmtBlock(blk) => m.raiseError("SSA construction failed, Statement Block should not be handled here.") // todo

      case Switch(exp, blocks) => m.raiseError("SSA construction failed, Switch statement is not supported.") // todo
      case Synchronized(exp, blk) => m.raiseError("SSA construction failed, Synchronized statement is not supported.") // todo

      case Throw(exp) => for {
        _  <- addToAEnv(tctx)
        // lbl  <- toLbl(tctx)
        exp1 <- kexp(exp, tctx)
        _    <- addToEEnv(tctx)
        _    <- setOkCtx(tctx)
      } yield SSABlock(lbl, List(SSAThrow(exp1)))

      /* TODO fix me
      case Try(try_blk, Catch(param, catch_blk)::Nil, finally_blk) => finally_blk match {
        case Some(b) => m.raiseError("SSA construction failed, Try catch finally should be desugared to Try catch.")
        case None    => for {
          _         <- addToAEnv(tctx)
          // lbl       <- toLbl(tctx)
          st        <- get

          stTryIn   <- st match {
            case State(vm, eCtx, aenv, eenv, benv, cenv, nestedDecls, methInvs, srcLblEnv) => m.pure(State(vm,eCtx,aenv, Nil, benv, cenv, nestedDecls, methInvs, srcLblEnv))
          }
          _         <- put(stTryIn)
          try_ctx   <- m.pure(putSCtx(ctx, STry(SBox)))
          try_stmts <- kBlock(try_blk, try_ctx)
          stTryOut  <- get

          tctx1p    <- m.pure(putTCtx(tctx, TTryPeriPhi))
          lbl1p     <- toLbl(tctx1p)
          phis_peri <- mkPhisFromThrows(stTryOut, eenvFromState(st), benvFromState(st), cenvFromState(st), lbl1p)

          catch_ctx <- m.pure(putSCtx(ctx, SCatch(SBox)))
          catch_tctx <- m.pure(putTCtx(tctx, TCatch(TBox)))
          stCatchIn <- stTryOut match {
            case State(vm1, eCtx1, aenv1, eenv1, benv1, cenv1, nestedDecls1, methInvs1, srcLblEnv1) => for {
              entries <- vm1.keySet.toList.traverse(v => for {
                v_lbl <- mkName(v, lbl1p)
              } yield (v, ctx, tctx1p, v_lbl))
            } yield State((entries.foldLeft(vm1)((vm, ent) => ent match {
              case (v, sctx, tctx, v_lbl) => vm.get(v) match {
                case None => vm
                case Some(m) => vm + (v -> (m + (tctx -> (sctx, v_lbl))))
              }}) + ((paramIdtoName(param)) -> Map(catch_tctx -> ((ctx, paramIdtoName(param)))))), 
              tctx1p, aenv1, Nil, benv1, cenv1, nestedDecls1, methInvs1, srcLblEnv1)
            }
          _           <- put(stCatchIn)
          catch_stmts <- kBlock(catch_blk, catch_ctx)
          _          <- removeVarFromVM(paramIdtoName(param))
          stCatchOut <- get
          
          tctx3 <- m.pure(putTCtx(tctx, TTryPostPhi))
          lbl3  <- toLbl(tctx3)

          phis_post <- mkPhi(stTryOut, stCatchOut, lbl3)
          _         <- extendAllVarsWithContextAndLabel(ctx, tctx3, lbl3)
          _         <- setOkCtx(tctx)
          _         <- eenvFromState(st).traverse( ctx => addToEEnv(ctx))
        } yield SSABlock(lbl, List(SSATry(try_stmts, phis_peri, param, catch_stmts, phis_post)))
      } */
      case Try(_, Nil, _) => m.raiseError("SSA construction failed, there is no catch in a try statement")
      case Try(_, _::_, _) => m.raiseError("SSA construction failed, Multiple catch clauses encountered, which should have been merged.")
      case While(exp, stmt) => for {
        _  <- addToAEnv(tctx)
        // lbl <- toLbl(tctx)
        st  <- get
        lbl0  <- m.pure(okCtxFromState(st))
        tctx_pre0 <- m.pure(putTCtx(tctx, TWhileJoin(0)))
        tctx_pre1 <- m.pure(putTCtx(tctx, TWhileJoin(1)))
        // lbl1_0  <- m.pure(tctx_pre0) // not in used, we use option 2
        lbl1  <- m.pure(tctx_pre1)

        phis_pre  <- st match { // phi_bar
          case State(vm0, eCtx0, aenv0, eenv0,  nestedDecls0, methInvs0, srcLblEnv0, conf0) 
          if (eenv0.contains(eCtx0)) => // is this still possible? it means the while statement is dead code
            vm0.keySet.toList.traverse( v => for {
              v_lbl <- mkName(v, lbl1)
            } yield Phi(v, v_lbl, List()))
          case State(vm0, eCtx0, aenv0, eenv0, nestedDecls0, methInvs0, srcLblEnv0, conf0) => 
            vm0.keySet.toList.traverse( v => for {
              v_lbl <- mkName(v, lbl1)
              rhs <- Rleq(aenv0, eenv0, eCtx0, vm0, v) match {
                case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub for the operand of ${v} in the phi-asssignment in the while stmt conversion. None found.")
                case (c,n)::Nil => m.pure(List((lbl0 -> n)))
                case _::_ => m.raiseError(s"SSA construction failed, Rleq failed to find a lub for the operand of ${v} in the phi-asssignment during the while stmt conversion. More than one candidates found.")
              }
            } yield Phi(v, v_lbl, rhs))
        }

        stBodyIn <- st match { // creating vm1
          case State(vm0, eCtx0, aenv0, eenv0, nestedDecls0, methInvs0, srcLblEnv0, conf0) => for {
            entries <- vm0.keySet.toList.traverse( v => for {
              v_lbl <- mkName(v, lbl1)
            } yield (v, tctx_pre0, ctx, v_lbl))
          } yield State(entries.foldLeft(vm0)((vm1, ent) => ent match {
            case (v, tctx2, sctx, v_lbl) => vm1.get(v) match {
              case None => vm1
              case Some(m) => vm1 + (v -> (m + (tctx2  -> v_lbl)))
            }}), tctx_pre0, aenv0, eenv0,  nestedDecls0, methInvs0, srcLblEnv0, conf0)
        }
        _ <- put(stBodyIn)
        exp1 <- kexp(exp, tctx)
        body_ctx <- m.pure(putSCtx(ctx, SWhile(SBox)))
        body_stmts <- kstmtBlock(stmt, body_ctx)
        stBodyOut <- get

        phis_pre_updated <- (st, stBodyIn, stBodyOut) match { // phi_bar'
          // todo check the case for break and continue
          // commented as we don't handle continue and break here
          // case (State(vm0, eCtx0, aenv0, eenv0, nestedDecls0, methInvs0, srcLblEnv0, conf0),
          //     State(vm1, eCtx1, aenv1, eenv1, nestedDecls1, methInvs1, srcLblEnv1, conf1), 
          //     State(vm2, eCtx2, aenv2, eenv2,  nestedDecls2, methInvs2, srcLblEnv2, conf2)) if (eenv2.contains(eCtx2)) => for { // why eenv2 contains eCtx2? something wrong here.
          //  phis_pre2 <- phis_pre.traverse( phi => updatePhiFromCEnv(phi, stBodyOut, tctx)) // we don't need to set the lower bound for the Ctx, since we have 0 bit set in the vm environment, but what about the nested one?
          // } yield phis_pre2
          // the third case from the tech report.
          case (State(vm0, eCtx0, aenv0, eenv0, nestedDecls0, methInvs0, srcLblEnv0, conf0),
                State(vm1, eCtx1, aenv1, eenv1, nestedDecls1, methInvs1, srcLblEnv1, conf1),
                State(vm2, eCtx2, aenv2, eenv2, nestedDecls2, methInvs2, srcLblEnv2, conf2)) => {
            val vs = diffVarMap(vm2,vm1).keySet // dom3(vm2- vm1) // TODO: do we need to exclude those in benv and cenv?
            for {
              phis <- vs.toList.traverse (v => for { 
                v_lbl <- mkName(v, lbl1)
                lbl0  <- m.pure(eCtx0)
                lbl2  <- m.pure(eCtx2)
                name0 <- Rleq(aenv0, eenv0, eCtx0, vm0, v) match {
                  case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub for the first operand of ${v} in the updated phi-asssignment during the while stmt conversion.")
                  case (c,n)::Nil => m.pure(n)
                  case _::_ => m.raiseError(s"SSA construction failed, Rleq failed to find a lub for the first operand of ${v} in the updated phi-asssignment during the while stmt conversion. More than one candidates found.")          
                }
                name2 <- Rleq(aenv2, eenv2, eCtx2, vm2, v) match {
                  case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub for the second operand of ${v} in the updated  phi-asssignment during the while stmt conversion.")
                  case (c,n)::Nil => m.pure(n)
                  case _::_ => m.raiseError(s"SSA construction failed, Rleq failed to find a lub for the second operand of ${v} in the updated phi-asssignment during the while stmt conversion. More than one candidates found.")            
                }
              } yield Phi(v, v_lbl, List(lbl0 -> name0, lbl2 -> name2)))
              // phis2 <- phis.traverse( phi => updatePhiFromCEnv(phi, stBodyOut, tctx))
            } yield phis //  phis2
          }
        }
        subst <- mkSubstFromStates(st, stBodyIn, stBodyOut, lbl1) // theta

        body_stmts_s <- m.pure(snOps.appSubst(subst, body_stmts)) // theta(B)
        exp1_s <- m.pure(snOps.appSubst(subst, exp1)) // theta(E)


        // vm3 // todo what about cenv and benv and eenv
        vm3 <- (st, stBodyIn, stBodyOut) match { // vm3 
            case (State(vm0, eCtx0, aenv0, eenv0, nestedDecls0, methInvs0, srcLblEnv0, conf0),
                State(vm1, eCtx1, aenv1, eenv1,  nestedDecls1, methInvs1, srcLblEnv1, conf1),
                State(vm2, eCtx2, aenv2, eenv2,  nestedDecls2, methInvs2, srcLblEnv2, conf2)) => {
                  val vs = (diffVarMap(vm2,vm1)).keySet // dom3(vm2 - vm1)
                  for {
                    entries <- vs.toList.traverse( v => for {
                      v_lbl <- mkName(v, lbl1)
                    } yield (v, tctx_pre1, ctx, v_lbl))
                  } yield entries.foldLeft(vm0 ++ diffVarMap(vm2,vm1))((vm3, ent) => ent match {
                      case (v, tctx11, sctx, v_lbl) => vm3.get(v) match {
                        case None => vm3
                        case Some(m) => vm3 + (v -> (m + (tctx11  -> v_lbl)))
                      }
                  }) 
                }
        }
        /*
        _ <- m.pure(println("hello"))
        _ <- m.pure(println(vm3))
        */
      } yield SSABlock(lbl, List(SSAWhile(phis_pre_updated, exp1_s, body_stmts_s)))
    }
  } 




  /**
    * mkPhi - create phis by merging all variables found in vmap in st1 and st2, 
    *
    * @param st0 - based state 0
    * @param st1 - incoming state 1
    * @param st2 - incoming state 2
    * @param lbl - phi LHS variable label
    * @param m   - monad type class context
    * @return
    */

  def mkPhi(st0:State, st1:State, st2:State, lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State, List[Phi]] = (st0, st1, st2) match {
    /*
    case (State(vm1, eCtx1, aenv1, eenv1, benv1, cenv1,  _, _, _), State(vm2, eCtx2, aenv2, eenv2, benv2, cenv2, _, _,_)) if ((eenv1 ++ dom(benv1 ++ cenv1)).contains(eCtx1)) && ((eenv2 ++ dom(benv2 ++ cenv2)).contains(eCtx2)) => for {
      // do we still need this case? it means dead code
      vs <- m.pure(vm1.keySet ++ vm2.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
      } yield Phi(v, vlbl, Map()))
    } yield phis
    case (State(vm1, eCtx1, aenv1, eenv1, benv1, cenv1, _, _,_), State(vm2, eCtx2, aenv2, eenv2, benv2, cenv2, _, _,_)) if !((eenv1 ++ dom(benv1 ++ cenv1)).contains(eCtx1)) && ((eenv2 ++ dom(benv2 ++ cenv2)).contains(eCtx2)) => for {
      vs <- m.pure(vm1.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbl1 <- toLbl(eCtx1)
        name <- Rleq(aenv1, eenv1, benv1, cenv1, eCtx1, vm1, v) match {
          case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub in mkPhi() for ${v}. None exists. ${vm1}")
          case (c,n)::Nil => m.pure(n)
          case _::_ => m.raiseError("SSA construction failed, Rleq failed to find a lub in mkPhi(). More than one candidates found.")
        }
      } yield Phi(v, vlbl, Map(lbl1 -> name)))  
    } yield phis
    case (State(vm1, eCtx1, aenv1, eenv1, benv1, cenv1, _, _, _), State(vm2, eCtx2, aenv2, eenv2, benv2, cenv2,_, _, _)) if ((eenv1 ++ dom(benv1 ++ cenv1)).contains(eCtx1)) && !((eenv2 ++ dom(benv2 ++ cenv2)).contains(eCtx2)) => for {
      vs <- m.pure(vm2.keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbl2 <- toLbl(eCtx2)
        name <- Rleq(aenv2, eenv2, benv2, cenv2, eCtx2, vm2, v) match {
          case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub in mkPhi() for ${v}. None exists. ${vm2}")
          case (c,n)::Nil => m.pure(n)
          case _::_ => m.raiseError("SSA construction failed, Rleq failed to find a lub in mkPhi(). More than one candidates found.")          
        }
      } yield Phi(v, vlbl, Map(lbl2 -> name)))
    } yield phis
    */  
    case (State(vm0, eCtx0, aenv0, eenv0, _, _, _, _), State(vm1, eCtx1, aenv1, eenv1, _, _, _,_ ), State(vm2, eCtx2, aenv2, eenv2,_, _, _, _)) => for {
      vs <- m.pure((diffVarMap(vm1,vm0) ++ diffVarMap(vm2,vm0)).keySet)
      phis <- vs.toList.traverse( v => for {
        vlbl <- mkName(v, lbl)
        lbl1 <- m.pure(eCtx1)
        lbl2 <- m.pure(eCtx2)
        name1 <- Rleq(aenv1, eenv1, eCtx1, vm1, v) match {
          case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub in mkPhi() for ${v}. None exists. ${vm1}")
          case (c,n)::Nil => m.pure(n)
          case _::_ => m.raiseError("SSA construction failed, Rleq failed to find a lub in mkPhi(). More than one candidates found.")          
        }
        name2 <- Rleq(aenv2, eenv2, eCtx2, vm2, v) match {
          case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub in mkPhi() for ${v}. None exists. ${vm2}")
          case (c,n)::Nil => m.pure(n)
          case _::_ => m.raiseError("SSA construction failed, Rleq failed to find a lub in mkPhi(). More than one candidates found.")            
        }
      } yield Phi(v, vlbl, List(lbl1 -> name1, lbl2 -> name2)))
    } yield phis  
  }



  /**
    * genVarDecls - generate the list of statements from the nested declaration collected during the conversion.
    *
    * @param m
    * @return
    */    
  def genVarDecls(using m:MonadError[SSAState, ErrorM]):SState[State, List[SSAStmt]] = for {
    st <- get
    stmts <- st match {
      case State(vm, eCtx, aenv, eenv, nestedDecls, methodInvs, srcLblEnv, conf) => for {
        tbl <- m.pure(mkTable(nestedDecls)) 
        ll  <- vm.toList.traverse( p => p match {
          case (x, ctxm) => tbl.get(x) match {
            case None => m.pure(Nil:List[SSAStmt])
            case Some((ty,mods)) => { 
              def go(q:(TCtx, (SCtx, Name))):SState[State, SSAStmt] = q match {
                case (tctx, (ctx, xlbl@Name(List(id)))) => {
                  m.pure(SSAVarDecls(mods, ty, List(VarDecl(VarId(id), None))))
                }
                case (tctx, (ctx, xlbl)) => {
                  m.raiseError("SSA construction failed, genVarDecls - the renamed variable is a not a simple id.")
                }
              }
              ctxm.toList.traverse(go(_)) // TODO should sort it first so that the list is more deterministics for testing?
            }
          }
        })
      } yield ll.flatMap( p => p)
    }
  } yield stmts

  /**
    * prepand variable declaration to the current list of blocks
    *
    * @param lbl
    * @param vDecls
    * @param blocks
    * @return
    */
  def prependVarDecls(lbl:Label, vDecls:List[SSAStmt], blocks:List[SSABlock]):List[SSABlock] = blocks match {
    case Nil => List(SSABlock(lbl, vDecls))
    case (SSABlock(lbl, stmts))::bs => SSABlock(lbl, vDecls ++ stmts)::bs
  }

  /**
    * Turn the list of nested declaration into a hash map (name -> (Type, [Modifier]))
    *
    * @param nestedDecls
    * @return
    */
  def mkTable(nestedDecls: List[(TCtx, Ident, Type, List[Modifier])]):Map[Name, (Type, List[Modifier])] = 
    nestedDecls.foldLeft(Map():Map[Name, (Type, List[Modifier])])( (m, ndecl) => ndecl match {
      case (_, id, ty, mods) => m + (Name(List(id)) -> (ty,mods))
    })

  /**
      * paramIdtoName - turn a formal param into a Name
      *
      * @param fp
      * @return
      */
  def paramIdtoName(fp:FormalParam):Name = fp match {
    case FormalParam(mods, ty, arity, var_decl_id) => Name(List(idFromVarDeclId(var_decl_id)))
  }


  /**
    * kctx - converts a source program context into a target program context
    *
    * @param ctx
    * @return
    */

  def kctx(ctx:SCtx):TCtx = ctx match {
    case SBox => TBox
    case SLast(ctx1) => TLast(kctx(ctx1))
    case SHead(ctx1) => THead(kctx(ctx1))
    case STail(ctx1) => TTail(kctx(ctx1))
    case SThen(ctx1) => TThen(kctx(ctx1))
    case SElse(ctx1) => TElse(kctx(ctx1))
    case SWhile(ctx1) => TWhile(kctx(ctx1))
    case STry(ctx1) => TTry(kctx(ctx1))
    case SCatch(ctx1) => TCatch(kctx(ctx1))
  } 

  /**
    * mkId - create a new ID from an existing id and a label
    *
    * @param id
    * @param lbl
    * @param m
    * @return
    */
  def mkId(id:Ident, lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State, Ident] = for {
    s <- lblToStr(lbl)    
  } yield(appIdStr(id,s))


  // exclude entries based on a value in the codomain

  def codexclude[A,B](m:List[(A,Option[B])],v:B):List[(A,Option[B])] = m.flatMap{
    case (a,None) => List((a,None))
    case (a,Some(b)) if b == v => Nil
    case (a,Some(b)) => List((a,Some(b)))
  }


  /**
    * make substitution for while conversion
    *
    * @param st0 - input state to the entire statement
    * @param st1 - input state to the while body
    * @param st2 - output state from the while body
    * @param lbl1 - preWhilePhi^0 
    * @param m
    * @return
    */

  def mkSubstFromStates(st0:State, st1:State, st2:State, lbl1:Label)(using m:MonadError[SSAState,ErrorM]):SState[State,Map[Name,Name]] = {
    
    def go(vs:List[Name], aenv:AEnv, eenv:EEnv, eCtx:TCtx, vm:VarMap, lbl:Label)(using m:MonadError[SSAState, ErrorM]):SState[State,List[(Name,Name)]] = vs.traverse(v => for {
          v_lbl1 <- mkName(v,lbl) // the v_l1 to be renamed back to the original
          v_ori <- Rleq(aenv, eenv, eCtx, vm, v) match {
            case Nil => m.raiseError(s"SSA construction failed, Rleq failed to find a lub during the while stmt conversion.")
            case (c,n)::Nil => m.pure(n)
            case _::_ => m.raiseError("SSA construction failed, Rleq failed to find a lub during the while stmt conversion. More than one candidates found.")          
          }
        } yield (v_lbl1, v_ori))
          
    (st0, st1, st2) match { 
    // todo check the case for break and continue // no need?
    // 
      case (State(vm0, eCtx0, aenv0, eenv0, nestedDecls0, methInvs0, srcLblEnv0, conf0),
            State(vm1, eCtx1, aenv1, eenv1, nestedDecls1, methInvs1, srcLblEnv1, conf1),
            State(vm2, eCtx2, aenv2, eenv2, nestedDecls2, methInvs2, srcLblEnv2, conf2)) => {
              val vs = diffVarMap(vm2, vm1).keySet // dom3(vm2 - vm1)
              val no_update = vm0.keySet -- vs
              for {
                ls <- go(no_update.toList, aenv0, eenv0, eCtx0, vm0, lbl1)
              } yield ls.foldLeft(Map():Map[Name,Name])((m:Map[Name,Name],p:(Name,Name)) => (m + (p._1 -> p._2)))
            }
    }
  
  }
    






}
