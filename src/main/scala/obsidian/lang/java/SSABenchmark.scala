package obsidian.lang.java

// TODO: should benchmark MinSSA with SSA
/*
import obsidian.lang.java._ 
import obsidian.lang.java.Label._
import obsidian.lang.java.SSADL._
import obsidian.lang.java.SSACFG._

object SSABenchmark {
  trait CountPhi[A] {
    def cntPhi(a:A):Int
  }

  object countPhiOps {
    def cntPhi[A](a:A)(using cp:CountPhi[A]):Int = cp.cntPhi(a)
  }

  given declCntPhi:CountPhi[SSAMethodDecl] = new CountPhi[SSAMethodDecl] {
    override def cntPhi(a:SSAMethodDecl):Int = a match {
      case SSAMethodDecl(mods, ty_params, ty, id, fps, ex_types, exp, body) => 
        countPhiOps.cntPhi(body)
    }
  }

  given bodyCntphi:CountPhi[SSAMethodBody] = new CountPhi[SSAMethodBody] {
    override def cntPhi(a:SSAMethodBody):Int = a match {
      case SSAMethodBody(blocks) => blocks.map(countPhiOps.cntPhi(_)).sum
    }
  }

  given blockCntPhi:CountPhi[SSABlock] = new CountPhi[SSABlock] {
    override def cntPhi(a:SSABlock):Int = a match {
      case SSABlock(lbl, stmts) => stmts.map(countPhiOps.cntPhi(_)).sum
    }
  }

  given stmtCntPhi:CountPhi[SSAStmt] = new CountPhi[SSAStmt] {
    override def cntPhi(a:SSAStmt):Int = a match {
      case SSAVarDecls(mods, ty, varDecls) => 0
      case SSAAssert(exp, msg) => 0
      case SSAAssignments(stmts) => 0
      case SSAExps(stmts) => 0
      case SSAReturn(_) => 0
      case SSAThrow(_) => 0
      case SSABreak(_) => 0
      case SSAContinue(_) => 0
      case SSAEmpty => 0
      case SSATry(tryStmts, phiCatch, param, catchStmts, phiFinally) => phiCatch.size + phiFinally.size + tryStmts.map(countPhiOps.cntPhi(_)).sum + catchStmts.map(countPhiOps.cntPhi(_)).sum  
      case SSAWhile(phiEntr, exp, stmts, phiExit) => phiEntr.size + phiExit.size + stmts.map(countPhiOps.cntPhi(_)).sum
      case SSAIf(e, thenStmts, elseStmts, phiExit) => phiExit.size + thenStmts.map(countPhiOps.cntPhi(_)).sum + elseStmts.map(countPhiOps.cntPhi(_)).sum
    }
  }

  given cfgCntPhi:CountPhi[SSACFG] = new CountPhi[SSACFG] {
      override def cntPhi(a:SSACFG):Int = a.values.map(countPhiOps.cntPhi(_)).sum
  }

  given nodeCntPhi:CountPhi[SSANode] = new CountPhi[SSANode] {
      override def cntPhi(a:SSANode):Int = a match {
          case AssignmentsNode(id, stmts, localDecls, lVars, rVars, preds, succs, rhsVarReplacements, lhsVarReplacements) => 0
          case IfThenElseNode(id, thenNode, elseNode, lVars, rVars, preds, succs, rhsVarReplacements, joins) => joins.values.map( p => p._2.values.size ).sum
          case SwitchNode(id, caseNodes, lvars, rvars, preds, succs, rhsVarReplacements, joins) => joins.values.map( p => p._2.values.size ).sum
          case WhileNode(id, bodyNode, lvars, rvars, preds, succs, rhsVarReplacements, preJoins, postJoins) => preJoins.values.map( p => p._2.values.size ).sum + postJoins.values.map( p => p._2.values.size ).sum
          case TryCatchFinallyNode(id, tryNode, catchNodes, catchLocalVars, finallyNode, preds, succs, catchJoins, finallyJoins) => catchJoins.values.map( p => p._2.values.size ).sum + finallyJoins.values.map( p => p._2.values.size ).sum
          case ReturnNode(id, lvars, rvars, preds, rhsVarReplacements) => 0
          case ThrowNode(id, lVars, rVars, preds, succs, rhsVarReplacements) => 0
          case AssertNode(id, lVars, rVars, preds, succs, rhsVarReplacements) => 0
          case BreakNode(id, preds, succs) => 0
          case ContNode(id, preds, succs) => 0
          case DefaultNode(id, stmts, preds, succs) => 0
          case CaseNode(id, stmts,preds, succs) => 0
      }
  }
}

*/