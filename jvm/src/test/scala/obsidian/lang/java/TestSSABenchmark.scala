package obsidian.lang.java

import org.scalatest.{funsuite, matchers}

import obsidian.lang.java.scalangj.Lexer
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.* 
// import obsidian.lang.java.Label._
/*
import obsidian.lang.java.SSADL.*
import obsidian.lang.java.CFG.{CFGError, CFGOk, cfgOps, initStateInfo}
import obsidian.lang.java.SSACFG.*
import obsidian.lang.java.SSABenchmark.*


class SSABenchmark1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR =
    """
      |    public static int fib(int n)
      |    {
      |        int f1;    //0 f1 -> 0
      |        int f2;    //1 f2 -> 0
      |        int i;       //2
      |        int t;
      |        f1 = 1;
      |        f2 = 1;
      |        i = 0;
      |        while(i < n) {   //3 phis: i, 1 -> ((0) -> 0, (3 0 1) -> 2), f1, 1 -> ((0) -> 0, (3 0 1) -> 2), f2, 1 -> ((0) -> 0, (3 0 1) -> 2)
      |            t = f1 + f2; //3 0 0 (t 2)
      |            f1 = f2;         //3 0 1 (f1 2)
      |            f2 = t;          //3 0 2 (f2 2)
      |            i++;             //3 0 3 (i 2)
      |        }
      |        return f2;           //4 (f2 1)
      |    }""".stripMargin
    test("SSABenchmark1") {
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
                                        
                                        val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                                            case CFGError(message) => fail(message)
                                            case CFGOk((st, unit)) => {
                                            // println(st.cfg)
                                                st.cfg
                                            }
                                        }
                                        SSACFG.runSSACFG(d_methodDecl, cfg, List(0)) match {
                                            case SsacfgError(msg) => fail(s"SSA CFG construction failed with ${msg}")
                                            case SsacfgOk(ssacfg) => {
                                                val ssacfgcount = countPhiOps.cntPhi(ssacfg)
                                                val dlcount = countPhiOps.cntPhi(ssa_methodDecl)
                                                println(s"${ssacfgcount} : ${dlcount}")
                                            }
                                        }
                                        

                                    }
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


class SSABenchmark2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR =
    """
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
}""".stripMargin
    test("SSABenchmark2") {
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
                                        
                                        val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                                            case CFGError(message) => fail(message)
                                            case CFGOk((st, unit)) => {
                                            // println(st.cfg)
                                                st.cfg
                                            }
                                        }
                                        SSACFG.runSSACFG(d_methodDecl, cfg, List(0)) match {
                                            case SsacfgError(msg) => fail(s"SSA CFG construction failed with ${msg}")
                                            case SsacfgOk(ssacfg) => {
                                                val ssacfgcount = countPhiOps.cntPhi(ssacfg)
                                                val dlcount = countPhiOps.cntPhi(ssa_methodDecl)
                                                println(s"${ssacfgcount} : ${dlcount}")
                                            }
                                        }
                                        

                                    }
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



class SSABenchmark3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR =
    """
 public static void main(String [] args) {
	int x; 
    int s;
    x = 0;
    s = 0;
    while (x < 10) {
        s = x + s;
    }
}""".stripMargin
    test("SSABenchmark3") {
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
                                        
                                        val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                                            case CFGError(message) => fail(message)
                                            case CFGOk((st, unit)) => {
                                            // println(st.cfg)
                                                st.cfg
                                            }
                                        }
                                        SSACFG.runSSACFG(d_methodDecl, cfg, List(0)) match {
                                            case SsacfgError(msg) => fail(s"SSA CFG construction failed with ${msg}")
                                            case SsacfgOk(ssacfg) => {
                                                val ssacfgcount = countPhiOps.cntPhi(ssacfg)
                                                val dlcount = countPhiOps.cntPhi(ssa_methodDecl)
                                                println(s"${ssacfgcount} : ${dlcount}")
                                            }
                                        }
                                        

                                    }
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

class SSABenchmark4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR =
     """
       |    public static int fun(int n)
       |    {
       |       int error_type; 
       |       error_type = 0;
       |       try {
       |         if (n < 0){
       |           error_type = 1;
       |           throw new Exception();
       |         } else if (n > 100) {
       |           error_type = 2;
       |           throw new Exception();
       |         }
       |       } catch(Exception e){
       |       }
       |       return error_type;
       |    }""".stripMargin
    test("SSABenchmark4") {
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
                                        
                                        val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                                            case CFGError(message) => fail(message)
                                            case CFGOk((st, unit)) => {
                                            // println(st.cfg)
                                                st.cfg
                                            }
                                        }
                                        SSACFG.runSSACFG(d_methodDecl, cfg, List(0)) match {
                                            case SsacfgError(msg) => fail(s"SSA CFG construction failed with ${msg}")
                                            case SsacfgOk(ssacfg) => {
                                                val ssacfgcount = countPhiOps.cntPhi(ssacfg)
                                                val dlcount = countPhiOps.cntPhi(ssa_methodDecl)
                                                println(s"${ssacfgcount} : ${dlcount}")
                                            }
                                        }
                                        

                                    }
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


class SSABenchmark5 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val METHODSTR ="""
                    |    public static int fun(int m)
                    |    {
                    |       int a;
                    |       int n;
                    |       a = 0;
                    |       n = m;
                    |       if (n%2==0) a = 1;
                    |       else if (n%3 == 0) a = 2;
                    |       else a = 3;
                    |       return a;
                    |    }""".stripMargin
    test("SSABenchmark5") {
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
                                        
                                        val cfg = cfgOps.buildCFG(d_methodDecl, List()).run(initStateInfo) match {
                                            case CFGError(message) => fail(message)
                                            case CFGOk((st, unit)) => {
                                            // println(st.cfg)
                                                st.cfg
                                            }
                                        }
                                        SSACFG.runSSACFG(d_methodDecl, cfg, List(0)) match {
                                            case SsacfgError(msg) => fail(s"SSA CFG construction failed with ${msg}")
                                            case SsacfgOk(ssacfg) => {
                                                val ssacfgcount = countPhiOps.cntPhi(ssacfg)
                                                val dlcount = countPhiOps.cntPhi(ssa_methodDecl)
                                                println(s"${ssacfgcount} : ${dlcount}")
                                            }
                                        }
                                        

                                    }
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
*/