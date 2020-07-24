package com.github.luzhuomi.obsidian

import scala.util.parsing.combinator._
import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Lexer._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._


object Main extends App {
    val STRING = """
  public class Fib
{
    public static int fib(int n)
    {
	int f1 = 0;
	int f2 = 1;
	int i=0;
	while(i<n) {
	    int t = f2;
	    f2 = f1 + f2;
	    f1 = t;
	    i++;
	}
	return f2;
    }

    public static void  main(String argv[]) {
	System.out.println(fib(10));
    }
}
    """
    val prCU = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING))

    if (prCU.successful) {
        val prettified = prettyPrint(prCU.get)
        println(prettified)
    } 
}