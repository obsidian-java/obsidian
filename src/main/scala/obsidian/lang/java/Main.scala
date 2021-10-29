package obsidian.lang.java

import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Pretty._
import com.github.luzhuomi.scalangj.Syntax._


object Main extends App {
    val STRING = """
public class Fib
{
    public static int fib(int n)
    {
	int f1 = 1;
	int f2 = 1;
	int i=0;
	while(i<n) {
	    int t = f1 + f2;
	    f1 = f2;
	    f2 = t;
	    i++;
	}
	return f2;
    }

    public static void  main(String argv[]) {
	System.out.println(fib(10));
    }
}
    """
    def run(cu:CompilationUnit):CompilationUnit = cu // TODO

    val eCU = parseCompilationUnit(STRING)
    eCU match {
        case Left(error_msg) => println(error_msg)
        case Right(cu) => println(prettyPrint(run(cu))) 
    }
}
