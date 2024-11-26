package obsidian.lang.java.obsidian

import scala.io.*
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.obsidian.Obsidian.*

//import os.Path


object Main extends App {
    val STRING = """
public class Test
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

    val eCU = parseNonEmptyCompilationUnit(STRING)
    eCU match {
        case Left(error_msg) => println(error_msg)
        case Right(cu) => {
            //val path:os.Path = os.pwd / "output" / "Test.java"
            println("Original Code:")
            println(prettyPrint(cu))
            println("--------------------------------------")
            println("Obfuscated Code:")
            println(generateObfuscatedCode(cu))
            //os.write(path, prettyPrint(obs_cu)) 
        }
    }
}
