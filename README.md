# obsidian
Control flow obfuscation for Java code

It is work in progress


# Usage

## Visual Studio Code Plug-In
We have created a VSCode plug-in that transforms highlighted Java code into the obfuscated form. For more information, including how to run the plug-in, click [here](https://github.com/obsidian-java/vscode-obsidian).

## IntelliJ IDEA Plug-In
Alternatively obsidian can be used as an IntelliJ IDEA plug-in to similarily obfuscate highlighed Java code in the editor. For more information, including how to install and run the plug-in, click [here](https://github.com/obsidian-java/intellij-obsidian)

## In-code
Compiled binaries are available in the [binrepo](https://github.com/obsidian-java/binrepo/tree/main/obsidian/lang/java/obsidian_3) repository.

To utilize these binaries as a developer looking to utilize the Obsidian library for code obfuscation, add the following repository to your dependancy file and add the Obsidian library as a dependancy.

You should now be able to import it like any other library and use it in your code.
```java
import obsidian.lang.java.obsidian.Obsidian
```

### Gradle
```groovy
repositories {
    maven {
        url 'https://raw.githubusercontent.com/obsidian-java/binrepo/master/'
    }
}
// ...
dependencies {
    implementation 'obsidian:obsidian:0.0.2'
}
```

### SBT
```scala
ThisBuild / resolvers += "obsidian binary github repo" at "https://raw.githubusercontent.com/obsidian-java/binrepo/master/"

// ...

libraryDependencies += "obsidian.lang.java" %%% "obsidian" % "0.0.2"
```

### Maven
```xml
<repositories>
    <repository>
        <id>obsidian-java</id>
        <url>https://raw.github.com/obsidian-java/binrepo/master</url>
    </repository>
</repositories>
// ...
<dependencies>
    <dependency>
        <groupId>obsidian.lang.java</groupId>
        <artifactId>obsidian</artifactId>
        <version>0.0.2</version>
</dependency>
```

# Development (Setup Guide)
As a developer for obsidian, follow the following instructions to set up your environment.

## Obsidian environment setup 
> builds and publish binaries to binrepo repository for library download
```bash
# Clone binrepo repository to this specific location
cd ~ && mkdir obsidian-java && git clone git@github.com:obsidian-java/binrepo.git

# Clone obsidian repository to any location
cd ~/code && git clone git@github.com:obsidian-java/obsidian.git && cd obsidian

# Compile and publish obsidian library to ~/obsidian-java/binrepo folder
sbt compile && sbt publish

# Check if the library is published
cd ~/obsidian-java/binrepo && git status
```

# TODOS:

1. MinSSA does not handle the following
    1. synchronized method, variable, class, which should be an easy fix. 
    2. try-catch block, this one is hard.

# Documentation

* [Code Obfuscation using CPS](https://github.com/obsidian-java/obsidian/raw/master/pepm21-fixed.pdf)
* [Structured SSA construction](https://github.com/obsidian-java/obsidian/raw/master/ftfjp23-fixed.pdf)

# Example 

Some examples in Java

## Original Code

```java


public class FibExcept {

    public class InputException extends Exception {
        public InputException() {
        };
    }

    private int f1, f2, lpos;

    public FibExcept() {
        f1 = 0;
        f2 = 1;
        lpos = 1;
    }
    public int get(int x) {
        // 1 
        int i = lpos;
        try { // 2
            if (x < i) { // 3
                throw new InputException();
            } else { // 4
                while (i < x) { // 5
                    int t = f1 + f2;
                    f1 = f2;
                    f2 = t;
                    i++;
                }    
            } 
            lpos = i;
            return f2;
        } catch (InputException e) { // 6 
            System.out.println("the input should be greater than " + i + ".");
            return -1;
        }
        // 7
    }

    public static void main(String[] args) {

        FibExcept fs = new FibExcept();
        System.out.println(fs.get(10));
        System.out.println(fs.get(9));
    }

}
```
## Obfuscated code simplified version without phi resolution
```java
import java.util.function.*;

public class FibExceptObf {

    public class InputException extends Exception {
        public InputException() { };
    }

    private int f1;
    private int f2;
    private int lpos;

    public FibExceptObf() {
        f1 = 0;
        f2 = 1;
        lpos = 1;
    }

    public static class Ctxt {
        public int input;
        public int fib_result;
        public Exception ex;
        public int i;
    }

    public int get(int x) {
        final Ctxt ctxt = new Ctxt();

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> fib0 = raise -> k -> {
            ctxt.fib_result = -1;
            ctxt.i = this.lpos;
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib5 = raise -> k -> { 
            int t = this.f1 + this.f2;
            this.f1 = this.f2;
            this.f2 = t;
            ctxt.i = ctxt.i + 1;
            return k.apply(null);
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib7 = raise -> k -> {
            this.lpos = ctxt.i;
            ctxt.fib_result = this.f2;
            return k.apply(null);
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib4 = raise -> {
            return k -> {
                Function<Void, Boolean> cond = isnull -> {
                    return (new Boolean(ctxt.i < ctxt.input));
                };
                return loop(cond, fib5, fib7).apply(raise).apply(k);
            };
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib3 = raise -> k -> {
            return raise.apply(new InputException());
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib2 = raise -> k -> {
            Function<Void, Boolean> cond = isnull -> new Boolean(x < ctxt.i);
            return ifelse(cond, fib3, fib4).apply(raise).apply(k);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib6 = raise -> k -> {
            System.out.println("the input should be greater than " + ctxt.i + ".");
            return k.apply(null);
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> fib1 = raise -> k -> {
            return trycatch(fib2,ex -> { ctxt.ex = ex; return fib6; } ).apply(raise).apply(k);
        };

        
        Function<Integer, Function<Function<Exception,Void>, Function<Function<Integer, Void>, Void>>> get_cps = input -> raise -> k -> {
            ctxt.input = input;
            return seq(fib0,fib1).apply(raise).apply((Void n) -> k.apply(new Integer(ctxt.fib_result)) );
        };

        get_cps.apply(x).apply(idHandler).apply(r -> { ctxt.fib_result = r; return null;});
        return ctxt.fib_result;
    }

    public static Function<Void, Void> id = (isNull) -> {
        return null;
    };

    public static Function<Exception, Void> idHandler = e -> {
        System.out.println(e.toString());
        return null;
    };


    public static Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> 
    loop(Function<Void, Boolean> cond, 
            Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> visitor,
            Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> exit) { 
        return raise -> k -> { 
                if (cond.apply(null)) {
                    return visitor.apply(raise).apply( isNull -> 
                        loop(cond, visitor, exit).apply(raise).apply(k)
                    );
                } else {
                    return exit.apply(raise).apply(k);
                }
            };
        }
    

    
    public static Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> 
    trycatch (Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> visitor,
              Function<Exception, Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>>> handler) {
        return raise -> k -> 
            visitor.apply(ex -> handler.apply(ex).apply(raise).apply(k)).apply(k);
    
    }

    public static Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>> 
    seq (Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> first,
        Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> second) {
        return raise -> k -> 
            first.apply(raise).apply(isnull -> second.apply(raise).apply(k));
    }


    public static Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>>
    ifelse (Function<Void, Boolean> cond, 
    Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>> th, 
    Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>> el) {
        return raise -> k -> {
                if (cond.apply(null)) {
                    return th.apply(raise).apply(k);
                } else {
                    return el.apply(raise).apply(k);
                }
        };
    }

    public static void main(String[] args) {

        FibExceptObf fs = new FibExceptObf();
        System.out.println(fs.get(10));
        System.out.println(fs.get(9));
    }
}


```

## Obfuscated code full version with phi resolution

```java
import java.util.function.*;

public class FibExceptObfFull {

    public class InputException extends Exception {
        public InputException() { };
    }

    private int f1;
    private int f2;
    private int lpos;

    public FibExceptObfFull() {
        f1 = 0;
        f2 = 1;
        lpos = 1;
    }

    public static class Ctxt {
        public int input;
        public int get_result;
        public Exception ex;
        public int i_1, i_2, i_5, i_6, t_6, r_1, r_2, r_7;
    }

    public int get(int x) {
        final Ctxt ctxt = new Ctxt();

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> get1 = raise -> k -> {
            ctxt.r_1 = -1;
            ctxt.i_1 = this.lpos;
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get6 = raise -> k -> { 
            ctxt.t_6 = this.f1 + this.f2;
            this.f1 = this.f2;
            this.f2 = ctxt.t_6;
            ctxt.i_6 = ctxt.i_5 + 1;
            return k.apply(null);
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get7 = raise -> k -> {
            this.lpos = ctxt.i_5;
            ctxt.r_7 = this.f2;
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> getk7 = raise -> k -> {
            ctxt.i_2 = ctxt.i_5;
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> getk6 = raise -> k -> {
            ctxt.i_5 = ctxt.i_6;
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get5 = raise -> k -> {
            Function<Void, Boolean> cond = isnull -> {
                return (new Boolean(ctxt.i_5 < ctxt.input));
            };
            return loop(cond, seq(get6,getk6), seq(get7,getk7)).apply(raise).apply(k);
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get4 = raise -> k -> {
            ctxt.i_2 = ctxt.i_1;
            return raise.apply(new InputException());
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> getk3b = raise -> k -> {
            ctxt.i_5 = ctxt.i_1;
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get3 = raise -> k -> {
            Function<Void, Boolean> cond = isnull -> new Boolean(ctxt.input < ctxt.i_1);
            return ifelse(cond, get4, seq(getk3b, get5)).apply(raise).apply(k);
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> getk3a = raise -> k -> {
            ctxt.r_2 = ctxt.r_7; return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get8 = raise -> k -> {
            System.out.println("the input should be greater than " + ctxt.i_2 + ".");
            return k.apply(null);
        };

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> getk8 = raise -> k -> {
            ctxt.r_2 = ctxt.r_1; 
            return k.apply(null);
        };
        

        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get9 = raise -> k -> {
            ctxt.get_result = ctxt.r_2; return k.apply(null); 
        };
        Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> get2 = raise -> k -> {
            return seq(trycatch(seq(get3,getk3a),ex -> { ctxt.ex = ex; return seq(get8,getk8); }),get9).apply(raise).apply(k);
        };

        
        Function<Integer, Function<Function<Exception,Void>, Function<Function<Integer, Void>, Void>>> get_cps = y -> raise -> k -> {
            ctxt.input = y;
            return seq(get1,get2).apply(raise).apply((Void n) -> k.apply(new Integer(ctxt.get_result)) );
        };

        get_cps.apply(x).apply(idHandler).apply(r -> { ctxt.get_result = r; return null;});
        return ctxt.get_result;
    }

    public static Function<Void, Void> id = (isNull) -> {
        return null;
    };

    public static Function<Exception, Void> idHandler = e -> {
        System.out.println(e.toString());
        return null;
    };


    public static Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> 
    loop(Function<Void, Boolean> cond, 
            Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> visitor,
            Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> exit) { 
        return raise -> k -> { 
                if (cond.apply(null)) {
                    return visitor.apply(raise).apply( isNull -> 
                        loop(cond, visitor, exit).apply(raise).apply(k)
                    );
                } else {
                    return exit.apply(raise).apply(k);
                }
            };
        }
    

    
    public static Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> 
    trycatch (Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> visitor,
              Function<Exception, Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>>> handler) {
        return raise -> k -> 
            visitor.apply(ex -> handler.apply(ex).apply(raise).apply(k)).apply(k);
    
    }

    public static Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>> 
    seq (Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> first,
        Function<Function<Exception, Void>, Function<Function<Void,Void>, Void>> second) {
        return raise -> k -> 
            first.apply(raise).apply(isnull -> second.apply(raise).apply(k));
    }


    public static Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>>
    ifelse (Function<Void, Boolean> cond, 
    Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>> th, 
    Function<Function<Exception,Void>, Function<Function<Void,Void>, Void>> el) {
        return raise -> k -> {
                if (cond.apply(null)) {
                    return th.apply(raise).apply(k);
                } else {
                    return el.apply(raise).apply(k);
                }
        };
    }

    public static void main(String[] args) {

        FibExceptObfFull fs = new FibExceptObfFull();
        System.out.println(fs.get(10));
        System.out.println(fs.get(9));
    }
}
```
