# obsidian
Control flow obfuscation for Java code

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

    int get1(int x) {
        int i_1, i_3, i_5, i_6, t_6, res_1, res_7, res9;
     L1:i_1 = this.lpos;
        res_1 = -1;
     L2:try {
         L3:if(x < i_1) {
                L4:throw new InputException();
            } else { 
                L5: join (i_5=phi(L3:i_1, L6:i_6)) while (i_5 < x) {
                     L6: t_6 = this.f1 + this.f2;
                         this.f1 = this.f2;
                         this.f2 = t_6;
                         i_6 = i_5 + 1;
                     }
                L7: this.lpos = i_5;
                     res_7 = this.f2;
            } join (i_3 = phi(L4:i_1, L7:i_5))
        } catch (InputException e) {
            L8:System.out.println("the input should be greater than " + i_1 + ".");
        } join (res_2 = phi(L3:res_7, L8:res_1));
      L9:return res_2; 
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
