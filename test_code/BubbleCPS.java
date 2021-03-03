import java.util.function.*;
import java.util.Map;
import java.util.HashMap;

public class BubbleCPS {
    public static void main(String [] argv) {
		int [] nums = {13, 3, 4, 14, 67, 45, 34};
		bubble(nums);
		for (int i = 0; i < nums.length; i ++) {
			System.out.println(nums[i]);
		}
    }


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

    public static Function<Void, Void> id = (isNull) -> {
        return null;
    };

    public static Function<Exception, Void> idHandler = e -> {
        System.out.println(e.toString());
        return null;
    };


	public static class Ctxt {
		int t_0; // 0
		boolean changed_0 = false;
		int i_0;
		int j_0;
		int i_1, i_1p, i_2 ,i_3, i_3p, i_4, i_8, i_11;
		int j_1, j_1p, j_2, j_3, j_3p, j_4, j_7, j_8;
		int t_1, t_1p, t_3, t_3p, t_5, t_4, t_8;
	    boolean changed_1, changed_1p, changed_2, changed_3, changed_3p, changed_5, changed_4, changed_8;
        HashMap<Integer, Function<Exception,Void>> raise_map;
        HashMap<Integer, Function<Void,Void>> k_map;
        HashMap<Integer, Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>>> func_map;
        public Ctxt() {
            this.raise_map = new HashMap<>();
            this.k_map = new HashMap<>();
            this.func_map = new HashMap<>();
        }
	}



	
    public static void bubble(int [] nums) {

		final Ctxt ctxt = new Ctxt();

		Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble0 = raise -> k -> {
            ctxt.changed_0 = false;
            ctxt.i_0 = nums.length - 1;
            return k.apply(null);
        };

 
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k01 = raise -> k -> {
            ctxt.i_1 = ctxt.i_0;
            ctxt.j_1 = ctxt.j_0;
            ctxt.t_1 = ctxt.t_0;
            ctxt.changed_1 = ctxt.changed_0;
    
            return k.apply(null);
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k11 = raise -> k -> {
            ctxt.i_1p = ctxt.i_11;
            ctxt.j_1p = ctxt.j_8;
            ctxt.changed_1p = ctxt.changed_8;
            ctxt.t_1p = ctxt.t_8;
            return k.apply(null);
        };

                
        // block 2
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble2 = raise -> k -> {
            System.out.println("bubble2");
            ctxt.changed_2 = false;
            ctxt.j_2 = 0;
            return k.apply(null);
        };


        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k23 = raise -> k -> {
            ctxt.i_3 = ctxt.i_1;
            ctxt.j_3 = ctxt.j_2;
            ctxt.t_3 = ctxt.t_1;
            ctxt.changed_3 = ctxt.changed_2;
            return k.apply(null);
        };


        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble5 = raise -> k -> {
            ctxt.t_5 = nums[ctxt.j_3+1];
            nums[ctxt.j_3+1] = nums[ctxt.j_3];
            nums[ctxt.j_3] = ctxt.t_5;
            ctxt.changed_5 = true;
            return k.apply(null); 
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k54 = raise -> k -> {
            ctxt.j_4 = ctxt.j_3;
            ctxt.changed_4 = ctxt.changed_5;
            ctxt.i_4 = ctxt.i_3;
            ctxt.t_4 = ctxt.t_5;
            return k.apply(null);
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble6 = raise -> k -> {
            return k.apply(null); // supposed to be empty
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k64 = raise -> k -> {
            ctxt.j_4 = ctxt.j_3;
            ctxt.changed_4 = ctxt.changed_3;
            ctxt.i_4 = ctxt.i_3;
            ctxt.t_4 = ctxt.t_3;
            return k.apply(null);
        };
        

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble4 = raise -> k -> {
            Function<Void, Boolean> cond = isnull -> new Boolean(nums[ctxt.j_3] > nums[ctxt.j_3+1]);
            return ifelse(cond, seq(bubble5,k54), seq(bubble6, k64)).apply(raise).apply(k);
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble7 = raise -> k -> {
            ctxt.j_7 = ctxt.j_4 + 1;
            return k.apply(null); 
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k73 = raise -> k -> {
            ctxt.i_3 = ctxt.i_4;
            ctxt.j_3 = ctxt.j_7;
            ctxt.t_3 = ctxt.t_4;
	    ctxt.changed_3 = ctxt.changed_4;
            return k.apply(null);
        };


        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k33 = raise -> k -> {
            ctxt.i_3p = ctxt.i_3;
            ctxt.j_3p = ctxt.j_3;
            ctxt.t_3p = ctxt.t_3;
            ctxt.changed_3p = ctxt.changed_3;
            return k.apply(null);
        };

        // k91 is the merge at the end of while loop (block 1)
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k91 = raise -> k -> {
            ctxt.i_1p = ctxt.i_3p;
            ctxt.j_1p = ctxt.i_3p;
            ctxt.changed_1p = ctxt.changed_3p;
            ctxt.t_1p = ctxt.t_3p;
            return k.apply(null);
        };
        // block 9 is a break statement
        // PROBLEM, bubble12 will continue with the current continuation
        // we need to save the continuations at the point of the loop is first entered.
        /*
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble9 = raise -> k -> {
            return bubble12.apply(raise).apply(k); 
        }
        */
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble9 = raise -> k -> {
            // we look up the continuation context;
            Function<Exception,Void> raise1 = ctxt.raise_map.get(1);
            Function<Void,Void> k1 = ctxt.k_map.get(1); 
            Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> exp = ctxt.func_map.get(1);
            System.out.println("bubble9");
            return exp.apply(raise1).apply(k1); 
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble10 = raise -> k -> {
            return k.apply(null);
        };

        // phi resolution from 10 to 8
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k108 = raise -> k -> {
            ctxt.changed_8 = ctxt.changed_3p;
            ctxt.i_8 = ctxt.i_3p;
            ctxt.j_8 = ctxt.j_3p;
            ctxt.t_8 = ctxt.t_3p;
            return k.apply(null);
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble8 = raise -> k -> {
            Function<Void, Boolean> cond = isnull -> new Boolean(!(ctxt.changed_3p));
            // Function<Void, Boolean> cond = isnull -> new Boolean(false); 

            return ifelse(cond, seq(k91, bubble9), seq(bubble10, k108)).apply(raise).apply(k);
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble11 = raise -> k -> {
            ctxt.i_11 = ctxt.i_8 - 1;
            return k.apply(null);
        };

        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> k111 = raise -> k -> {
            ctxt.i_1 = ctxt.i_11;
            ctxt.j_1 = ctxt.j_8;
            ctxt.changed_1 = ctxt.changed_8;
            ctxt.t_1 = ctxt.t_8;
            return k.apply(null);
        };

        // named loop in block 3
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> loop3 = raise -> k -> {
            Function<Void, Boolean> cond = isnull -> new Boolean(ctxt.j_3 < ctxt.i_3);
            return loop(cond, seq(bubble4,seq(bubble7,k73)), seq(k33,seq(bubble8, seq(bubble11, k111)))).apply(raise).apply(k);
        };

        // block 3
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble3 = raise -> k -> {
            return seq(k23,loop3).apply(raise).apply(k);
        };
        

                
        // block 12
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble12 = raise -> k -> {
            return k.apply(null);
        };
        
        // named loop in block 1
        Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> loop1 = raise -> k -> {
            
            ctxt.raise_map.put(1, raise);
            ctxt.k_map.put(1,k);
            ctxt.func_map.put(1,bubble12);
            Function<Void, Boolean> cond = isnull -> new Boolean(ctxt.i_1 > 0);
            return loop(cond, seq(bubble2, bubble3), seq(k11, bubble12)).apply(raise).apply(k);
        };

        // block 1
		Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble1 = raise -> k -> {
            System.out.println("bubble1");
            return seq(k01, loop1).apply(raise).apply(k);
        };


        
        seq(bubble0,bubble1).apply(idHandler).apply(id);

    }

}
