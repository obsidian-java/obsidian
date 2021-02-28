import java.util.function.*;

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

	public static class Ctxt {
		int t_0; // 0
		boolean changed_0 = false;
		int i_0;
		int j_0;
		int i_1, i_1p, i_2 ,i_3, i_3p, i_4, i_8, i_11;
		int j_1, j_1p, j_2, j_3, j_3p, j_4, j_7, j_8;
		int t_1, t_1p, t_3, t_3p, t_5, t_4, t_8;
		boolean changed_1, changed_1p, changed_3, changed_3p, changed_5, changed_4, changed_8;
	}


	
    public static void bubble(int [] nums) {

		final Ctxt ctxt = new Ctxt();

		Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble0 = raise -> k -> {
            ctxt.changed_0 = false;
            ctxt.i_0 = nums.length - 1;

            ctxt.i_1 = ctxt.i_0;
            ctxt.j_1 = ctxt.j_0;
            ctxt.t_1 = ctxt.t_0;
            ctxt.changed_1 = ctxt.changed_0;
    
            return k.apply(null);
        };

        

		Function<Function<Exception,Void>, Function<Function<Void, Void>, Void>> bubble1 = raise -> k -> {
            
        };

    }

}
