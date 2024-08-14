
public class Test
{
  public static int f ()
  {
    class Ctxt
    {
      int x_3320;
      int x_333327;
      int x_333328310;
      int s_33320;
      int s_333327;
      int s_33332820;
      int x_20;
      int s_320;
      int res;
      Exception ex;
      public static java.util.function.Function<Void, Void> id = e -> { return null; };
      public static java.util.function.Function<Exception, Void> idHandler = e -> { return null; };
      public static java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> loop (java.util.function.Function<Void, Boolean> cond, java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> visitor, java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> exit)
      {
        return raise -> k -> {
          if (cond.apply(null))
          {
            return visitor.apply(raise).apply(n -> loop(cond, visitor, exit).apply(raise).apply(k));
          }
          else
          { return exit.apply(raise).apply(k); }
        };
      }
      public static java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> seq (java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> first, java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> second)
      { return raise -> k -> first.apply(raise).apply(n -> second.apply(raise).apply(k)); }
      public static java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> ifelse (java.util.function.Function<Void, Boolean> cond, java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> th, java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> el)
      {
        return raise -> k -> {
          if (cond.apply(null))
          { return th.apply(raise).apply(k); }
          else
          { return el.apply(raise).apply(k); }
        };
      }
    }
    Ctxt ctxt = new Ctxt()
    ;
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> m_3320 = raise -> k -> {
      ctxt.x_3320 = 0;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> m_33320 = raise -> k -> {
      ctxt.s_33320 = 0;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> mk_33320 = raise -> k -> {
      ctxt.x_333327 = ctxt.x_3320;
      ctxt.s_333327 = ctxt.s_33320;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> m_33332820 = raise -> k -> {
      ctxt.s_33332820 = ctxt.x_333327 + ctxt.s_333327;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> m_333328310 = raise -> k -> {
      ctxt.x_333328310 = ctxt.x_333327 + 1;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> mk_333328310 = raise -> k -> {
      ctxt.x_333327 = ctxt.x_333328310;
      ctxt.s_333327 = ctxt.s_33332820;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Void, Void>, Void>> m_3333310 = raise -> k -> {
      ctxt.res = ctxt.s_33332820;
      return k.apply(null);
    };
    java.util.function.Function<java.util.function.Function<Exception, Void>, java.util.function.Function<java.util.function.Function<Integer, Void>, Void>> f_cps = raise -> k -> Ctxt.seq(m_3320, Ctxt.seq(m_33320, Ctxt.seq(mk_33320, Ctxt.loop(unit -> (ctxt.x_333327 < 10), Ctxt.seq(m_33332820, Ctxt.seq(m_333328310, mk_333328310)), m_3333310)))).apply(raise).apply(unit -> k.apply(ctxt.res));
    f_cps.apply(Ctxt.idHandler).apply(r -> { ctxt.res = r; return null; });
    return ctxt.res;
  }
}