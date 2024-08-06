
public class
{
  public static int f ()
  {
    int res;
    Exception ex;
    int x_3320;
    int x_333327;
    int x_333328310;
    int s_33320;
    int s_333327;
    int s_33332820;
    int x_20;
    int s_320;
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_3320 = raise -> k -> {
      x_3320 = 0;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_33320 = raise -> k -> {
      s_33320 = 0;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> mk_33320 = raise -> k -> {
      x_333327 = x_3320;
      s_333327 = s_33320;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_33332820 = raise -> k -> {
      s_33332820 = x_333327 + s_333327;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_333328310 = raise -> k -> {
      x_333328310 = x_333327 + 1;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> mk_333328310 = raise -> k -> {
      x_333327 = x_333328310;
      s_333327 = s_33332820;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_3333310 = raise -> k -> {
      res = s_33332820;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Integer, Void>, Void>> f_cps = raise -> k -> seq(m_3320, seq(m_33320, seq(mk_33320, loop(unit -> unit -> (x_333327 < 10), seq(m_33332820, seq(m_333328310, mk_333328310)), m_3333310)))).apply(raise).apply(unit -> k.apply(res));
    f_cps.apply(id_raise).apply(r -> { res = r; return ; });
    return res;
  }
}