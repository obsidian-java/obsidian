
public class
{
  public static void f ()
  {
    Void res;
    Exception ex;
    int x_3320;
    int s_33332810;
    int s_333327;
    int s_333329;
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
      s_333327 = s_33320;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_33332810 = raise -> k -> {
      s_33332810 = x_3320 + s_333327;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> mk_33332810 = raise -> k -> {
      s_333327 = s_33332810;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_3333310 = raise -> k -> {
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> f_cps = raise -> k -> seq(m_3320, seq(m_33320, seq(mk_33320, loop(unit -> unit -> (x_3320 < 10), seq(m_33332810, mk_33332810), m_3333310)))).apply(raise).apply(unit -> k.apply(res));
    f_cps.apply(id_raise).apply(r -> { res = r; return ; });
    return res;
  }
  public static int fib (int n)
  {
    int res;
    Exception ex;
    int f2_33333333283320;
    int f2_3333333327;
    int f2_3333333329;
    int i_333333332833310;
    int i_3333333327;
    int i_3333333329;
    int t_333333332820;
    int t_3333333327;
    int t_3333333329;
    int f1_3333333328320;
    int f1_3333333327;
    int f1_3333333329;
    int f1_20;
    int f2_320;
    int i_3320;
    int t_33320;
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_333320 = raise -> k -> {
      f1_333320 = 1;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_3333320 = raise -> k -> {
      f2_3333320 = 1;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_33333320 = raise -> k -> {
      i_33333320 = 0;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_333333320 = raise -> k -> {
      t_333333320 = 0;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> mk_333333320 = raise -> k -> {
      f2_3333333327 = f2_3333320;
      t_3333333327 = t_333333320;
      i_3333333327 = i_33333320;
      f1_3333333327 = f1_333320;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_333333332820 = raise -> k -> {
      t_333333332820 = f1_3333333327 + f2_3333333327;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_3333333328320 = raise -> k -> {
      f1_3333333328320 = f2_3333333327;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_33333333283320 = raise -> k -> {
      f2_33333333283320 = t_333333332820;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_333333332833310 = raise -> k -> {
      i_333333332833310 = i_3333333327 + 1;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> mk_333333332833310 = raise -> k -> {
      f2_3333333327 = f2_33333333283320;
      t_3333333327 = t_333333332820;
      i_3333333327 = i_333333332833310;
      f1_3333333327 = f1_3333333328320;
      return k();
    };
    Function<Function<Exception, Void>, Function<Function<Void, Void>, Void>> m_33333333310 = raise -> k -> {
      res = f2_3333333329;
      return k();
    };
    Function<Integer, Function<Function<Exception, Void>, Function<Function<Integer, Void>, Void>>> fib_cps = n -> raise -> k -> seq(m_333320, seq(m_3333320, seq(m_33333320, seq(m_333333320, seq(mk_333333320, loop(unit -> unit -> (i_3333333327 < n), seq(m_333333332820, seq(m_3333333328320, seq(m_33333333283320, seq(m_333333332833310, mk_333333332833310)))), m_33333333310)))))).apply(raise).apply(unit -> k.apply(res));
    fib_cps.apply(n).apply(id_raise).apply(r -> { res = r; return ; });
    return res;
  }
  public static void main (String argv)
  { System.out.println(fib(10)); }
}