-module(type_syntax_test).

my_fun() ->
  type F :: Float,
  F = 1.0,
  F.


myfun(A, B, C) ->
  T = 22234234234,
  P :: Integer = T * 2 + B,
  S = [1, 2, 3, 4] ++ A,
  type F :: ((ok, Integer, Integer) -> Integer;
               (error, Float, Float) -> Float),
  F = fun(ok, A, B) ->
          list_to_integer(C),
          A + B * P;
         (error, A, B) -> A - B
      end,
  F(ok, P, B).
