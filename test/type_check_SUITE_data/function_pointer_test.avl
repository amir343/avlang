-module(function_pointer_test).


fun_1 :: ((Integer -> Integer), Integer) -> Integer.
fun_1(F, A) ->
  F(A).


fun_2(A) ->
  A + 1.

fun_3() ->
  fun_1(fun fun_2/1, 123).
