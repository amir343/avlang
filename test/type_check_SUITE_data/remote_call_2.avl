-module(remote_call_2).

-compile([export_all]).

fun_1() ->
  "Amir Moulavi".

fun_3() ->
  remote_call_1:fun_2("Hello ").

test_super_fun() ->
  element(2, remote_call_1:super_fun(error, [])).






