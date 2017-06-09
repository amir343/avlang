-module(remote_call_1).

-compile([export_all]).

fun_2(A) ->
  A ++ remote_call_2:fun_1().


super_fun :: (ok, []) -> [];
             (error, []) -> {error, not_found}.
super_fun(ok, []) ->
  [];
super_fun(error, []) ->
  {error, not_found}.
