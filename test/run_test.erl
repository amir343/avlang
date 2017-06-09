-module(run_test).

-export([run/0]).

suites() ->
  [type_check_SUITE].


run() ->
  SuitesTCs = [{S, S:all()} || S <- suites()],
  {T, F, S} =
    lists:foldl(
      fun({Suite, TCs}, {Total, Failed, Success}) ->
          io:format("~n", []),
          {T, F, S} = run_tests(Suite, TCs),
          {Total + T, Failed + F, Success + S}
      end, {0, 0, 0}, SuitesTCs),
  io:format("~nTotal: ~p, Failed: ~p, Passed: ~p~n", [T, F, S]),
  halt().


run_tests(Suite, TCs) ->
  Res =
    lists:foldl(
      fun(TC, Acc) ->
          io:format("Running ~p:~p...", [Suite, TC]),
          try apply(type_check_SUITE, TC, []) of
              _ ->
              io:format("Ok~n", []),
              Acc
          catch
            E:T ->
              io:format("Failed~n\t~p:~p~n", [E, T]),
              [{error, TC} | Acc]
          end
      end, [], TCs),
  Failed = length(Res),
  Total = length(TCs),
  Success = Total - Failed,
  {Total, Failed, Success}.
