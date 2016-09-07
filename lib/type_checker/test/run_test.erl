-module(run_test).

-export([run/0]).


run() ->
  TCs = type_check_SUITE:all(),
  run_tests(TCs),
  halt().


run_tests(TCs) ->
  Res =
    lists:foldl(fun(TC, Acc) ->
                    io:format("Running ~p...", [TC]),
                    try apply(type_check_SUITE, TC, []) of
                        _ ->
                        io:format("Ok~n", []),
                        Acc
                    catch
                      Error ->
                        io:format("Failed~n    ~p~n", [Error]),
                        [{error, Error, TC} | Acc]
                    end
                end, [], TCs),
  Failed = length(Res),
  Total = length(TCs),
  Success = Total - Failed,
  io:format("~nTotal: ~p, Failed: ~p, Passed: ~p~n", [Total, Failed, Success]),
  halt().
