-module(char_test).

dup_chars(C) ->
  string:chars(C, 100).

test_lists_all() ->
  A = $A,
  Input = [$B, A],
  lists:all(fun(I) ->
                string:chars(I, 12) =:= "amir"
            end, Input).

