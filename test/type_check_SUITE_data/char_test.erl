-module(char_test).

dup_chars(C) ->
  string:chars(C, 100).

test_lists_all() ->
  A = $A,
  Input = [$B, A],
  lists:all(fun(I) ->
                string:chars(I, 12) =:= "amir"
            end, Input).

module_to_action :: (String) -> Char.
module_to_action(Str) ->
  F :: (Char -> Boolean) = fun (C) -> C =/= $_ end,
  [A | _] = lists:dropwhile(F, Str),
  A.
