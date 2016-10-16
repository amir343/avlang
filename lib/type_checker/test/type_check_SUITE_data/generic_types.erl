-module(generic_types).

my_map :: ([], Any) -> [];
          ([A], (A -> B)) -> [B].
my_map([], _F) ->
  [];
my_map([H|T], F) ->
  [F(H) | my_map(T, F)].


test_my_map() ->
  type F :: (Integer -> Integer),
  F = fun(I) -> I + 1 end,
  my_map([1, 2, 3, 4], F).
