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


fun_1 :: ([A], (A -> [B])) -> [[B]].
fun_1(A, F) ->
  my_map(A, F).

test_lists_map() ->
  lists:map([1, 2, 3], fun(I) ->
                           I + 1 end).

test_lists_foldl() ->
  lists:foldl(fun(E, Acc) ->
                  [atom_to_list(E)] ++ Acc
              end, [], [a, b, c]).

test_lists_all() ->
  lists:all(fun(I) ->
                I div 2 > 0
            end, [1, 2, 3, 4]).

test_hd_and_tl :: () -> {Integer, [Integer]}.
test_hd_and_tl() ->
  List = [1, 2, 3, 4, 5],
  {hd(List), erlang:tl(List)}.
