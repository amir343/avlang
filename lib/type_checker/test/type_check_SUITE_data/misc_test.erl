-module(misc_test).

specs() ->
  [
    {1, "Amir", "Moulavi", 2.0, ok}
  , {2, "Tobias", "Lindahl", 3.0, ok}
  ].

and1(true, true) -> true;
and1(false, _)   -> false;
and1(_, false)   -> false.


lc() ->
  [begin
     [I1]
   end
   || I <- integers(), I1 <- I, I1 rem 2 =:= 0].

integers() ->
  [[1], [2], [3], [4]].


pid() ->
  Pid :: Pid = spawn(fun() -> 1 =/= 1 end),
  Pid ! yes_that_is_ok.

bar_baz :: (Integer, Integer) -> Any.
bar_baz(A, B) ->
  if
    A > B andalso A =/= B ->
      "Equal";
    B > A ->
      B;
    A =/= B ->
      foo(A, B);
    true ->
      B - A
  end.

foo :: (Integer, Integer) -> Integer.
foo(A, B) ->
  A + B.

bar() ->
  L = [1, 2, 3, 4],
  B :: Boolean = case L of
                   [] ->
                     false;
                   [_ | T] ->
                     T =:= [2, 3, 4]
                 end,
  B.

baz :: [Integer] -> Boolean.
baz(N) ->
  case N of
    [] ->
      false;
    [H | _] when is_integer(H) ->
      true
  end.

foo1(X, Y) when is_integer(X) andalso is_integer(Y) ->
  {X, Y};
foo1(X, Y) when is_atom(X), is_integer(Y) ->
  Y + list_to_integer(atom_to_list(X)).


f :: (list, Integer, Integer) -> [Integer];
     (tuple, Integer, Integer) -> {Integer, Integer}.
f(list, A, B) ->
  [A + B];
f(tuple, A, B) ->
  {A, B}.

myfun :: (Integer, Integer) -> [Integer].
myfun(A, B) ->
  T :: Integer = 1 + B,
  N = T * 2 + A,
  [N].

myfun(A, B, C) ->
  T = 22234234234,
  P :: Integer = T * 2 + B,
  S = [1, 2, 3, 4] ++ A,
  F :: ((ok, Integer, Integer) -> Integer;
       (error, Float, Float) -> Float)
       = fun(ok, A, B) ->
        list_to_integer(C),
        A + B * P;
       (error, A, B) -> A - B end,
  F(ok, P, B).

f2 :: Integer -> Integer;
      Float -> Float.
f2(A) ->
  T :: Integer = 1,
  P = A + T,
  P;
f2(A) ->
  T :: Integer = 1,
  P = A + T,
  P.

match :: {ok, String}, Any -> String;
         {error, {Integer, String}}, Any -> {String, Integer}.
match({ok, T}, _) ->
  T;
match({error, {RC, Reason}}, _) ->
  {Reason, RC}.

test_best(H, T) ->
  {list(head, H), list(tail, T)}.


list :: (head, [Integer]) -> Integer;
        (tail, [Integer]) -> [Integer].
list(head, [H|T]) ->
  H;
list(tail, [_|T]) ->
  T.


get_func_name() ->
  match.

another_func() ->
  element(2, {1, 2}),
  get_func_name().

recursive_fun :: ok, Any -> ok;
                 recurse, sth -> ok.

recursive_fun(ok, _) ->
  ok;
recursive_fun(recurse, sth) ->
  recursive_fun(ok, sth).

test() ->
  L = [1.2, "Amir"],
  F :: match = another_func(),
  F({error, {list(head, [2, 3]), "Amir"}}, "AbbasAgha"),
  {Reason, RC} = match({error, {23, "Amir"}}, "Bagh"),
  fun(A) ->
      list_to_integer(A)
  end.


test2() ->
  F :: (String -> Integer) = test(),
  F("123").

process_test() ->
  spawn_opt(?MODULE, fun_test, [1, 2, 3], [{test, true}]).

fun_test :: Integer, Integer, Integer -> Integer.
fun_test(A, B, C) ->
  A + B + C.
