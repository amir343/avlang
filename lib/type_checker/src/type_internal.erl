-module(type_internal).

-export([ built_in/1
        , dispatch/2
        , dispatch/3
        , invalid_operator/0
        , lcs/2
        , reduce/3
        , tag_built_in/1
        , type_equivalent/2
        , type_tag/1
        ]).

%% Binary operator dispatcher to respective types
dispatch(undefined, _, undefined) ->
  undefined;
dispatch(undefined, Op, T) ->
  dispatch(T, Op, undefined);

dispatch({union_type, Ts}, Op, T) ->
  dispatch(T, Op, {union_type, Ts});

dispatch({_, 'Integer'} = T, Op, {union_type, Ts}) ->
  [dispatch(T, Op, T1) || T1 <- Ts];
dispatch({_, 'Integer'}, Op, {_, T}) ->
  terl_integer:op(Op, T);
dispatch({_, 'Integer'}, Op, undefined) ->
  terl_integer:op(Op, undefined);

dispatch({_, 'Float'} = T, Op, {union_type, Ts}) ->
  [dispatch(T, Op, T1) || T1 <- Ts];
dispatch({_, 'Float'}, Op, {_, T}) ->
  terl_float:op(Op, T);
dispatch({_, 'Float'}, Op, undefined) ->
  terl_float:op(Op, undefined);


dispatch({_, 'Boolean'} = T, Op, {union_type, Ts}) ->
  [dispatch(T, Op, T1) || T1 <- Ts];
dispatch({_, 'Boolean'}, Op, {_, T}) ->
  terl_boolean:op(Op, T);
dispatch({_, 'Boolean'}, Op, undefined) ->
  terl_boolean:op(Op, undefined);

dispatch({_, 'String'} = T, Op, {union_type, Ts}) ->
  [dispatch(T, Op, T1) || T1 <- Ts];
dispatch({_, 'String'}, Op, {_, T}) ->
  terl_string:op(Op, T);
dispatch({_, 'String'}, Op, undefined) ->
  terl_string:op(Op, undefined);

dispatch({list_type, T1}, Op, {union_type, Ts}) ->
  [terl_list:op(Op, T1, T2) || T2 <- Ts];
dispatch({list_type, T1}, Op, T2) ->
  terl_list:op(Op, T1, T2);

dispatch(T, Op, T2) ->
  io:format("No dispatcher defined for Operator "
            ++ "~p and Type ~p and ~p~n", [Op, T, T2]),
  undefined.

%% Unary operator dispatcher to respective types

dispatch(Op, {_, 'Boolean'}) ->
  terl_boolean:op(Op);
dispatch(Op, {_, 'Float'}) ->
  terl_float:op(Op);
dispatch(Op, {_, 'Integer'}) ->
  terl_integer:op(Op);
dispatch(Op, {_, 'String'}) ->
  terl_string:op(Op);
dispatch(Op, {_, list_type}) ->
  terl_list:op(Op);
dispatch(Op, {union_type, Ts}) ->
  [dispatch(Op, T) || T <- Ts];
dispatch(Op, T) ->
  io:format("No dispatcher defined for Operator ~p and Type ~p~n", [Op, T]),
  undefined.

lcs({_, 'Integer'}, T) ->
  terl_integer:lcs(T);
lcs({_, 'Float'}, T) ->
  terl_float:lcs(T);
lcs({_, 'Boolean'}, T) ->
  terl_boolean:lcs(T);
lcs({_, 'String'}, T) ->
  terl_string:lcs(T);
lcs({terl_type, 'Any'} = T, _) ->
  T;
lcs(_, {terl_type, 'Any'} = T) ->
  T;
lcs({list_type, T1}, T2) ->
  terl_list:lcs(T1, T2);
lcs(undefined, _) ->
  undefined;
lcs(_, undefined) ->
  undefined;
lcs(T, nothing) ->
  T;
lcs(nothing, T) ->
  T;
lcs({union_type, _} = T1, {union_type, _} = T2) ->
  case type_equivalent(T1, T2) of
    true -> T1;
    false -> {terl_type, 'Any'}
  end;
lcs({union_type, _}, _) ->
  {terl_type, 'Any'};
lcs({fun_type, _, _} = T1, {fun_type, _, _} = T2) ->
  case type_equivalent(T1, T2) of
    true -> T1;
    false -> {terl_type, 'Any'}
  end;
lcs({fun_type, _, _}, _) ->
  {terl_type, 'Any'};
lcs(_, _) ->
  tag_built_in('Any').

type_tag(Type) ->
  case built_in(Type) of
    true -> {terl_type, Type};
    false -> {terl_generic_type, Type}
  end.

invalid_operator() ->
  invalid_operator.

tag_built_in(Type) ->
  {terl_type, Type}.

built_in('Any') -> true;
built_in('None') -> true;
built_in('Atom') -> true;
built_in('Integer') -> true;
built_in('Pid') -> true;
built_in('Reference') -> true;
built_in('Binary') -> true;
built_in('String') -> true;
built_in('Boolean') -> true;
built_in('Float') -> true;


built_in(_) -> false.


%% Check if given two types are equivalent
type_equivalent({fun_type, Is1, O1}, {fun_type, Is2, O2}) ->
  length(Is1) =:= length(Is2)
    andalso
    lists:all(fun(E) -> E =:= true end,
              [type_equivalent(T1, T2) ||
                {T1, T2} <- lists:zip(Is1, Is2)])
    andalso
    type_equivalent(O1, O2);
type_equivalent({union_type, Ts1}, {union_type, Ts2}) ->
  (length(Ts1) =:= length(Ts2)) andalso
    lists:all(fun(E) -> E =:= true
              end,
              [lists:any(
                 fun(E1) ->
                     E1 =:= true
                 end, [type_equivalent(T, TT) || TT <- Ts2])
               || T <- Ts1]);
type_equivalent({tuple_type, _}, {tuple_type, []}) ->
  true;
type_equivalent({tuple_type, []}, {tuple_type, _}) ->
  true;
type_equivalent({tuple_type, Ts1}, {tuple_type, Ts2}) ->
  (length(Ts1) =:= length(Ts2)) andalso
    lists:all(fun(E) -> E =:= true end,
              [type_equivalent(T1, T2) || {T1, T2} <- lists:zip(Ts1, Ts2)]);
type_equivalent({terl_atom_type, T}, {terl_type, T}) ->
  true;
type_equivalent({terl_type, T}, {terl_atom_type, T}) ->
  true;
type_equivalent({terl_type, 'Any'}, _) ->
  true;
type_equivalent(_, {terl_type, 'Any'}) ->
  true;
type_equivalent(T, T) ->
  true;
type_equivalent(_, _) ->
  false.


%% Tries to pattern match LHS and RHS and infer type
%% for variables in LHS from RHS. For sake of error handling
%% we need to reduce to all posssible terminals in LHS.
reduce({integer, _, _}, _, Rs) ->
  Rs;
reduce({atom, _, _}, _, Rs) ->
  Rs;
reduce({var, _, '_'}, _, Rs) ->
  Rs;
reduce({var, _, _} = V, T, Rs) ->
  [{V, T} | Rs];
reduce({cons, _, A, B}, T, Rs0) ->
  Rs1 = reduce(A, ulist(T), Rs0),
  reduce(B, T, Rs1);
reduce({tuple, L, Es}, {tuple_type, Ts} = T, Rs0) ->
  case length(Es) =/= length(Ts) of
    true -> throw({error, L, {match_on_unequally_sized_tuple, T}});
    false ->
      lists:foldl(fun({K, V}, Acc) ->
                      reduce(K, V, Acc)
                  end, Rs0, lists:zip(Es, Ts))
  end;
reduce({tuple, _L, Es}, _, Rs0) ->
  lists:flatten([reduce(E, undefined, []) || E <- Es]) ++ Rs0;
reduce({op, _, '++', {string, _, _}, {var, _, _} = V},
       {terl_type, string} = T, Rs) ->
  reduce(V, T, Rs);
reduce(_, _, W) ->
  W.

ulist({list_type, T}) ->
  T;
ulist(_) ->
  undefined.
