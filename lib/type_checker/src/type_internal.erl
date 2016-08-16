-module(type_internal).

-export([ built_in/1
        , dispatch/2
        , dispatch/3
        , eliminate/2
        , eliminate/3
        , extract_generic_types/1
        , extract_type_terminals/2
        , extract_user_defined_types/1
        , invalid_operator/0
        , lcs/1
        , lcs/2
        , op/1
        , op/2
        , sub_type_of/2
        , tag_built_in/1
        , type_equivalent/2
        , type_map/2
        , type_tag/1
        , type_terminals/1
        ]).

-include("type_macros.hrl").


module_of({terl_type, 'Integer'}) ->
  ?INTEGER_MOD;
module_of({terl_type, 'Float'}) ->
  ?FLOAT_MOD;
module_of({terl_type, 'String'}) ->
  ?STRING_MOD;
module_of({terl_type, 'Any'}) ->
  ?ANY_MOD;
module_of({terl_type, 'Boolean'}) ->
  ?BOOLEAN_MOD;
module_of({union_type, _}) ->
  ?UNION_MOD;
module_of({list_type, _}) ->
  ?LIST_MOD;
module_of(W) ->
  io:format("Module_of is not defined for ~p~n", [W]),
  ?MODULE.



%% Binary operator dispatcher to respective types
dispatch(undefined, _, undefined) ->
  undefined;
dispatch(undefined, Op, T) ->
  dispatch(T, Op, undefined);

dispatch({list_type, T1}, Op, {union_type, Ts}) ->
  [terl_list:op(Op, T1, T2) || T2 <- Ts];
dispatch({list_type, T1}, Op, T2) ->
  terl_list:op(Op, T1, T2);

dispatch({union_type, Ts}, Op, T) ->
  dispatch(T, Op, {union_type, Ts});

dispatch(T1, Op, T2) ->
  apply(module_of(T1), op, [Op, T2]).

%% Unary operator dispatcher to respective types

dispatch(Op, {union_type, Ts}) ->
  [dispatch(Op, T) || T <- Ts];

dispatch(Op, T) ->
  apply(module_of(T), op, [Op]).


op(Op) ->
  io:format("No dispatcher defined for Operator ~p~n", [Op]),
  undefined.

op(Op, T2) ->
  io:format("No dispatcher defined for Operator "
            ++ "~p and Type ~p~n", [Op, T2]),
  undefined.

lcs(T) ->
  io:format("No lcs defined to ~p~n", [T]),
  ?ANY.

lcs(T, nothing) ->
  T;
lcs(nothing, T) ->
  T;
lcs({list_type, T1}, T2) ->
  terl_list:lcs(T1, T2);
lcs(undefined, _) ->
  undefined;
lcs(_, undefined) ->
  undefined;
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
lcs(T1, T2) ->
  M = module_of(T1),
  apply(M, lcs, [T2]).

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
built_in('Atom') -> true;
built_in('Binary') -> true;
built_in('Boolean') -> true;
built_in('Float') -> true;
built_in('Integer') -> true;
built_in('None') -> true;
built_in('Number') -> true;
built_in('Reference') -> true;
built_in('String') -> true;
built_in('Pid') -> true;
built_in('Port') -> true;


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
type_equivalent({terl_atom_type, _}, {terl_type, 'Atom'}) ->
  true;
type_equivalent({terl_type, 'Atom'}, {terl_atom_type, _}) ->
  true;
type_equivalent({terl_type, 'Any'}, _) ->
  true;
type_equivalent(_, {terl_type, 'Any'}) ->
  true;
type_equivalent([_, _] = Ts1, [_, _] = Ts2) ->
  (length(Ts1) =:= length(Ts2)) andalso
    lists:all(fun(E) -> E =:= true
              end,
              [lists:any(
                 fun(E1) ->
                     E1 =:= true
                 end, [type_equivalent(T, TT) || TT <- Ts2])
               || T <- Ts1]);
type_equivalent(T, T) ->
  true;
type_equivalent(_, _) ->
  false.



sub_type_of({fun_type, Is1, O1}, {fun_type, Is2, O2}) ->
  lists:all(fun({I1, I2}) ->
                sub_type_of(I1, I2)
            end, lists:zip(Is1, Is2)) andalso
  sub_type_of(O2, O1);
sub_type_of({union_type, Ts1}, {union_type, _} = UT2) ->
  lists:all(fun(E) -> E =:= true end,
            [sub_type_of(T, UT2) || T <- Ts1]);
sub_type_of(T1, {union_type, Ts}) ->
  lists:any(fun(E) -> E =:= true end,
            [sub_type_of(T1, T) || T <- Ts]);
sub_type_of({tuple_type, _} = TT1, {tuple_type, _} = TT2) ->
  type_equivalent(TT1, TT2);
sub_type_of({list_type, T1}, {list_type, T2}) ->
  sub_type_of(T1, T2);
sub_type_of(_, {terl_type, 'Term'}) ->
  true;
sub_type_of(_, {terl_type, 'Any'}) ->
  true;
sub_type_of(T, T) ->
  true;
sub_type_of(_, _) ->
  false.

%% Tries to pattern match LHS and RHS and infer type
%% for variables in LHS from RHS. For sake of error handling
%% we need to eliminate to all posssible terminals in LHS.
eliminate(LHS, T) ->
  eliminate(LHS, T, []).

eliminate({integer, _, _}, _, Rs) ->
  Rs;
eliminate({atom, _, _}, _, Rs) ->
  Rs;
eliminate({var, _, '_'}, _, Rs) ->
  Rs;
eliminate({var, _, _} = V, T, Rs) ->
  [{V, T} | Rs];
eliminate({cons, _, A, B}, T, Rs0) ->
  Rs1 = eliminate(A, ulist(T), Rs0),
  eliminate(B, T, Rs1);
eliminate({tuple, L, Es}, {tuple_type, Ts} = T, Rs0) ->
  case length(Es) =/= length(Ts) of
    true -> throw({error, L, {match_on_unequally_sized_tuple, T}});
    false ->
      lists:foldl(fun({K, V}, Acc) ->
                      eliminate(K, V, Acc)
                  end, Rs0, lists:zip(Es, Ts))
  end;
eliminate({tuple, _L, Es}, _, Rs0) ->
  lists:flatten([eliminate(E, undefined, []) || E <- Es]) ++ Rs0;
eliminate({op, _, '++', {string, _, _}, {var, _, _} = V},
       {terl_type, string} = T, Rs) ->
  eliminate(V, T, Rs);
eliminate(_, _, W) ->
  W.

ulist({list_type, T}) ->
  T;
ulist(_) ->
  undefined.



type_map({fun_sig, L, N, Ts}, FMap) ->
  {fun_sig, L, N, [type_map(T, FMap)|| T <- Ts]};
type_map({fun_remote_sig, L, M, N, Ts}, FMap) ->
  {fun_rempte_sig, L, M, N, [type_map(T, FMap) || T <- Ts]};
type_map({type_alias, L, T}, FMap) ->
  {type_alias, L, type_map(T, FMap)};
type_map({fun_type, Is, O}, FMap) ->
  {fun_type, [type_map(I, FMap) || I <- Is], type_map(O, FMap)};
type_map({union_type, Ts}, FMap) ->
  {union_type, [type_map(T, FMap) || T <- Ts]};
type_map({list_type, T}, FMap) ->
  {list_type, type_map(T, FMap)};
type_map({tuple_type, Ts}, FMap) ->
  {tuple_type, [type_map(T, FMap) || T <- Ts]};
type_map({record_type, N, Ts}, FMap) ->
  {record_type, N, [type_map(T, FMap) || T <- Ts]};
type_map({type_instance, N, Ts}, FMap) ->
  {type_instance, N, [type_map(T, FMap) || T <- Ts]};
type_map(T, FMap) ->
  FMap(T).


extract_user_defined_types(Ts) when is_list(Ts) ->
  lists:flatten([extract_user_defined_types(T) || T <- Ts]);
extract_user_defined_types(T) ->
  extract_type_terminals(terl_user_defined, T).

extract_generic_types(T) ->
  extract_type_terminals(terl_generic_type, T).

%% Tags can be one of the following:
%% - terl_type
%% - terl_generic_type
%% - terl_type_ref
%% - terl_user_defined
extract_type_terminals(Tag, T) ->
  lists:foldl(fun(Tp, Acc) ->
                  case Tp of
                    {Tag, Type} ->
                      [Type | Acc];
                    Tag ->
                      [Tag | Acc];
                    _ -> Acc
                  end
              end, [], type_terminals(T)).

type_terminals({fun_type, Is, O}) ->
  lists:flatten([type_terminals(I) || I <- Is])
    ++ type_terminals(O);
type_terminals({union_type, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({list_type, T}) ->
  type_terminals(T);
type_terminals({tuple_type, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({record_type, _, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({type_instance, _, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({terl_type, _} = T) ->
  [T];
type_terminals({terl_generic_type, _} = T) ->
  [T];
type_terminals({terl_type_ref, _, _} = T) ->
  [T];
type_terminals({terl_user_defined, _} = T) ->
  [T];
type_terminals({terl_atom_type, _} = T) ->
  [T];
type_terminals({untyped_fun, nil, nil} = T) ->
  [T];
type_terminals(T) when is_list(T) ->
  lists:flatten([type_terminals(TT) || TT <- T]);
type_terminals(undefined) ->
  [undefined];
type_terminals(W) ->
  throw({fatal_error, not_recognized, W}).


