%% Copyright (c) 2016-2017 Amir Moulavi
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(type_internal).

-export([ built_in/1
        , dispatch/2
        , dispatch/3
        , eliminate/3
        , eliminate/4
        , extract_generic_types/1
        , extract_type_terminals/2
        , extract_user_defined_types/1
        , find_record_type/2
        , find_record_field_type/2
        , generic_materialisation/2
        , invalid_operator/0
        , lub/1
        , lub/2
        , op/1
        , op/2
        , sub_type_of/2
        , type_alias/1
        , type_equivalent/2
        , type_intersection/2
        , type_map/2
        , type_mapfold/3
        , type_tag/1
        , type_terminals/1
        , var_terminals/1
        , operand_type/1
        ]).

%%------------------------------------------------------------------------------

-include("type_macros.hrl").
-include("type_checker_state.hrl").

%%------------------------------------------------------------------------------

module_of(?INTEGER) ->
  ?INTEGER_MOD;
module_of(?CHAR) ->
  ?CHAR_MOD;
module_of(?FLOAT) ->
  ?FLOAT_MOD;
module_of(?STRING) ->
  ?STRING_MOD;
module_of(?ANY) ->
  ?ANY_MOD;
module_of(?BOOLEAN) ->
  ?BOOLEAN_MOD;
module_of(?PID) ->
  ?PID_MOD;
module_of(?NUMBER) ->
  ?NUMBER_MOD;
module_of({union_type, _}) ->
  ?UNION_MOD;
module_of({list_type, _}) ->
  ?LIST_MOD;
module_of({avl_atom_type, _}) ->
  ?MODULE;
module_of({tuple_type, _}) ->
  ?MODULE;
module_of(W) ->
  io:format("Module_of is not defined for ~p~n", [W]),
  ?MODULE.

%%_-----------------------------------------------------------------------------

%% Binary operator dispatcher to respective types
dispatch(undefined, _, undefined) ->
  undefined;
dispatch(undefined, Op, T) ->
  dispatch(T, Op, undefined);

dispatch({list_type, T1}, Op, {union_type, Ts}) ->
  [avl_list:op(Op, T1, T2) || T2 <- Ts];
dispatch({list_type, T1}, Op, T2) ->
  avl_list:op(Op, T1, T2);

dispatch({union_type, Ts}, Op, T) ->
  dispatch(T, Op, {union_type, Ts});

dispatch(T1, Op, T2) ->
  apply(module_of(T1), op, [Op, T2]).

%% Unary operator dispatcher to respective types

dispatch(Op, {union_type, Ts}) ->
  [dispatch(Op, T) || T <- Ts];

dispatch(Op, T) ->
  apply(module_of(T), op, [Op]).

%%_-----------------------------------------------------------------------------

op(Op) ->
  io:format("No dispatcher defined for Operator ~p~n", [Op]),
  undefined.

op(Op, T2) ->
  io:format("No dispatcher defined for Operator "
            ++ "~p and Type ~p~n", [Op, T2]),
  undefined.

%%_-----------------------------------------------------------------------------

lub(_T) ->
  ?ANY.

lub(T, T) ->
  T;
lub(T, nothing) ->
  T;
lub(nothing, T) ->
  T;
lub({list_type, T1}, T2) ->
  avl_list:lub(T1, T2);
lub(undefined, _) ->
  undefined;
lub(_, undefined) ->
  undefined;
lub({union_type, _} = T1, {union_type, _} = T2) ->
  case type_equivalent(T1, T2) of
    true  -> T1;
    false -> ?ANY
  end;
lub({tuple_type, T1}, {tuple_type, T2}) ->
  case length(T1) =:= length(T2) of
    true ->
      {tuple_type, lists:map(fun({A, B}) ->
                                 lub(A, B)
                             end, lists:zip(T1, T2))};
    false ->
      ?ANY
  end;
lub({union_type, _}, _) ->
  ?ANY;
lub({fun_type, _, _} = T1, {fun_type, _, _} = T2) ->
  case type_equivalent(T1, T2) of
    true  -> T1;
    false -> ?ANY
  end;
lub({fun_type, _, _}, _) ->
  ?ANY;
lub({avl_atom_type, _}, {avl_atom_type, _}) ->
  ?ATOM;
lub(?ATOM, {avl_atom_type, _}) ->
  ?ATOM;
lub({avl_atom_type, _}, ?ATOM) ->
  ?ATOM;
lub({avl_atom_type, _}, _) ->
  ?ANY;
lub(_, {avl_atom_type, _}) ->
  ?ANY;
lub(?ANY, _) ->
  ?ANY;
lub(_, ?ANY) ->
  ?ANY;
lub(T1, T2) ->
  M = module_of(T1),
  case M =:= ?MODULE of
    true -> io:format("No lub defined for ~p and ~p~n", [T1, T2]);
    false -> ok
  end,
  apply(M, lub, [T2]).

%%_-----------------------------------------------------------------------------

type_tag(Type) ->
  case built_in(Type) of
    true  -> {avl_type, Type};
    false -> {avl_generic_type, Type}
  end.

built_in('Any')         -> true;
built_in('Atom')        -> true;
built_in('Binary')      -> true;
built_in('Char')        -> true;
built_in('Boolean')     -> true;
built_in('Float')       -> true;
built_in('Integer')     -> true;
built_in('None')        -> true;
built_in('Number')      -> true;
built_in('Reference')   -> true;
built_in('String')      -> true;
built_in('Pid')         -> true;
built_in('Port')        -> true;

built_in(_)             -> false.

%%_-----------------------------------------------------------------------------

invalid_operator() ->
  invalid_operator.

%%_-----------------------------------------------------------------------------

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
type_equivalent({avl_atom_type, _}, ?ATOM) ->
  true;
type_equivalent(?ATOM, {avl_atom_type, _}) ->
  true;
type_equivalent(?ANY, _) ->
  true;
type_equivalent(_, ?ANY) ->
  true;
type_equivalent(?STRING, {list_type, ?CHAR}) ->
  true;
type_equivalent({list_type, ?CHAR}, ?STRING) ->
  true;
type_equivalent([T1], T2) ->
  type_equivalent(T1, T2);
type_equivalent(T1, [T2]) ->
  type_equivalent(T1, T2);
type_equivalent([_ | _] = Ts1, [_ | _] = Ts2) ->
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

%%_-----------------------------------------------------------------------------

sub_type_of(undefined, undefined) ->
  false;
sub_type_of(undefined, _) ->
  true;
sub_type_of({list_type, nothing}, {list_type, _}) ->
  true;
sub_type_of({list_type, T1}, {list_type, T2}) ->
  sub_type_of(T1, T2);
sub_type_of([_ | _] = T1, [_ | _] = T2) ->
  lists:all(fun(R) ->
                R =:= true
            end,
            [sub_type_of(TT1, T2) || TT1 <- T1]);
sub_type_of(T1, [_ | _] = T2) ->
  lists:any(fun(R) ->
                R =:= true
            end,
            [sub_type_of(T1, TT) || TT <- T2]);
sub_type_of([_ | _] = T1, T2) ->
  lists:any(fun(A) ->
                sub_type_of(A, T2)
            end, T1);
sub_type_of({fun_type, Is1, O1}, {fun_type, Is2, O2}) ->
  lists:all(fun({I1, I2}) ->
                sub_type_of(I1, I2)
            end, lists:zip(Is1, Is2)) andalso
    sub_type_of(O2, O1);
sub_type_of({fun_type, _, _}, {untyped_fun, nil, nil}) ->
  true;
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
sub_type_of(_, ?TERM) ->
  true;
sub_type_of(_, ?ANY) ->
  true;
sub_type_of(_, {avl_generic_type, _}) ->
  true;
sub_type_of({avl_atom_type, _}, ?ATOM) ->
  true;
sub_type_of({avl_atom_type, true}, ?BOOLEAN) ->
  true;
sub_type_of({avl_atom_type, false}, ?BOOLEAN) ->
  true;
sub_type_of(?BOOLEAN, {avl_atom_type, true}) ->
  true;
sub_type_of(?BOOLEAN, {avl_atom_type, false}) ->
  true;
sub_type_of(?BOOLEAN, ?ATOM) ->
  true;
sub_type_of(?INTEGER, ?NUMBER) ->
  true;
sub_type_of(?FLOAT, ?NUMBER) ->
  true;
sub_type_of(T, T) ->
  true;
sub_type_of(T1, T2) ->
  type_equivalent(T1, T2).

%%_-----------------------------------------------------------------------------

%% Type intersection for two tpes T1 and T2 is defined as T1 if
%% T1 is subtype of T2 and T2 if T2 is subtype of T1. Otherwise
%% the intersection would be type nothing.
type_intersection(undefined, T) ->
  T;
type_intersection(T, undefined) ->
  T;
type_intersection(T1, T2) ->
  case {sub_type_of(T1, T2), sub_type_of(T2, T1)} of
    {true, _}      -> T1;
    {false, true}  -> T2;
    {false, false} -> nothing
  end.

%%_-----------------------------------------------------------------------------

%% Tries to pattern match LHS and RHS and infer type
%% for variables in LHS from RHS. For sake of error handling
%% we need to eliminate to all posssible terminals in LHS.
eliminate(LHS, T, State) ->
  eliminate(LHS, T, [], State).

eliminate({var, _, '_'}, _, Rs, _) ->
  Rs;
eliminate({var, _, _} = V, T, Rs, _) ->
  [{V, T} | Rs];
eliminate({cons, _, A, B}, T, Rs0, State) ->
  Rs1 = eliminate(A, ulist(T), Rs0, State),
  eliminate(B, T, Rs1, State);
eliminate({tuple, L, Es}, {tuple_type, Ts} = T, Rs0, State) ->
  case length(Es) =/= length(Ts) of
    true -> throw({error, L, {match_on_unequally_sized_tuple, T}});
    false ->
      lists:foldl(fun({K, V}, Acc) ->
                      eliminate(K, V, Acc, State)
                  end, Rs0, lists:zip(Es, Ts))
  end;
eliminate({tuple, _L, Es}, _, Rs0, _) ->
  lists:flatten([eliminate(E, undefined, []) || E <- Es]) ++ Rs0;
eliminate({op, _, '++', {string, _, _}, {var, _, _} = V},
          ?STRING = T, Rs, State) ->
  eliminate(V, T, Rs, State);
eliminate({record, _, _, _} = R, _T, Rs, State) ->
  Rs ++ eliminate_record_type(R, State);
eliminate({match, _, L, R}, T, Rs, State) ->
  Rs ++ eliminate(L, T, Rs, State) ++ eliminate(R, T, Rs, State);
eliminate({atom, _, _}, _, Rs, _) ->
  Rs;
eliminate({nil, _}, _, Rs, _) ->
  Rs;
eliminate({bin, _, _}, _, Rs, _) ->
  Rs;
eliminate({call, _, _, _}, _, Rs, _) ->
  Rs;
eliminate(V, T, W, _) ->
  io:format("Investigate this and remove this log line (V: ~p, T: ~p): ~p"
           , [V, T, erlang:get_stacktrace()]),
  W.

eliminate_record_type({record, _, N, Ts}, State0=#state{}) ->
  TR = find_record_type(N, State0),
  lists:foldl(fun({record_field, _, {atom, _, F}, V}, Acc) ->
                  TF = find_record_field_type(F, TR),
                  Acc ++ eliminate(V, TF, State0)
              end, [], Ts).

%%_-----------------------------------------------------------------------------

generic_materialisation(Generic, T) ->
  Res = {_FT, _Maps, _Errs} = gm(Generic, T, dict:new(), []),
  Res.

gm([{fun_type, _, _} = FT1], {fun_type, _, _} = FT2, Mappings, Errs) ->
  gm(FT1, FT2, Mappings, Errs);
gm({fun_type, _, _} = FT1, [{fun_type, _, _} = FT2], Mappings, Errs) ->
  gm(FT1, FT2, Mappings, Errs);
gm([{fun_type, _, _} = FT1], [{fun_type, _, _} = FT2], Mappings, Errs) ->
  gm(FT1, FT2, Mappings, Errs);
gm({fun_type, Is1, O1} = FT1, {fun_type, Is2, O2} = FT2, Mappings, Errs) ->
  case length(Is1) =/= length(Is2) of
    true -> {generic_to_undefined(FT1),
             Mappings, Errs ++ [{non_matching_fun_args, FT1, FT2}]};
    false ->
      {NIs, NMappings, NErrs} =
        lists:foldl(fun({I1, I2}, {Acc, M, E}) ->
                        {T1, M1, E1} = gm(I1, I2, M, E),
                        {Acc ++ [T1], M1, E1}
                    end, {[], Mappings, Errs}, lists:zip(Is1, Is2)),
      {NO, NMappings1, NErrs1} = gm(O1, O2, NMappings, NErrs),
      {{fun_type, NIs, NO}, NMappings1, NErrs1}
  end;
gm({fun_type, Is, O}, TypedArgs, Mappings, Errs) ->
  {NIs, NMappings, NErrs} =
    lists:foldl(fun({G, T}, {Acc, M, E}) ->
                    {T1, M1, E1} = gm(G, T, M, E),
                    {Acc ++ [T1], M1, E1}
                end, {[], Mappings, Errs}, lists:zip(Is, TypedArgs)),
  FoldF =
    fun({avl_generic_type, T} = GT, Acc) ->
        case dict:find(T, NMappings) of
          {ok, Set} ->
            case gb_sets:to_list(Set) of
              [] -> {undefined, Acc};
              [V] -> {V, Acc};
              [_|_] = Vs ->
                {GT, Acc ++ [{can_not_instantiate_generic_type, T, Vs}]}
            end;
          error ->
            {GT, Acc}
        end;
       (W, Acc) ->
        {W, Acc}
    end,
  {NO, NErrs1} = type_mapfold(O, FoldF, NErrs),
  {{fun_type, NIs, NO}, NMappings, NErrs1};
gm({avl_generic_type, G}, T, Mappings, Errs) ->
  OldSet = case dict:find(G, Mappings) of
             {ok, Set} -> Set;
             error     -> gb_sets:new()
           end,
  NewSet = add_type(T, OldSet),
  Errs1 = case gb_sets:size(NewSet) > 1 of
            true ->
              [{can_not_instantiate_generic_type, G, gb_sets:to_list(NewSet)}];
            false ->
              []
          end,
  NMappings = dict:store(G, NewSet, Mappings),
  {T, NMappings, Errs ++ Errs1};
gm({list_type, T1}, {list_type, T2}, Mappings, Errs) ->
  {T, NMappings, NErrs} = gm(T1, T2, Mappings, Errs),
  {{list_type, T}, NMappings, NErrs};
gm({tuple_type, Ts1} = TT1, {tuple_type, Ts2} = TT2, Mappings, Errs) ->
  case length(Ts1) =/= length(Ts2) of
    true ->
      {generic_to_undefined({tuple_type, Ts1}), Mappings, Errs ++
         [{non_matching_tuple_length, TT1, TT2}]};
    false ->
      {T, NM, NE} =
        lists:foldl(fun({T1, T2}, {Acc, M, E}) ->
                        {T11, M1, E1} = gm(T1, T2, M, E),
                        {Acc ++ [T11], M1, E1}
                    end, {[], Mappings, Errs}, lists:zip(Ts1, Ts2)),
      {{tuple_type, T}, NM, NE}
  end;
gm(T1, _T2, Mappings, Errs) ->
  {generic_to_undefined(T1), Mappings, Errs}.

add_type(undefined, OldSet) ->
  OldSet;
add_type({list_type, _} = T, OldSet) ->
  reduce_list_types(T, OldSet);
add_type(T, OldSet) ->
  gb_sets:add(T, OldSet).

reduce_list_types({list_type, nothing} = T, OldSet) ->
  case lists:keyfind(list_type, 1, gb_sets:to_list(OldSet)) of
    false -> gb_sets:add(T, OldSet);
    _     -> OldSet
  end;
reduce_list_types({list_type, _} = T, OldSet) ->
  case gb_sets:is_member({list_type, nothing}, OldSet) of
    true  -> gb_sets:add(T, gb_sets:delete({list_type, nothing}, OldSet));
    false -> gb_sets:add(T, OldSet)
  end.

generic_to_undefined(Type) ->
  Map = fun({avl_generic_type, _}) ->
            undefined;
           (T) -> T
        end,
  type_map(Type, Map).

%%_-----------------------------------------------------------------------------

ulist({list_type, T}) ->
  T;
ulist(_) ->
  undefined.

%%_-----------------------------------------------------------------------------

var_terminals(Expr) ->
  var_terminals(Expr, []).

var_terminals({integer, _, _}, Rs) ->
  Rs;
var_terminals({char, _, _}, Rs) ->
  Rs;
var_terminals({atom, _, _}, Rs) ->
  Rs;
var_terminals({var, _, '_'}, Rs) ->
  Rs;
var_terminals({var, _, _} = V, Rs) ->
  [V | Rs];
var_terminals({cons, _, A, B}, Rs0) ->
  Rs1 = var_terminals(A, Rs0),
  var_terminals(B, Rs1);
var_terminals({tuple, _L, Es}, Rs0) ->
  lists:flatten([var_terminals(E, []) || E <- Es]) ++ Rs0;
var_terminals(_, W) ->
  W.

%%_-----------------------------------------------------------------------------

type_mapfold({fun_sig, L, N, Ts}, FoldF, Acc) ->
  {NTs, Acc1} = type_mapfold_helper(Ts, FoldF, Acc),
  {{fun_sig, L, N, NTs}, Acc1};
type_mapfold({fun_remote_sig, L, M, N, Ts}, FoldF, Acc) ->
  {NTs, Acc1} = type_mapfold_helper(Ts, FoldF, Acc),
  {{fun_rempte_sig, L, M, N, NTs}, Acc1};
type_mapfold({type_alias, L, T}, FoldF, Acc) ->
  {NT, Acc1} = FoldF(T, Acc),
  {{type_alias, L, NT}, Acc1};
type_mapfold({fun_type, Is, O}, FoldF, Acc) ->
  {NIs, Acc1} = type_mapfold_helper(Is, FoldF, Acc),
  {NO, Acc2} = FoldF(O, Acc1),
  {{fun_type, NIs, NO}, Acc2};
type_mapfold({union_type, Ts}, FoldF, Acc) ->
  {NTs, Acc1} = type_mapfold_helper(Ts, FoldF, Acc),
  {{union_type, NTs}, Acc1};
type_mapfold({list_type, T}, FoldF, Acc) ->
  {NT, Acc1} = FoldF(T, Acc),
  {{list_type, NT}, Acc1};
type_mapfold({tuple_type, Ts}, FoldF, Acc) ->
  {NTs, Acc1} = type_mapfold_helper(Ts, FoldF, Acc),
  {{tuple_type, NTs}, Acc1};
type_mapfold({record_type, N, Ts}, FoldF, Acc) ->
  {NTs, Acc1} = type_mapfold_helper(Ts, FoldF, Acc),
  {{record_type, N, NTs}, Acc1};
type_mapfold({type_instance, N, Ts}, FoldF, Acc) ->
  {NTs, Acc1} = type_mapfold_helper(Ts, FoldF, Acc),
  {{type_instance, N, NTs}, Acc1};
type_mapfold(T, FoldF, Acc) ->
  FoldF(T, Acc).

type_mapfold_helper(Ts, FoldF, Acc) ->
  lists:foldl(fun(T, {TT, Ac}) ->
                  {NT, Ac1} = type_mapfold(T, FoldF, Ac),
                  {TT ++ [NT], Ac1}
              end, {[], Acc}, Ts).

%%_-----------------------------------------------------------------------------

type_map({fun_sig, L, N, Ts}, FMap) ->
  {fun_sig, L, N, [type_map(T, FMap) || T <- Ts]};
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

%%_-----------------------------------------------------------------------------

extract_user_defined_types(Ts) when is_list(Ts) ->
  lists:flatten([extract_user_defined_types(T) || T <- Ts]);
extract_user_defined_types(T) ->
  extract_type_terminals(avl_user_defined, T).

%%_-----------------------------------------------------------------------------

extract_generic_types(T) ->
  extract_type_terminals(avl_generic_type, T).

%%_-----------------------------------------------------------------------------

%% Tags can be one of the following:
%% - avl_type
%% - avl_generic_type
%% - avl_type_ref
%% - avl_user_defined
extract_type_terminals(Tag, T) ->
  lists:foldl(
    fun(Tp, Acc) ->
        case Tp of
          {Tag, Type} -> [Type | Acc];
          Tag         -> [Tag | Acc];
          _           -> Acc
        end
    end, [], type_terminals(T)).

type_terminals({fun_type, Is, O}) ->
  lists:flatten([type_terminals(I) || I <- Is])
    ++ type_terminals(O);
type_terminals({union_type, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({list_type, nothing} = T) ->
  [T];
type_terminals({list_type, T}) ->
  type_terminals(T);
type_terminals({tuple_type, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({record_type, _, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({type_instance, _, Ts}) ->
  lists:flatten([type_terminals(T) || T <- Ts]);
type_terminals({avl_type, _} = T) ->
  [T];
type_terminals({avl_generic_type, _} = T) ->
  [T];
type_terminals({avl_type_ref, _, _} = T) ->
  [T];
type_terminals({avl_user_defined, _} = T) ->
  [T];
type_terminals({avl_atom_type, _} = T) ->
  [T];
type_terminals({record_type, _} = T) ->
  [T];
type_terminals({untyped_fun, nil, nil} = T) ->
  [T];
type_terminals(T) when is_list(T) ->
  lists:flatten([type_terminals(TT) || TT <- T]);
type_terminals(undefined) ->
  [undefined];
type_terminals(W) ->
  throw({fatal_error, not_recognized, W}).

%%_-----------------------------------------------------------------------------

find_record_type(N, State=#state{}) ->
  RT = state_dl:record_types(State),
  case dict:find(N, RT) of
    {ok, T} -> T;
    error   -> undefined
  end.

find_record_field_type(_, undefined) ->
  undefined;
find_record_field_type(F, T) ->
  proplists:get_value(F, T).

%%_-----------------------------------------------------------------------------

type_alias(?STRING) ->
  {list_type, ?CHAR};
type_alias(T) ->
  T.

%%_-----------------------------------------------------------------------------

operand_type('/')     -> ?NUMBER;
operand_type('+')     -> ?NUMBER;
operand_type('*')     -> ?NUMBER;
operand_type('-')     -> ?NUMBER;
operand_type('band')  -> ?NUMBER;
operand_type('bor')   -> ?NUMBER;
operand_type('bxor')  -> ?NUMBER;
operand_type('bsl')   -> ?NUMBER;
operand_type('bsr')   -> ?NUMBER;
operand_type('bnot')  -> ?NUMBER;
operand_type('div')   -> ?NUMBER;
operand_type('rem')   -> ?NUMBER;
operand_type(_)       -> undefined.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:










