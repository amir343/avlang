%% Current abstract forms from erl_parser:
%%
%% {fun_sig, Line, Name, Type}
%% {fun_type, Is, O}
%% {type_alias, Line, Name, Type}
%% {type_cons, Line, Name, Params, Type}
%% {type_instance, Name, Params}
%% {terl_type_ref, Module, Name}
%% {union_type, Types}
%% {record_type, Name, Types}
%% {list_type, Type}
%% {tuple_type, Types}
%% {terl_type, Type}
%% {terl_user_defined, Type}
%% {terl_generic_type, Type}
%%


-module(type_check).

-export([ module/3
        , format_error/1
        ]).

-record(state, { fun_sigs      = dict:new()
                 %% {key, [{fun_sig, L, N, T}]}
               , type_aliases  = dict:new()
                 %% {Key, {type_alias, L, N, T}}
               , type_cons     = dict:new()
                 %% {Key, {type_cons, L, N, P, T}}
               , declared_fun  = dict:new()
                 %% {Key, [{function, ...}]}
               , type_used     = gb_sets:new()
                 %% Set(atom)
               , type_used_loc = dict:new()
                 %% {Key, [Line]}
         }).

-define(TYPE_MSG, type_check).

module(Forms, FileName, Opts) ->
  run_passes(standard_passes(), Forms, FileName, Opts, [], #state{}).

run_passes([P | Ps], Fs, Fn, Opts, Ws0, State0) ->
  try
    case P(Fs, Fn, Opts, State0) of
      {ok, Ws, State1} ->
        run_passes(Ps, Fs, Fn, Opts, Ws0 ++ Ws, State1);
      {error, _, _} = E ->
        E
    end
  catch
    _:{error, L, Desc} -> {error, [{Fn, [{L, ?TYPE_MSG, Desc}]}], []};
    _:L when is_list(L) -> {error, L, []};
    EE:Err ->
      io:format("Something bad happened, type system apologizes: ~p:~p~n"
               , [EE, Err])
  end;
run_passes([], _, _, _, Ws, _) ->
  {ok, Ws}.

standard_passes() ->
  [ fun type_lint/4
  , fun type_check/4
  ].

type_lint(Forms, FileName, _Opts, State) ->
  io:format("~p~n", [Forms]),
  St = collect_types(Forms, State),
  check_consistency_type_cons(St, FileName),
  Ws1 = check_unsued_user_defined_types(FileName, St),
  check_undefined_types(FileName, St),
  match_fun_sig_with_declared_fun(St),
  %% TODO: checks for generic types
  %% - RHS usage
  %% - Type expansion: type instances, type aliases
  {ok, Ws1, St}.

type_check(Forms, _FileName, _Opts, State) ->
  type_check0(Forms, State),
  {ok, [], State}.

%% Collect forms of interest into state record
collect_types([{fun_sig, L, _, T} = Form | Forms], St0) ->
  St1 = extract_user_defined_types_with_locs(T, L, St0),
  collect_types(Forms, add_fun_sigs(Form, St1));
collect_types([{type_alias, L, _, T} = Form | Forms], St0) ->
  St1 = extract_user_defined_types_with_locs(T, L, St0),
  collect_types(Forms, add_type_alias(Form, St1));
collect_types([{type_cons, _, _, _, _} = Form | Forms], St0) ->
  collect_types(Forms, add_type_cons(Form, St0));
collect_types([{function, _L, _N, _A, Cls} = F | Forms], St0) ->
  St1 = collect_types(Cls, St0),
  collect_types(Forms, add_declared_fun(F, St1));
collect_types([{clause, _, _A, _G, Exprs} | Cls], St0) ->
  St1 = collect_types(Exprs, St0),
  collect_types(Cls, St1);
collect_types([{match, L, _P, T, _E}| Exprs], St0) ->
  St1 = extract_user_defined_types_with_locs(T, L, St0),
  collect_types(Exprs, St1);
collect_types([_ | Forms], St) ->
  collect_types(Forms, St);
collect_types([], St) ->
  St.

%% Extract user defined types given at line L and associate each one with
%% line L in the returned state.type_used_loc.
extract_user_defined_types_with_locs(T, L, St) ->
  Ts = extract_user_defined_types(T),
  Locs = generate_locs_for_types(Ts, L),
  St#state{type_used =
             gb_sets:union(gb_sets:from_list(Ts), St#state.type_used)
          , type_used_loc = merge(Locs, St#state.type_used_loc)}.


generate_locs_for_types(Ts, L) ->
  lists:foldl(fun (Tp, D) ->
                  dict:append(Tp, L, D)
              end, dict:new(), Ts).

merge(D1, D2) ->
  dict:merge(fun(_, V1, V2) ->
                 V1 ++ V2
             end, D1, D2).

%% Check if a refered type is undefined in this module
check_undefined_types(FileName, #state{ type_aliases = TA
                                      , type_used = TU
                                      , type_used_loc = TUL}) ->
  [case dict:find(T, TA) of
     {ok, _} -> ok;
     _ ->
       {ok, L} = dict:find(T, TUL),
       Locs = gb_sets:to_list(gb_sets:from_list(L)),
       throw([{FileName, [{L1, ?TYPE_MSG, {undefined_type, T}}]}
              || L1 <-  Locs])
   end
   || T <- gb_sets:to_list(TU)].

%% Check if user defined types are used
check_unsued_user_defined_types(FileName, #state{type_aliases = TA
                                                , type_used = TU}) ->
  lists:flatten(
    [ case gb_sets:is_member(N, TU) of
        true  -> [];
        false -> [{FileName,
                   [{L, ?TYPE_MSG,
                     {type_alias_defined_not_used, N, L}}]}]
      end
      || {_, {type_alias, L, N, _}} <- dict:to_list(TA)]).


%% Check that the declared funs and corresponding function signature has
%% the same arity, otherwise, if missing or mismatch, error with proper message.
match_fun_sig_with_declared_fun(#state{declared_fun = DF, fun_sigs = FS}) ->
  [case V of
     [_|_] = L -> [match_fun_sig0(L1, DF) || L1 <- L];
     A         -> match_fun_sig0(A, DF)
   end
   || {_, V} <- dict:to_list(FS)].

match_fun_sig0({fun_sig, L2, N, _} = F, DF) ->
  SigAr = fun_arity(F),
  case dict:find(N, DF) of
    error ->
      throw({error, L2, {no_fun_decl_found_for_sig, N, L2}});
    {ok, [_|_] = L} ->
      case length([E || {function, _, _, Ar, _} = E <- L, SigAr =:= Ar])
      of
        0 ->
          throw({error, L2, {no_matching_fun_decl_for_fun_sig, N, SigAr, L2}});
        1 ->
          ok;
        _ ->
          throw({error, L2, {multi_match_fun_decl_for_fun_sig, N, L2}})
      end;
    {ok, {function, _, _, Ar, _}} ->
      case SigAr =:= Ar of
        false ->
          throw({error, L2, {no_matching_fun_decl_for_fun_sig, N, SigAr, L2}});
        true ->
          ok
      end
    end.


%% Adds a new function signature to state record or throws an exception if
%% this function signature is already defined.
add_fun_sigs({fun_sig, L2, N, _} = F, St=#state{fun_sigs = FS}) ->
  case dict:find(N, FS) of
    {ok, Vs} ->
      Ar = fun_arity(F),
      case [V || V <- Vs, fun_arity(V) =:= Ar] of
        [{_, L1, _, _} | _] ->
          throw({error, L2, {duplicate_fun_sig_decl, N, L1, L2}});
        _ ->
          St#state{fun_sigs = dict:append(N, F, FS)}
      end;
    error ->
      St#state{fun_sigs = dict:append(N, F, FS)}
  end.

%% Adds a new type alias to state record or throws an exception if this
%% type alias is already defined.
add_type_alias({type_alias, L2, N, _} = F, St=#state{type_aliases = TA}) ->
  case dict:find(N, TA) of
    {ok, {_, L1, _, _}} ->
      throw({error, L2, {duplicate_type_alias_decl, N, L1, L2}});
    error ->
      St#state{type_aliases = dict:store(N, F, TA)}
  end.

%% Adds a new type constructor to state record or throws an exception if this
%% type cons is already defined by another type cons or type alias.
add_type_cons({type_cons, L2, N, _P, _T} = F, St=#state{ type_cons = TC
                                                     , type_aliases = TA}) ->
  case dict:find(N, TC) of
    {ok, {_, L1, _, _, _}} ->
      throw({error, L2, {duplicate_type_cons_decl, N, L1, L2}});
    error ->
      case dict:find(N, TA) of
        {ok, {_, L3, _, _, _}} ->
          throw({error, L2, {duplicate_type_cons_decl, N, L2, L3}});
        error ->
          St#state{type_cons = dict:store(N, F, TC)}
      end
  end.

add_declared_fun({function, _, N, _, _} = F, St=#state{declared_fun = DF}) ->
  St#state{declared_fun = dict:append(N, F, DF)}.

check_consistency_type_cons(#state{type_cons = TC}, FN) ->
  lists:foreach(fun({_, E}) ->
                    check_consistency_type_cons_lhs_rhs(E, FN),
                    no_terl_type_used_lhs(E)
                end, dict:to_list(TC)).

%% Check if that all defined generic type parameters in the left hand side of
%% type constructor is used in the right hand side and vice versa
check_consistency_type_cons_lhs_rhs({type_cons, L, _N, Is, O}, FN) ->
  GTI0 = lists:flatten([extract_generic_types(I) || I <- Is]),
  GTO0 = extract_generic_types(O),
  GTI1 = gb_sets:to_list(gb_sets:from_list(GTI0)),
  GTO1 = gb_sets:to_list(gb_sets:from_list(GTO0)),
  NotUsedRhs = GTI1 -- GTO1,
  NotUsedLhs = GTO1 -- GTI1,
  case NotUsedLhs =:= NotUsedRhs of
    true -> ok;
    false ->
      Errs = [{FN, [{L, ?TYPE_MSG, {tc_generic_type_not_used_lhs, NotUsedLhs}}]}
       || length(NotUsedLhs) =/= 0] ++
        [{FN, [{L, ?TYPE_MSG, {tc_generic_type_not_used_rhs, NotUsedRhs}}]}
         || length(NotUsedRhs) =/= 0],
     throw(Errs)
  end.

no_terl_type_used_lhs({type_cons, L, _N, Is, _O}) ->
  TI0 = lists:flatten([type_terminals(I) || I <- Is]),
  TI1 = lists:filter(fun({Tag, _}) -> Tag =/= terl_generic_type end, TI0),
  case length(TI1) of
    0 ->
      ok;
    _ ->
      TI2 = lists:map(fun(E) -> element(2, E) end, TI1),
      throw({error, L, {tc_only_generic_type_lhs, TI2}})
  end.

fun_arity({fun_sig, _, _, {fun_type, I, _}}) ->
  length(I).

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
type_terminals(W) ->
  throw({fatal_error, not_recognized, W}).


%%%%%%%% Type check, scary stuff! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(scope, { local = dict:new()
               , global = dict:new()
               , state = #state{}
               , final = false}).

type_check0(Forms, State) ->
  Scope0 = #scope{state = State},
  Scope1 = type_check1(Forms, Scope0),
  Scope2 = Scope1#scope{local = dict:new(), final = true},
  type_check1(Forms, Scope2).

type_check1([], Scope) ->
  Scope;

type_check1([{function, _L, _N, _A, Cls} | Forms], Scope) ->
  type_check1(Cls, Scope),
  type_check1(Forms, Scope);

type_check1([{clause, _L,  _A, _G, Exprs} | Forms], Scope) ->
  type_check1(Exprs, Scope#scope{local = dict:new()}),
  type_check1(Forms, Scope);

type_check1([{match, _L, LHS, RHS} | Exprs], Scope0) ->
  Inferred = type_of(RHS, Scope0),
  VarTypes = reduce(LHS, Inferred, []),
  Scope1 = update_local(Scope0, VarTypes),
  type_check1(Exprs, Scope1);

type_check1([{match, L, {var, _, Var} = V, Type, RHS} | Exprs], Scope0) ->
  Inferred = type_of(RHS, Scope0),
  assert_type_equality(Var, L, Type, Inferred),
  Scope1 = update_local(Scope0, V, Inferred),
  type_check1(Exprs, Scope1);

type_check1([_ | Fs], Scope) ->
  type_check1(Fs, Scope).

%% Tries to pattern match LHS and RHS and infer type
%% for a variable in LHS from RHS. For sake of error handling
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



type_of({nil, _}, _) ->
  {list_type, nothing};

type_of({integer, _, _}, _) ->
  type_internal:tag_built_in(integer);

type_of({float, _, _}, _) ->
  type_internal:tag_built_in(float);

type_of({atom, _, _}, _) ->
  type_internal:tag_built_in(atom);

type_of({string, _, _}, _) ->
  type_internal:tag_built_in(string);

type_of({op, L, Op, LHS, RHS}, Scope) ->
  TL = type_of(LHS, Scope),
  is_valid_type(LHS, TL, Scope),
  TR = type_of(RHS, Scope),
  is_valid_type(RHS, TR, Scope),
  Res = type_internal:dispatch(TL, Op, TR),
  assert_operator_validity(Res, Op, TL, TR, L);

type_of({op, L, Op, RHS}, Scope) ->
  TR = type_of(RHS, Scope),
  is_valid_type(RHS, TR, Scope),
  Res = type_internal:dispatch(Op, TR),
  assert_operator_validity(Res, Op, TR, L);

type_of({var, _L, Var} = F, #scope{local = LD} = S) ->
  T = case dict:find(Var, LD) of
        {ok, V} -> V;
        error -> undefined
      end,
  is_valid_type(F, T, S),
  T;

type_of({cons, L, H, T}, Scope) ->
  TH = type_of(H, Scope),
  is_valid_type(H, TH, Scope),
  TT = unwrap_list(T, type_of(T, Scope), L),
  is_valid_type(T, TT, Scope),
  assert_list_validity(TT, TH, L);

type_of({tuple, L, Es}, Scope) ->
  TEs = [ begin
            T = type_of(E, Scope),
            is_valid_type(E, T, Scope),
            T
          end || E <- Es],
  assert_tuple_validity(TEs, L);

type_of(T, _) ->
  io:format("type_of ~p not implemented", [T]),
  undefined.

is_valid_type(E, T, S) ->
  is_valid_type1(E, T, S#scope.final).

is_valid_type1(E, T, true) ->
  case T of
    undefined ->
      throw({error, element(2, E), {can_not_infer_type, E}});
    _ ->
      ok
  end;
is_valid_type1(_, _, false) ->
  ok.


unwrap_list(_, {list_type, T}, _) ->
  T;
unwrap_list(_, undefined, _) ->
  undefined;
unwrap_list(_, T, L) ->
  throw({error, L, {not_list_cons_position, T}}).


assert_list_validity(TT, TH, L) ->
  case {TH, TT} of
    {undefined, _}         -> undefined;
    {_, undefined}         -> undefined;
    {undefined, undefined} -> undefined;
    {_, nothing}           -> {list_type, TH};
    {T1, T2} ->
      case T1 =:= T2 of
        true -> {list_type, T1};
        false ->
          throw({error,
                L, {heterogeneous_list_not_supported, T1, T2}})
      end
  end.

assert_tuple_validity(TES, _L) ->
  Undefined = lists:filter(fun(T) ->
                   T =:= undefined
               end, TES),
  case length(Undefined) of
    0 -> {tuple_type, TES};
    _ -> undefined
  end.


assert_type_equality(Var, L, Declared, Inferred) ->
  case Inferred of
    undefined ->
      ok;
    T ->
      case T =:= Declared of
        true -> ok;
        false ->
          throw({error,
                 L, {declared_inferred_not_match, Var, Declared, Inferred}})
      end
  end.

assert_operator_validity(Res, Op, TL, TR, L) ->
  InvalidOp = type_internal:invalid_operator(),
  case Res of
    InvalidOp ->
      throw({error, L, {invalid_operator, Op, TL, TR}});
    R ->
      R
  end.

assert_operator_validity(Res, Op, TR, L) ->
  InvalidOp = type_internal:invalid_operator(),
  case Res of
    InvalidOp ->
      throw({error, L, {invalid_operator, Op, TR}});
    R -> R
  end.

update_local(S=#scope{local = LD, final = false}, Var, Type) ->
  {var, _, V} = Var,
  case Type of
    undefined ->
      S;
    _ ->
      io:format("Var=~p, Type=~s~n", [V, pp_type(Type)]),
      S#scope{local = dict:store(V, Type, LD)}
  end;
update_local(S=#scope{local = LD, final = true}, Var, Type) ->
  {var, L, V} = Var,
  case Type of
    undefined ->
      throw({error, L, {can_not_infer_type, V}});
    _ ->
      io:format("Var=~p, Type=~s~n", [Var, pp_type(Type)]),
      S#scope{local = dict:store(V, Type, LD)}
  end.

update_local(S, VarTypes) ->
  lists:foldl(fun({K, T}, Acc) ->
                 update_local(Acc, K, T)
             end, S, VarTypes).



%%% Format errors
format_error({duplicate_fun_sig_decl, N, L1, L2}) ->
  io_lib:format(
    "Duplicate function signature definitions '~w' at line ~p and ~p.",
    [N, L1, L2]);
format_error({duplicate_type_alias_decl, N, L1, L2}) ->
  io_lib:format(
    "Type alias definition '~w' at line ~p is already defined with "
    ++ "the same name at ~p.",
    [N, L2, L1]);
format_error({duplicate_type_cons_decl, N, L1, L2}) ->
  io_lib:format(
    "Type constructor definition '~w' at line ~p is already defined"
    ++  " with same name at ~p.",
    [N, L2, L1]);
format_error({no_fun_decl_found_for_sig, N, L2}) ->
  io_lib:format(
    "No function implementation found for declared function signature '~w' "
    ++ "at line ~p.",
    [N, L2]);
format_error({no_matching_fun_decl_for_fun_sig, N, Ar, L2}) ->
  io_lib:format(
    "No function implementation matched with declared function signature "
    ++ "'~w'/~p at line ~p.",
    [N, Ar, L2]);
format_error({multi_match_fun_decl_for_fun_sig, N, L2}) ->
  io_lib:format(
    "Multiple function implementations matched with declared function"
    ++ " signature '~w' at line ~p. This is a fatal error in compiler!",
    [N, L2]);
format_error({type_alias_defined_not_used, N, L}) ->
  io_lib:format(
    "Type alias '~w' defined at line ~p but never used",
    [N, L]);
format_error({undefined_type, N}) ->
  io_lib:format(
    "Undefined type '~w'",
    [N]);
format_error({tc_generic_type_not_used_rhs, Ts}) ->
  io_lib:format(
    "Generic type parameter(s) ~s is not used in the right hand side of"
    ++ " type constructor",
    [list_to_string(Ts, "")]);
format_error({tc_generic_type_not_used_lhs, Ts}) ->
  io_lib:format(
    "Generic type parameter(s) ~s is not defined in the left hand side of"
    ++ " type constructor",
    [list_to_string(Ts, "")]);
format_error({tc_only_generic_type_lhs, TI}) ->
  io_lib:format(
    "Only generic type parameters are allowed in left hand side of "
    ++ "type constructor definitions. ~s has/have violated this rule.",
    [list_to_string(TI, "")]);
format_error({declared_inferred_not_match, Var, Declared, Inferred}) ->
  io_lib:format(
    "Expected variable ~p to be of type ~s but is ~s",
    [Var, pp_type(Declared), pp_type(Inferred)]);
format_error({invalid_operator, Op, TL, TR}) ->
  io_lib:format(
    "Invalid operator ~p on types ~s and ~s",
    [Op, pp_type(TL), pp_type(TR)]);
format_error({invalid_operator, Op, TR}) ->
  io_lib:format(
    "Illegal operator ~p on type ~s",
    [Op, pp_type(TR)]);
format_error({heterogeneous_list_not_supported, T1, T2}) ->
  io_lib:format(
    "List of heterogeneous types of ~s and ~s is not allowed",
    [pp_type(T1), pp_type(T2)]);
format_error({can_not_infer_type, E}) ->
  io_lib:format(
    "Could not infer the type for ~s",
    [pp_expr(E)]);
format_error({not_list_cons_position, T}) ->
  io_lib:format(
    "Expected a type of list in cons position but found ~s",
    [pp_type(T)]);
format_error({match_on_unequally_sized_tuple, T}) ->
  io_lib:format(
    "Match on different tuple sizes. Right hand side tuple ~s is " ++
    "different from left hand side",
    [pp_type(T)]);
format_error(W) ->
  io_lib:format("Undefined Error in type system: ~p ", [W]).

list_to_string([], Res) ->
  Res;
list_to_string([H], Res) ->
  list_to_string([], io_lib:format("~s~p", [Res, atom_to_list(H)]));
list_to_string([H|[_, _] = T], Res) ->
  list_to_string(T, io_lib:format("~s~p, ", [Res, atom_to_list(H)]));
list_to_string([H|[_] = T], Res) ->
  list_to_string(T, io_lib:format("~s~p and ", [Res, atom_to_list(H)])).


pp_type({terl_type, T}) ->
  io_lib:format("~s", [T]);
pp_type({list_type, T}) ->
  io_lib:format("[~s]", [pp_type(T)]);
pp_type({tuple_type, Ts}) ->
  TT = [pp_type(T) || T <- Ts],
  io_lib:format("{~s}", [list_to_string_sep(TT, $,)]);
pp_type(T) ->
  T.

pp_expr({var, _, V}) ->
  io_lib:format("'~s'", [V]);
pp_expr(V) ->
  io_lib:format("~p", [V]).

list_to_string_sep(List, Sep) ->
  lists:flatten(lists:reverse(list_to_string_sep1(List, Sep, []))).

list_to_string_sep1([Head | []], _Sep, Acc) ->
  [Head | Acc];
list_to_string_sep1([Head | Tail], Sep, Acc) ->
  list_to_string_sep1(Tail, Sep, [Sep, Head | Acc]).
