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

-export([calc_score/2]).


-record(state, { fun_sigs      = dict:new()
                 %% {key, [{fun_sig, L, N, [T]}]}
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
      io:format("Backtrace ~p~n", [erlang:get_stacktrace()]),
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
  check_consistency_fun_sigs(St, FileName),
  Ws1 = check_unsued_user_defined_types(FileName, St),
  check_undefined_types(FileName, St),
  match_fun_sig_with_declared_fun(St),
  %% TODO: checks for generic types
  %% - RHS usage
  %% - Type expansion: type instances, type aliases
  {ok, Ws1, St}.

type_check(Forms, FileName, _Opts, State) ->
  type_check0(Forms, FileName, State).

%% Collect forms of interest into state record
collect_types([{fun_sig, L, _, Ts} = Form | Forms], St0) ->
  St1 = extract_user_defined_types_with_locs(Ts, L, St0),
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
                     {type_alias_defined_not_used, N}}]}]
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


%% All fun sigs clauses must have same arity
check_consistency_fun_sigs(#state{fun_sigs = FS}, _FN) ->
 [check_consistency_fun_sigs0(Sig) || {_, Sigs} <- dict:to_list(FS),
                                      Sig <- Sigs].

check_consistency_fun_sigs0({fun_sig, L, N, Cls}) ->
  SetArity = gb_sets:from_list([fun_arity(Cl) || Cl <- Cls]),
  case gb_sets:size(SetArity) of
    1 ->
      ok;
    _ ->
      throw({error, L, {fun_sig_clause_arity_not_match, N}})
  end.

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

fun_arity({fun_sig, _, _, [{fun_type, I, _} | _]}) ->
  length(I);
fun_arity({fun_type, I, _}) ->
  length(I);
fun_arity([{fun_type, I, _} | _]) ->
  length(I).


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
type_terminals(undefined) ->
  [undefined];
type_terminals(W) ->
  throw({fatal_error, not_recognized, W}).


%%%%%%%% Type check, scary stuff! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(meta_var, { type = undefined
                  , line = -1}).

-record(local_scope, { args         = dict:new()
                       %% vars :: Dict(Var, #meta_var)
                     , vars         = dict:new()
                     , outer_scope  = nil}).

-record(scopes, { local             = #local_scope{}
                , locals            = []
                , global            = dict:new()
                , state             = #state{}
                , errors            = []
                , final             = false}).


type_check0(Forms, FileName, State) ->
  Scopes0 = #scopes{state = State},
  %% Pass 1:
  Scopes1 = type_check1(Forms, Scopes0),

  %% Pass 2:
  #scopes{errors = Errs0} = type_check1(Forms, Scopes1#scopes{final = true}),
  Errs = lists:sort(fun({L1, _, _}, {L2, _, _}) -> L1 < L2 end
                   , gb_sets:to_list(gb_sets:from_list(Errs0))),

  case length(Errs) of
    0 -> {ok, [], State};
    _ -> {error, [{FileName, Errs}], []}
  end.

type_check1([], Scopes) ->
  Scopes;

type_check1([{function, L, N, A, Cls} | Forms], Scopes) ->
  {ClauseSig, Scopes1} = match_clauses_with_sig(N, A, Cls, Scopes),
  %% InferredTypeFSig =:= {Line, fun_type, fun_type}
  {InferredTypeFSig, Scopes2} =
    lists:foldl(fun({Cl, Sig}, {Ts, S0}) ->
                    {T, S1} = type_check_clause(Sig, Cl, S0),
                    %% Also save the computed local scope with forms?
                    L2 = element(2, Cl),
                    S2 = change_local_scope(S1),
                    {[{L2, T, Sig} | Ts], S2}
                end, {[], Scopes1}, ClauseSig),

  {FType, Scopes3} = infer_function_type({N, A}, InferredTypeFSig, Scopes2),
  Scopes4 = validate_fun_type({N, A, L}, FType, Scopes3),
  type_check1(Forms, Scopes4);

type_check1([_ | Fs], Scopes) ->
  type_check1(Fs, Scopes).


%% Returns [{clause, fun_type}]
match_clauses_with_sig(N, A, Cls, Scopes) ->
  Sig = find_fun_sig(N, A, Scopes),
  case Sig of
    {undefined, Scopes} ->
      lists:map(fun(E) -> {E, undefined} end, Cls);
    FSs ->
      match_clauses_with_sig0(Cls, FSs, [], Scopes)
  end.

%% Returns [{clause, fun_Type}]
match_clauses_with_sig0([], _, Res, Scopes) ->
  {Res, Scopes};
match_clauses_with_sig0([C | Cls], {fun_sig, L, N, Sigs}, Res, Scopes) ->
  Scores = lists:map(fun(S0) ->
                         {calc_score(S0, C), S0}
                     end, Sigs),
  SortedScores = lists:sort(fun({A, _}, {B, _}) ->
                                B =< A
                            end, Scores),
  %% Always picks the first best match. In case of multiple match with
  %% the same rank, it's developer's responsibility to declare fun sigs
  %% in correct order
  [{_Rank, BestMatch} | _] = SortedScores,
  match_clauses_with_sig0(Cls, {fun_sig, L, N, Sigs -- [BestMatch]},
                          Res ++ [{C, BestMatch}], Scopes).

%% Returns [{fun_sig, L, N, T}] | [undefined]
find_fun_sig(N, A, #scopes{state = State}) ->
  case dict:find(N, State#state.fun_sigs) of
    {ok, Vs} ->
      hd([FS || {fun_sig, _, _, T} = FS <- Vs, fun_arity(T) =:= A]
         ++ [undefined]);
    error -> undefined
  end.

calc_score({fun_type, SigArg, _},
           {clause, _, CArg, _, _}) ->
  Args = lists:zip(SigArg, CArg),
  lists:foldl(fun({A1, A2}, Acc) ->
                  Acc + arg_match(A1, A2)
              end, 0, Args).

arg_match({terl_atom_type, T}, {atom, _, T}) ->
  1;
arg_match({terl_tuple, Ts1}, {terl_tuple, Ts2}) ->
  case length(Ts1) =:= length(Ts2) of
    true -> 1 + arg_match(Ts1, Ts2);
    false -> 0
  end;
arg_match([_ | _] = L1, [_ | _] = L2) ->
  case length(L1) =:= length(L2) of
    true -> lists:sum([arg_match(A, B) || {A, B} <- lists:zip(L1, L2)]);
    false -> 0
  end;
arg_match({terl_list, T1}, {terl_list, T2}) ->
  arg_match(T1, T2);
arg_match(T, T) ->
  1;
arg_match(_, _) ->
  0.


infer_function_type(NA, InferredTypeFSigList, Scopes) ->
  lists:foldl(fun({L, FType, FSig}, {Ts, S0}) ->
                  {N, A} = NA,
                  {FT, S1} = infer_function_clause({N, A, L}, FType, FSig, S0),
                  {Ts ++ [FT], S1}
              end, {[], Scopes}, InferredTypeFSigList).

infer_function_clause(_, FT, undefined, S) ->
  {FT, S};
infer_function_clause(NAL, FT1, FT2, S=#scopes{final = Final}) ->
  case type_equivalent(FT1, FT2) of
    true -> {FT2, S};
    false ->
      case Final of
        true ->
          {N, A, L} = NAL,
          Err = {L, ?TYPE_MSG,
                 {declared_inferred_fun_type_do_not_match, N, A, FT2, FT1}},
          {undefined, S#scopes{errors = S#scopes.errors ++ [Err]}};
        false ->
          {undefined, S}
      end
  end.

validate_fun_type(NAL, FTypes, Scopes) ->
  lists:foldl(fun(FT, S0) ->
                  validate_fun_type0(NAL, FT, S0)
              end, Scopes, FTypes).

validate_fun_type0(NAL, FType, S=#scopes{final = true}) ->
  Undefs = extract_type_terminals(undefined, FType),
  case length(Undefs) of
    0 -> S;
    _ ->
      {N, A, L} = NAL,
      Err = {L, ?TYPE_MSG, {can_not_infer_fun_type, N, A, FType}},
      S#scopes{errors = S#scopes.errors ++ [Err]}
  end;
validate_fun_type0(_, _, S=#scopes{final = false}) ->
  S.

type_check_clause(undefined, {clause, _L, Args, _G, _E} = Cl, Scope0) ->
  Scope1 = lists:foldl(fun(Var, S0) ->
                            insert_args(Var, undefined, S0)
                        end, Scope0, [A || {Kind, _, _} = A
                                              <- Args, Kind =:= var]),
  type_check_clause0(Cl, Scope1);

type_check_clause({fun_type, Is, _},
                  {clause, _, Args, _, _} = Cl, Scopes0) ->
  VarTypes = lists:foldl(fun({LHS, RHS}, Acc) ->
                             reduce(LHS, RHS, Acc)
                         end, [], lists:zip(Args, Is)),
  %% TODO: validity of declated types for args
  Scopes1 = lists:foldl(fun({V, T}, S0) ->
                            insert_args(V, T, S0)
                        end, Scopes0, VarTypes),
  type_check_clause0(Cl, Scopes1).

%% function clause
type_check_clause0({clause, _L, Args, _G, Exprs}, Scopes0) ->
  {TOut, Scopes1} = lists:foldl(fun(Expr, {_, S0}) ->
                            type_check_expr(Expr, S0)
                        end, {nil, Scopes0}, Exprs),
  TIn = [element(1, type_of(Arg, Scopes1)) || Arg <- Args],
  {{fun_type, TIn, TOut}, check_local_scope(Scopes1)}.


type_check_expr({match, _L, LHS, RHS}, Scopes0) ->
  {Inferred, Scopes1} = type_of(RHS, Scopes0),
  VarTypes = reduce(LHS, Inferred, []),
  Scopes2 = update_local(Scopes1, VarTypes),
  {Inferred, Scopes2};
type_check_expr({match, L, {var, _, Var} = V, Type, RHS}, Scopes0) ->
  {Inferred, Scopes1} = type_of(RHS, Scopes0),
  assert_type_equality(Var, L, Type, Inferred),
  {_, Scopes2} = update_local(Scopes1, V, Inferred),
  {Inferred, Scopes2};
type_check_expr(E, Scopes0) ->
  type_of(E, Scopes0).

insert_args({var, _, '_'}, _, S) ->
  S;
insert_args({var, L, Var}, Type, S=#scopes{local = LS}) ->
  case {recursive_lookup(Var, LS), Type} of
    {undefined, undefined} -> S;
    {undefined, _}         -> insert_args0(Var, L, Type, S);
    {_,         undefined} -> S;
    {_,         _}         -> insert_args0(Var, L, Type, S)
    end.

insert_args0(Var, L, Type, S) ->
  LS = (S#scopes.local),
  io:format("Var=~p, Type=~s~n", [Var, pp_type(Type)]),
  MetaVar = #meta_var{type = Type, line = L},
  S#scopes{local =
             LS#local_scope{vars =
                              dict:store(Var, MetaVar, LS#local_scope.vars)}}.

check_local_scope(S=#scopes{final = false}) ->
  S;
check_local_scope(S=#scopes{final = true, local = #local_scope{vars = Vars}}) ->
  Undefs = [{L, ?TYPE_MSG, {can_not_infer_type, V}}
            || {V, #meta_var{type = T, line = L}}
                 <- dict:to_list(Vars), T =:= undefined],
  S#scopes{errors = S#scopes.errors ++ Undefs}.

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



type_of({nil, _}, S) ->
  {{list_type, nothing}, S};

type_of({integer, _, _}, S) ->
  {type_internal:tag_built_in('Integer'), S};

type_of({float, _, _}, S) ->
  {type_internal:tag_built_in('Float'), S};

type_of({atom, _, T}, S) ->
  {{terl_atom_type, T}, S};

type_of({string, _, _}, S) ->
  {type_internal:tag_built_in('String'), S};

type_of({var, _L, '_'}, Scopes0) ->
  {type_internal:tag_built_in('Any'), Scopes0};

type_of({op, L, Op, LHS, RHS}, Scopes0) ->
  {TL0, Scopes1} = type_of(LHS, Scopes0),
  {TR0, Scopes2} = type_of(RHS, Scopes1),
  Res = dispatch(TL0, Op, TR0),
  {TL1, Scopes3} = infer_from_op(Res, LHS, TL0, Scopes2),
  {TR1, Scopes4} = infer_from_op(Res, RHS, TR0, Scopes3),
  {assert_operator_validity(Res, Op, TL1, TR1, L), Scopes4};

type_of({op, L, Op, RHS}, Scopes0) ->
  {TR, Scopes1} = type_of(RHS, Scopes0),
  Res = dispatch(Op, TR),
  {assert_operator_validity(Res, Op, TR, L), Scopes1};

type_of({var, _L, Var}, #scopes{local = LS} = S) ->
  T = recursive_lookup(Var, LS),
  {T, S};

type_of({cons, L, H, T}, Scopes0) ->
  {TH, Scopes1} = type_of(H, Scopes0),
  {TT0, Scopes2} = type_of(T, Scopes1),
  TT1 = unwrap_list(T, TT0, L),
  {assert_list_validity(TH, TT1, L), Scopes2};

type_of({tuple, L, Es}, Scopes0) ->
  {TEs, Scopes1} = lists:foldl(fun(E, {Ts, Scopes}) ->
                                  {T, Scopes1} = type_of(E, Scopes),
                                  {Ts ++ [T], Scopes1}
                              end, {[], Scopes0}, Es),
  {assert_tuple_validity(TEs, L), Scopes1};

type_of(T, Scopes) ->
  io:format("type_of ~p not implemented", [T]),
  {undefined, Scopes}.

dispatch(TL, Op, TR) ->
  dispatch_result(type_internal:dispatch(TL, Op, TR)).

dispatch(Op, TR) ->
  dispatch_result(type_internal:dispatch(Op, TR)).

dispatch_result(Res) ->
  case Res of
    Ls when is_list(Ls) ->
      Ts = gb_sets:to_list(gb_sets:from_list(Ls)),
      case length(Ts) of
        1 ->
          hd(Ts);
        _ ->
          undefined
      end;
    T ->
      T
  end.


recursive_lookup(_, nil) ->
  undefined;
recursive_lookup(Var, #local_scope{vars = Vars, outer_scope = OS}) ->
  case dict:find(Var, Vars) of
    {ok, #meta_var{type = T}} -> T;
    error   -> recursive_lookup(Var, OS)
  end.


unwrap_list(_, {list_type, T}, _) ->
  T;
unwrap_list(_, undefined, _) ->
  undefined;
unwrap_list(_, T, L) ->
  throw({error, L, {not_list_cons_position, T}}).


assert_list_validity(TH, TT, L) ->
  case {TH, TT} of
    {undefined, _}          -> undefined;
    {_, undefined}          -> undefined;
    {undefined, undefined}  -> undefined;
    {_, nothing}            -> {list_type, TH};
    {{terl_type, 'Any'}, _} -> {list_type, TT};
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
      case type_equivalent(T, Declared) of
        true -> ok;
        false ->
          throw({error,
                 L, {declared_inferred_not_match, Var, Declared, Inferred}})
      end
  end.

infer_from_op(Res, {var, _, _} = Var, undefined, Scopes) ->
  update_local(Scopes, Var, Res);
infer_from_op(Res, {var, _, _} = Var, {union_type, _}, Scopes) ->
  update_local(Scopes, Var, Res);
infer_from_op(_, _, Type, Scopes) ->
  {Type, Scopes}.

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

%% Returns {Type, Scopes}
update_local(S=#scopes{final = Final}, {var, L, V}, Type) ->
  MetaVar = #meta_var{type = Type, line = L},
  LS = (S#scopes.local),
  S1 = S#scopes{local =
                  LS#local_scope{vars =
                                   dict:store(V
                                             , MetaVar
                                             , LS#local_scope.vars)}},
  case Type of
    undefined ->
      io:format("Var=~p, Type=undefined~n", [V]),
      {Type,
       S1#scopes{errors =
                  S#scopes.errors ++
                  [{L, ?TYPE_MSG, {can_not_infer_type, V}}
                   || Final =:= true andalso type_defined_in_local(V, S)]}};
    _ ->
      io:format("Var=~p, Type=~s~n", [V, pp_type(Type)]),
      {Type, S1}
  end.

%% This is to avoid same variables errored multiple places
type_defined_in_local(Var, #scopes{local = LS}) ->
  case recursive_lookup(Var, LS) of
    undefined -> false;
    _         -> true
  end.

%% Only returns Scope
update_local(S0, VarTypes) ->
  lists:foldl(fun({K, T}, Acc) ->
                  {_, S1} = update_local(Acc, K, T),
                  S1
             end, S0, VarTypes).

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
type_equivalent({tuple_type, Ts1}, {tuple_type, Ts2}) ->
  (length(Ts1) =:= length(Ts2)) andalso
    lists:all(fun(E) -> E =:= true end,
              [type_equivalent(T1, T2) || {T1, T2} <- lists:zip(Ts1, Ts2)]);
type_equivalent(T, T) ->
  true;
type_equivalent(_, _) ->
  false.

change_local_scope(S=#scopes{local = L, locals = LS, final = F}) ->
  case F of
    true ->
      [H|T] = LS,
      S#scopes{local = H, locals = T};
    false ->
      S#scopes{local = #local_scope{}, locals = S#scopes.locals ++ [L]}
  end.

nest_local_scope(S=#scopes{local = L, locals = LS, final = F}) ->
  case F of
    true ->
      [H|T] = LS,
      S#scopes{local = H, locals = T};
    false ->
      S#scopes{local = #local_scope{outer_scope = L},
               locals = S#scopes.locals ++ [L]}
  end.

%%% Format errors --------------------------------------------------------------

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
format_error({fun_sig_clause_arity_not_match, N}) ->
  io_lib:format(
    "Function signature ~w has clauses with different arity.",
   [N]);
format_error({multi_match_fun_decl_for_fun_sig, N, L2}) ->
  io_lib:format(
    "Multiple function implementations matched with declared function"
    ++ " signature '~w' at line ~p. This is a fatal error in compiler!",
    [N, L2]);
format_error({type_alias_defined_not_used, N}) ->
  io_lib:format(
    "Type alias ~w defined but never used",
    [N]);
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
format_error({can_not_infer_fun_type, N, A, FType}) ->
  io_lib:format(
    "Type system did its best to infer the type for '~p/~p' " ++
      "and what it got was '~s'.",
    [N, A, pp_type(FType)]);
format_error({declared_inferred_fun_type_do_not_match, N, A, Sig, FType}) ->
  io_lib:format(
    "Declared function signature for '~p/~p' does not match the inferred " ++
      "one.~n\t\tExpected:~n\t\t\t'~s'~n\t\tbut inferred:~n\t\t\t'~s'.",
    [N, A, pp_type(Sig), pp_type(FType)]);
format_error({multiple_inferred_type, Expr, Ts}) ->
  io_lib:format(
    "Multiple types can be inferred for '~s':~n\t\t~s",
    [pp_expr(Expr), list_to_string_sep([pp_type(T) || T <- Ts], ", ")]);
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
pp_type({fun_type, Is, O}) ->
  TIs = [pp_type(I) || I <- Is],
  io_lib:format("(~s) -> ~s", [list_to_string_sep(TIs, ", "), pp_type(O)]);
pp_type({union_type, Ts}) ->
  TEs = [pp_type(T) || T <- Ts],
  io_lib:format("~s", [list_to_string_sep(TEs, " | ")]);
pp_type({terl_atom_type, T}) ->
  io_lib:format("~s", [T]);
pp_type(undefined) ->
  "undefined";
pp_type(T) ->
  T.

pp_expr({var, _, V}) ->
  io_lib:format("~s", [V]);
pp_expr({integer, _, V}) ->
  io_lib:format("~p", [V]);
pp_expr({atom, _, V}) ->
  io_lib:format("~p", [V]);
pp_expr({op, _, Op, L, R}) ->
  io_lib:format("~s ~s ~s", [pp_expr(L), Op, pp_expr(R)]);
pp_expr(V) ->
  io_lib:format("~p", [V]).

list_to_string_sep(List, Sep) ->
  lists:flatten(lists:reverse(list_to_string_sep1(List, Sep, []))).

list_to_string_sep1([Head | []], _Sep, Acc) ->
  [Head | Acc];
list_to_string_sep1([Head | Tail], Sep, Acc) ->
  list_to_string_sep1(Tail, Sep, [Sep, Head | Acc]).
