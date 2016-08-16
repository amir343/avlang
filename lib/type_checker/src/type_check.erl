
%% Current abstract forms from erl_parser:
%%
%% {fun_sig, Line, Name, Type}
%% {fun_type, Is, O}
%% {untyped_fun, nil, nil}
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
        ]).

-export([find_lcs/1]).

-record(state, { fun_sigs         = dict:new()
                 %% {key, [{fun_sig, L, N, [T]}]}
               , remote_fun_sigs  = dict:new()
                 %% {key, [{fun_remote_sig, L, M, Fun, [T]}]}
               , type_aliases     = dict:new()
                 %% {Key, {type_alias, L, N, T}}
               , type_cons        = dict:new()
                 %% {Key, {type_cons, L, N, P, T}}
               , declared_fun     = dict:new()
                 %% {Key, [{function, ...}]}
               , type_used        = gb_sets:new()
                 %% Set(atom)
               , type_used_loc    = dict:new()
                 %% {Key, [Line]}
               , errors           = []
               , compiler_opts    = []
               , erlang_types     = dict:new()
               , guard_types      = dict:new()
         }).

-define(TYPE_MSG, type_err_msg).

module(Forms, FileName, Opts0) ->
  Opts1 = type_check_compiler_opts:options_of_interest(Opts0),
  ErlangTypes = bootstrap_erlang_types(),
  ErlangGuardTypes = erlang_guard_signature(),
  run_passes(standard_passes(),
             Forms,
             FileName,
             [],
             #state{ erlang_types = ErlangTypes
                   , guard_types = ErlangGuardTypes
                   , compiler_opts = Opts1}).

bootstrap_erlang_types() ->
  PrivDir = code:lib_dir(type_checker, priv),
  {ok, [Term | _]} = file:consult(filename:join(PrivDir, "erlang_types.eterm")),
  ParsedSignature = [begin
                       try
                         {ok, Tokens, _} = erl_scan:string(T),
                         {ok, ParsedTokens} = erl_parse:parse(Tokens),
                         ParsedTokens
                       catch
                           _:E ->
                           io:format("Syntax error: ~p~n", [T]),
                           throw(E)
                       end
                     end || T <- Term],
  {Sigs, _} =
    lists:foldl(
      fun({fun_remote_sig, _, M, N, Ts}, {Dict1, Dict2}) ->
          Key = atom_to_list(M) ++ "_" ++ atom_to_list(N),
          {dict:append(Key,
                       [substitute_type_alias(T, Dict2) || T <- Ts]
                      , Dict1), Dict2};
         ({fun_sig, _, N, Ts}, {Dict1, Dict2}) ->
          {dict:append(atom_to_list(N),
                       [substitute_type_alias(T, Dict2) || T <- Ts]
                      , Dict1), Dict2};
         ({type_alias, _, N, T}, {Dict1, Dict2}) ->
          {Dict1, dict:store(N, substitute_type_alias(T, Dict2), Dict2)}
      end, {dict:new(), dict:new()}, ParsedSignature),
  Sigs.

erlang_guard_signature() ->
  PrivDir = code:lib_dir(type_checker, priv),
  {ok, [Term | _]} =
    file:consult(filename:join(PrivDir, "erlang_guards.eterm")),
  lists:foldl(fun(T, Dict) ->
                  try
                    {ok, Tokens, _} = erl_scan:string(T),
                    {ok, {fun_sig, _, N, _Ts} = FT} = erl_parse:parse(Tokens),
                    dict:append(N, FT, Dict)
                  catch
                    _:E ->
                      io:format("Syntax error: ~p~n", [T]),
                      throw(E)
                  end
              end, dict:new(), Term).

substitute_type_alias(T, Aliases) ->
  type_internal:type_map(
    T, fun(Type) ->
           case Type of
             {terl_generic_type, N} ->
               case dict:find(N, Aliases) of
                 {ok, A} ->
                   A;
                 error -> Type
               end;
             _ -> Type
           end
       end).

run_passes([P | Ps], Fs, Fn, Ws0, State0) ->
  try
    case P(Fs, Fn, State0) of
      {ok, Ws, State1} ->
        run_passes(Ps, Fs, Fn, Ws0 ++ Ws, State1);
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
run_passes([], _, _, Ws, _) ->
  {ok, Ws}.

standard_passes() ->
  [ fun type_lint/3
  , fun type_check/3
  ].

type_lint(Forms, FileName, St0) ->
  St1 = collect_types(Forms, St0),
  debug_log(St1, "~p~n", [Forms]),
  check_consistency_type_cons(St1, FileName),
  check_consistency_fun_sigs(St1, FileName),
  St2 = no_remote_fun_sig_declared(St1),
  Ws1 = check_unsued_user_defined_types(FileName, St2),
  check_undefined_types(FileName, St2),
  match_fun_sig_with_declared_fun(St2),
  %% TODO: checks for generic types
  %% - RHS usage
  %% - Type expansion: type instances, type aliases
  Errs0 = St1#state.errors,
  Errs = lists:sort(fun({L1, _, _}, {L2, _, _}) -> L1 < L2 end
                   , gb_sets:to_list(gb_sets:from_list(Errs0))),

  case length(Errs) of
    0 -> {ok, Ws1, St1};
    _ -> {error, [{FileName, Errs}], []}
  end.

type_check(Forms, FileName, State) ->
  type_check0(Forms, FileName, State).

%% Collect forms of interest into state record
collect_types([{attribute, _, compile, Opts} | Forms], St0) ->
  St1 = insert_compiler_options(St0, Opts),
  collect_types(Forms, St1);
collect_types([{fun_sig, L, _, Ts} = Form | Forms], St0) ->
  St1 = extract_user_defined_types_with_locs(Ts, L, St0),
  collect_types(Forms, add_fun_sigs(Form, St1));
collect_types([{fun_remote_sig, _, _, _, _} = Form | Forms], St0) ->
  collect_types(Forms, add_remote_fun_sigs(Form, St0));
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

insert_compiler_options(St=#state{compiler_opts = Opts}, Options) ->
  Opts1 = type_check_compiler_opts:options_of_interest(Options),
  Opts2 = gb_sets:to_list(gb_sets:from_list(Opts ++ Opts1)),
  St#state{compiler_opts = Opts2}.

%% Extract user defined types given at line L and associate each one with
%% line L in the returned state.type_used_loc.
extract_user_defined_types_with_locs(T, L, St) ->
  Ts = type_internal:extract_user_defined_types(T),
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

%% Adds a new remote function signature to state record or throws
%% an exception if this function signature is already defined.
add_remote_fun_sigs({fun_remote_sig, L2, M, N, _} = F
                   , St=#state{remote_fun_sigs = FS}) ->
  Key = atom_to_list(M) ++ "_" ++ atom_to_list(N),
  case dict:find(Key, FS) of
    {ok, Vs} ->
      Ar = fun_arity(F),
      case [V || V <- Vs, fun_arity(V) =:= Ar] of
        [{_, L1, _, _} | _] ->
          throw({error, L2, {duplicate_fun_sig_decl, M, N, L1, L2}});
        _ ->
          St#state{remote_fun_sigs = dict:append(Key, F, FS)}
      end;
    error ->
      St#state{remote_fun_sigs = dict:append(Key, F, FS)}
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


no_remote_fun_sig_declared(S=#state{remote_fun_sigs = FSigs, errors = Errs}) ->
  Errors = [{L, ?TYPE_MSG, {no_remote_fun_sig_allowed, M, N}} ||
             {_, RFS} <- dict:to_list(FSigs),
             {fun_remote_sig, L, M, N, _} <- RFS],
  S#state{errors = Errs ++ Errors}.

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
  GTI0 = lists:flatten([type_internal:extract_generic_types(I) || I <- Is]),
  GTO0 = type_internal:extract_generic_types(O),
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
  TI0 = lists:flatten([type_internal:type_terminals(I) || I <- Is]),
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
fun_arity({fun_remote_sig, _, _, _, [{fun_type, I, _} | _]}) ->
  length(I);
fun_arity({fun_type, I, _}) ->
  length(I);
fun_arity([{fun_type, I, _} | _]) ->
  length(I).


%%%%%%%% Type check, the heart of the system %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(meta_var,
        { type                = undefined
        , line                = -1}).

-record(local_scope,
        { name                = nil
          %% vars :: Dict(Var, #meta_var)
        , vars                = dict:new()
        , type                = undefined
          %% Pointer to outer local scope name
        , outer_scope         = nil
        , inner_scopes        = gb_sets:new()
          %% last fun type signature for fun expression
        , last_ftype          = undefined
        , last_nr_undefined   = infinty}).

-record(scopes,
        { local               = nil
        , locals              = dict:new()
          %% global :: Dict(N, [[{fun_type}]])
        , global              = dict:new()
        , state               = #state{}
        , errors              = []
        , fun_lookup          = []
        , first_pass          = true}).


type_check0(Forms, FileName, State=#state{compiler_opts = Opts}) ->
  Scopes0 = #scopes{state = State},

  S = type_check_loop(1, Forms, Scopes0, -1),
  #scopes{errors = Errs0, locals = LS} = S,

  case type_check_compiler_opts:dump_local_scopes(Opts) of
    true -> dump_local_scopes(LS);
    false -> ok
  end,

  debug_log(S, ">>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<~n", []),
  debug_log(S, "Number of errors: ~p~n", [length(Errs0)]),
  debug_log(S, "Errors in raw: ~p~n", [Errs0]),

  %% Sort based on line numbers
  Errs = lists:sort(fun({L1, _, _}, {L2, _, _}) -> L1 < L2 end
                   , gb_sets:to_list(gb_sets:from_list(Errs0))),

  case length(Errs) of
    0 -> {ok, [], State};
    _ -> {error, [{FileName, Errs}], []}
  end.

%% TODO: how many iterations until we give up?
type_check_loop(10, _, S, _) ->
  S;
type_check_loop(PassN, Forms, Scopes0=#scopes{first_pass = FP}, PUndefs) ->
  debug_log(Scopes0,
            ">>>>>>>>>>>>>>>>>>>> PASS ~p <<<<<<<<<<<<<<<<<<<<<~n", [PassN]),

  Scopes1 = type_check1(Forms, Scopes0),
  %% Only for sake of debugging
  debug_log(Scopes0,
            "\t~~~~~~~~~~~~~~~~~~~~ Global scope ~~~~~~~~~~~~~~~~~~~~ ~n", []),
  lists:foreach(fun({N, FTypes}) ->
                    TS = [FT1 || FT <- FTypes, FT1 <- FT],
                    [debug_log(Scopes0, "\t~p/~p :: ~s~n",
                               [N, fun_arity(T), ?TYPE_MSG:pp_type(T)])
                     || T <- TS]
                end, dict:to_list(Scopes1#scopes.global)),
  Undefs = count_undefined(Scopes1),
  debug_log(Scopes0, "Number of undefined types: ~p~n", [Undefs]),
  Scopes2 = Scopes1#scopes{first_pass = false, errors = []},
  case Undefs of
    0 -> Scopes1;    %% All types could be inferred
    _ ->
      case FP of
        true ->
          type_check_loop(PassN + 1, Forms, Scopes2, Undefs);
        false ->
          case Undefs =:= PUndefs of
            %% Has number of Undefined changed from previous and this run?
            true ->
              Scopes1;
            false ->
              type_check_loop(PassN + 1, Forms, Scopes2, Undefs)
          end
      end
  end.


count_undefined(S) ->
  count_undefined_local_scopes(S)
    + count_undefined_global_scope(S).

count_undefined_local_scopes(#scopes{locals = LS}) ->
  lists:foldl(fun({_, L}, Cnt) ->
                  Cnt + count_undefined_local_scope(L)
              end, 0, dict:to_list(LS)).

count_undefined_local_scope(#local_scope{vars = L, type = Type}) ->
  length([1 || {_, #meta_var{type = T}} <- dict:to_list(L),
          T =:= undefined]) +
    length(type_internal:extract_type_terminals(undefined, Type)).

count_undefined_global_scope(#scopes{global = GS}) ->
  lists:foldl(
    fun({_, FTypes}, Cnt) ->
        Ts = [FT1 || FT <- FTypes, FT1 <- FT],
        Cnt +
          lists:sum(
            [length(type_internal:extract_type_terminals(undefined, T))
             || T <- Ts])
    end, 0, dict:to_list(GS)).


type_check1([], Scopes) ->
  Scopes;

type_check1([{function, L, N, A, Cls} | Forms], Scopes) ->
  {ClauseSig, Scopes1} = match_clauses_with_sig(N, A, Cls, Scopes),
  %% InferredTypeFSig =:= [{Line, fun_type, fun_type}]
  {_, InferredTypeFSig, Scopes2} =
    lists:foldl(fun({Cl, Sig}, {Ind, Ts, S0}) ->
                    L2 = element(2, Cl),
                    LsName = {N, L2, Ind},
                    S1 = start_ls(LsName, S0),
                    {T, S2} = type_check_clause(Sig, Cl, S1),
                    S3 = sync_ls(LsName, S2),
                    {Ind + 1, [{L2, T, Sig} | Ts], S3}
                end, {0, [], Scopes1}, ClauseSig),

  {FTypes, Scopes3} = infer_function_type({N, A}, InferredTypeFSig, Scopes2),
  Scopes4 = validate_fun_type({N, A, L}, FTypes, Scopes3),
  Scopes5 = update_global(Scopes4, N, A, FTypes),
  type_check1(Forms, Scopes5);

type_check1([_ | Fs], Scopes) ->
  type_check1(Fs, Scopes).


%% Returns [{clause, fun_type}]
match_clauses_with_sig(N, A, Cls, Scopes) ->
  Sig = find_fun_sig(N, A, Scopes),
  case Sig of
    undefined ->
      {lists:map(fun(E) -> {E, undefined} end, Cls), Scopes};
    FSs ->
      match_clauses_with_sig0(Cls, FSs, [], Scopes)
  end.

match_clauses_with_ftype(Cls, L, Scopes=#scopes{local = LS}) ->
  Scopes1 = Scopes#scopes{local = LS#local_scope{last_ftype = undefined}},
  case LS#local_scope.last_ftype of
    undefined ->
      {lists:map(fun(E) -> {E, undefined} end, Cls), Scopes1};
    FSs0 when is_list(FSs0) ->
      match_clauses_with_sig0(Cls, {fun_sig, L, noname, FSs0}, [], Scopes1);
    FSs1 ->
      match_clauses_with_sig0(Cls, {fun_sig, L, noname, [FSs1]}, [], Scopes1)
  end.

%% Returns [{clause, fun_Type}]
match_clauses_with_sig0([], _, Res, Scopes) ->
  {Res, Scopes};
match_clauses_with_sig0([{clause, _, CArg, _, _} = C | Cls],
                        {fun_sig, L, N, Sigs}, Res, Scopes) ->
  BestMatch = find_the_best_match(CArg, Sigs),
  match_clauses_with_sig0(Cls, {fun_sig, L, N, Sigs -- [BestMatch]},
                          Res ++ [{C, BestMatch}], Scopes).

%% Tries to find the best match by scoring on how many arguments
%% have the same type and returns the highest score and corresponding FType.
find_the_best_match(_, undefined) ->
  undefined;
find_the_best_match(TypedArgs, FTypes) ->
  Scores = lists:map(fun({fun_type, S0, _} = FType) ->
                         {calc_match_score(S0, TypedArgs), FType};
                        (undefined) ->
                         {0,
                          {fun_type,
                           [undefined || _ <- lists:seq(1, length(TypedArgs))],
                           undefined}}
                     end, FTypes),
  %% [{Integer, Sig}]
  SortedScores = lists:sort(fun({A, _}, {B, _}) ->
                                B =< A
                            end, Scores),
  %% Always picks the first best match. In case of multiple match with
  %% the same rank, it's developer's responsibility to declare fun sigs
  %% in correct order
  [{_Rank, BestMatch} | _] = SortedScores,
  BestMatch.

%% Calculate the scores given to two typed argument lists on the degree
%% of how much they match each other
calc_match_score(undefined, _) ->
  0;
calc_match_score(_, undefined) ->
  0;
calc_match_score(Args1, Args2) ->
  Args = lists:zip(Args1, Args2),
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


%% InferredTypeFSigList :: [{L, InferredType, FunSig}]
infer_function_type(NA, InferredTypeFSigList, Scopes) ->
  lists:foldl(fun({L, FType, FSig}, {Ts, S0}) ->
                  {N, A} = NA,
                  {FT, S1} = infer_function_clause({N, A, L}, FType, FSig, S0),
                  {Ts ++ [FT], S1}
              end, {[], Scopes}, InferredTypeFSigList).

infer_function_clause(_, FT, undefined, S) ->
  {FT, S};
infer_function_clause(NAL, Inferred, Declared, S=#scopes{}) ->
  case type_internal:sub_type_of(Declared, Inferred) of
    true -> {Declared, S};
    false ->
      {N, A, L} = NAL,
      Err =
        {L, ?TYPE_MSG,
         {declared_inferred_fun_type_do_not_match, N, A, Declared, Inferred}},
      {Inferred, S#scopes{errors = S#scopes.errors ++ [Err]}}
  end.

validate_fun_type(NAL, FTypes, Scopes) ->
  lists:foldl(fun(FT, S0) ->
                  validate_fun_type0(NAL, FT, S0)
              end, Scopes, FTypes).

validate_fun_type0(NAL, FType, S=#scopes{}) ->
  Undefs = type_internal:extract_type_terminals(undefined, FType),
  case length(Undefs) of
    0 -> S;
    _ ->
      {N, A, L} = NAL,
      Err = {L, ?TYPE_MSG, {can_not_infer_fun_type, N, A, FType}},
      S#scopes{errors = S#scopes.errors ++ [Err]}
  end.

type_check_clause(FSig, Cls, S=#scopes{first_pass = FP, local = LS}) ->
  case FP of
    true ->
      {Res, S1} = type_check_clause0(FSig, Cls, S),
      S2 = update_undefined(S1),
      {Res, S2};
    false ->
      UnDefs0 = LS#local_scope.last_nr_undefined,
      case UnDefs0 of
        0 ->
          {LS#local_scope.type, S};
        _ ->
          {Res, S1} = type_check_clause0(FSig, Cls, S),
          S2 = update_undefined(S1),
          {Res, S2}
      end
  end.

%% Update the number of undefined types in local scope
%% to save cost of calculation to one time so later passes
%% won't calculate this if it's already 0.
update_undefined(S0=#scopes{local = L}) ->
  InnerScopes =
    [find_ls(In, S0) || In <- gb_sets:to_list(L#local_scope.inner_scopes)],
  UnDefsInnerScopes =
    lists:sum([count_undefined_local_scope(IS) || IS <- InnerScopes]),
  UnDefs1 = count_undefined_local_scope(L),
  update_undefined_types_in_local(UnDefs1 + UnDefsInnerScopes, S0).

type_check_clause0(undefined, {clause, _L, Args, G, _E} = Cl, Scopes0) ->
  Scopes1 = type_check_clause_guard(G, Scopes0),
  Scopes2 = lists:foldl(fun(Var, S0) ->
                           insert_args(Var, undefined, S0)
                       end, Scopes1, [A || {Kind, _, _} = A
                                            <- Args, Kind =:= var]),
  type_check_clause1(Cl, Scopes2);

type_check_clause0({fun_type, Is, _},
                  {clause, _, Args, _, _} = Cl, Scopes0) ->
  VarTypes = lists:foldl(fun({LHS, RHS}, Acc) ->
                             type_internal:eliminate(LHS, RHS, Acc)
                         end, [], lists:zip(Args, Is)),
  %% TODO: validity of declared types for args
  Scopes1 = lists:foldl(fun({V, T}, S0) ->
                            insert_args(V, T, S0)
                        end, Scopes0, VarTypes),
  type_check_clause1(Cl, Scopes1).

type_check_clause_guard(Gs, Scopes0=#scopes{}) ->
  Scopes1 = Scopes0#scopes{fun_lookup = guard_fun_lookup_priorities()},
  GSeqs = [G1 || G0 <- Gs, G1 <- G0],
  Scopes2 = lists:foldl(fun(G, S0) ->
                       {TG, S1} = type_check_expr(G, S0),
                       assert_guard_type(G, TG, S1)
                   end, Scopes1, GSeqs),
  Scopes2#scopes{fun_lookup = standard_fun_lookup_priorities()}.

assert_guard_type(G, T, Scopes=#scopes{}) ->
  case T of
    {terl_type, 'Boolean'} ->
      Scopes;
    WrongType ->
      Scopes#scopes{
        errors = Scopes#scopes.errors ++
          [{element(2, G), ?TYPE_MSG, {wrong_guard_type, G, WrongType}}]}
  end.

%% function clause
type_check_clause1({clause, _L, Args, _G, Exprs}, Scopes0) ->
  {TOut, Scopes1} =
    lists:foldl(fun(Expr, {_, S0}) ->
                    {T, S1} = type_check_expr(Expr, S0),
                    LineNum = integer_to_list(element(2, Expr)),
                    update_local(S1, LineNum, T)
                end, {nil, Scopes0}, Exprs),
  TIn = [element(1, type_of(Arg, Scopes1)) || Arg <- Args],
  ClauseType = {fun_type, TIn, TOut},
  Scopes2 =
    Scopes1#scopes{local =
                     (Scopes1#scopes.local)#local_scope{type = ClauseType}},
  {ClauseType, check_local_scope(Scopes2)}.


type_check_expr({match, _L, LHS, RHS}, Scopes0) ->
  {Inferred, Scopes1} = type_of(RHS, Scopes0),
  VarTypes = type_internal:eliminate(LHS, Inferred),
  Scopes2 = update_local(Scopes1, VarTypes),
  {Inferred, Scopes2};

%% When there is type declaration
type_check_expr({match, L, {var, _, Var} = V, Type, RHS}, Scopes0) ->
  Scopes1 = check_for_fun_type(Type, Scopes0),
  {Inferred, Scopes2} = type_of(RHS, Scopes1),
  Scopes3 = assert_type_equality(Var, L, Type, Inferred, Scopes2),
  {_, Scopes4} = update_local(Scopes3, V, Inferred),
  {Inferred, Scopes4};

type_check_expr(E, Scopes0) ->
  type_of(E, Scopes0).

check_for_fun_type(Type, Scopes=#scopes{local = L}) ->
  case Type of
    FTs when is_list(FTs) ->
      Scopes#scopes{local = L#local_scope{last_ftype = FTs}};
    {fun_type, _, _} ->
      Scopes#scopes{local = L#local_scope{last_ftype = Type}};
    {untyped_fun, _, _} ->
      Scopes#scopes{local = L#local_scope{last_ftype = Type}};
    _ ->
      Scopes#scopes{local = L#local_scope{last_ftype = undefined}}
  end.

%% Insert argument types into local scope
insert_args({var, _, '_'}, _, S) ->
  S;
insert_args({var, L, Var}, Type, S=#scopes{}) ->
  case {non_recursive_lookup(Var, S), Type} of
    {undefined, undefined} -> insert_args0(Var, L, Type, S);
    {undefined, _}         -> insert_args0(Var, L, Type, S);
    {_,         undefined} -> S;
    {_,         _}         -> insert_args0(Var, L, Type, S)
  end.

insert_args0(Var, L, Type, S) ->
  LS = (S#scopes.local),
  debug_log(S, "\t~p :: ~s~n", [Var, ?TYPE_MSG:pp_type(Type)]),
  MetaVar = #meta_var{type = Type, line = L},
  S#scopes{local =
             LS#local_scope{vars =
                              dict:store(Var, MetaVar, LS#local_scope.vars)}}.

%% Generate type error for undefined types in local scope
check_local_scope(S=#scopes{local = #local_scope{vars = Vars}}) ->
  Undefs = [{L, ?TYPE_MSG, {can_not_infer_type, V}}
            || {V, #meta_var{type = T, line = L}}
                 <- dict:to_list(Vars), T =:= undefined],
  S#scopes{errors = S#scopes.errors ++ Undefs}.


type_of({nil, _}, S) ->
  {{list_type, nothing}, S};

type_of({integer, _, _}, S) ->
  {type_internal:tag_built_in('Integer'), S};

type_of({float, _, _}, S) ->
  {type_internal:tag_built_in('Float'), S};

type_of({atom, _, true}, S) ->
  {type_internal:tag_built_in('Boolean'), S};

type_of({atom, _, false}, S) ->
  {type_internal:tag_built_in('Boolean'), S};

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
  assert_operator_validity(Res, Op, TL1, TR1, L, Scopes4);

type_of({op, L, Op, RHS}, Scopes0) ->
  {TR, Scopes1} = type_of(RHS, Scopes0),
  Res = dispatch(Op, TR),
  assert_operator_validity(Res, Op, TR, L, Scopes1);

type_of({var, _L, Var}, #scopes{local = LS} = S) ->
  {recursive_lookup(Var, S, LS), S};

type_of({cons, L, H, T}, Scopes0) ->
  {TH, Scopes1} = type_of(H, Scopes0),
  {TT0, Scopes2} = type_of(T, Scopes1),
  TT1 = unwrap_list(T, TT0, L),
  {assert_list_validity(TH, TT1), Scopes2};

type_of({tuple, L, Es}, Scopes0) ->
  {TEs, Scopes1} = lists:foldl(fun(E, {Ts, Scopes}) ->
                                  {T, Scopes1} = type_of(E, Scopes),
                                  {Ts ++ [T], Scopes1}
                              end, {[], Scopes0}, Es),
  {assert_tuple_validity(TEs, L), Scopes1};


type_of({'fun', L, {clauses, [{clause, _, Args, _, _} | _] = Cls}}, Scopes0) ->
  {ClauseSig, Scopes1} = match_clauses_with_ftype(Cls, L, Scopes0),
  N = list_to_atom("anonymous_fun_at_" ++ integer_to_list(L)),
  A = length(Args),
  {_, InferredTypeFSig, Scopes2} =
    lists:foldl(fun({Cl, Sig}, {Ind, Ts, S0}) ->
                    L2 = element(2, Cl),
                    LsName = {N, L2, Ind},
                    S1 = nest_ls(LsName, S0),
                    {T, S2} = type_check_clause(Sig, Cl, S1),
                    S3 = sync_ls(LsName, S2),
                    {Ind + 1, [{L2, T, Sig} | Ts], S3}
                end, {0, [], Scopes1}, ClauseSig),

  {FTypes, Scopes3} = infer_function_type({N, A}, InferredTypeFSig, Scopes2),
  {FTypes, validate_fun_type({N, A, L}, FTypes, Scopes3)};

%% local calls
type_of({call, L, NN, Args}, Scopes0) ->
  Arity = length(Args),
  {N0, Scopes1} = type_of(NN, Scopes0),
  N = case N0 of
        undefined -> undefined;
        {_, N1} -> N1;
        _ -> element(3, NN)
      end,
  FTypes = find_fun_type(N, Arity, Scopes1),
  Scopes2 = assert_found_fun_type(FTypes, L, NN, Arity, Scopes1),
  {TypedArgs, Scopes3} = lists:foldl(fun(Arg, {Ts, S0}) ->
                                         {T, S1} = type_of(Arg, S0),
                                         {Ts ++ [T], S1}
                                     end, {[], Scopes2}, Args),

  Res = [find_exact_match(TypedArgs, FType) || FType <- FTypes],
  Matches = lists:filter(fun({R, _, _}) -> R =:= true end, Res),

  case length(Matches) of
    0 ->
      %% In case of no exact match we try to partially match the
      %% typed arguments against FTypes by using the eliminate method.
      {fun_type, Is, O} = find_the_best_match(TypedArgs, FTypes),
      {TypedArgs1, VartTypes} =
        lists:foldl(fun({A, T1, T2}, {Ts, VTs}) ->
                        case T1 of
                          undefined ->
                            VarTypes = type_internal:eliminate(A, T2),
                            case VarTypes of
                              [] -> {Ts ++ [T1], VTs};
                              _  -> {Ts ++ [T2], VTs ++ VarTypes}
                            end;
                          _ ->
                            {Ts ++ [T1], VTs}
                        end
                    end, {[], []}, lists:zip3(Args, TypedArgs, Is)),
      %% If the new TypedArgs is an exact match of callee then we can
      %% infer that we could eliminate types successfully, otherwise the
      %% old approach of generating type errors for arguments is followed.
      case find_exact_match(TypedArgs1, {fun_type, Is, O}) of
        {true, _, _} ->
          {O, update_local(Scopes3, VartTypes)};
        {false, _, _} ->
          Errs =
            lists:flatten(
              [generate_error_for_call(N, Arity, L, NonMatch)
               || {_, NonMatch, _} <- Res]),
          {undefined, Scopes3#scopes{errors = Scopes3#scopes.errors ++ Errs}}
      end;
    1 ->
      {fun_type, _, O} = element(3, hd(Matches)),
      {O, Scopes3};
    _ ->
      MatchingTypes = lists:map(fun(E) -> element(3, E) end, Matches),
      Err = {L, ?TYPE_MSG, {multiple_match_for_function_call, MatchingTypes}},
      {undefined, Scopes3#scopes{errors = Scopes3#scopes.errors ++ [Err]}}
  end;

type_of({'case', _, E, Cls}, Scopes0) ->
  {TE, Scopes1} = type_of(E, Scopes0),
  Scopes2 = case TE of
              undefined ->
                eliminate_based_on_clauses(E, Cls, Scopes1);
              _ ->
                Scopes1
            end,
  {_, TCls, Scopes3} =
    lists:foldl(fun({clause, L1, Es, Gs, Cs} = Cl, {Ind, Ts, S0}) ->
                    Name =
                      create_clause_name("case_clause", Ind, L1, Es, Gs, Cs),
                    S1 = nest_ls(Name, S0),
                    {TC, S2} = type_check_case_clause(TE, Cl, S1),
                    S3 = sync_ls(Name, S2),
                    {Ind + 1, Ts ++ [TC], S3}
                end, {0, [], Scopes2}, Cls),
  Tlcs = find_lcs(TCls),
  {Tlcs, Scopes3};

type_of({'if', _, Cls}, Scopes0) ->
  {_, TCls, Scopes1} =
    lists:foldl(fun({clause, L1, _, Gs, Exprs} = Cl, {Ind, Ts, S0}) ->
                    Name =
                      create_clause_name("if_clause", Ind, L1, [], Gs, Exprs),
                    S1 = nest_ls(Name, S0),
                    {TC, S2} = type_check_if_clause(Cl, S1),
                    S3 = sync_ls(Name, S2),
                    {Ind + 1, Ts ++ [TC], S3}
                end, {0, [], Scopes0}, Cls),
  Tlcs = find_lcs(TCls),
  {Tlcs, Scopes1};

type_of(T, Scopes) ->
  debug_log(Scopes, "type_of ~p not implemented~n", [T]),
  {undefined, Scopes}.

eliminate_based_on_clauses(E, Cls, Scopes0) ->
  VTsDict = lists:foldl(fun({clause, _, Es, _, _}, VTDict) ->
                        {TES, _} = type_of(hd(Es), Scopes0),
                        VT0 = type_internal:eliminate(E, TES),
                        lists:foldl(fun({K,V}, Dict) ->
                                        dict:append(K, V, Dict)
                                    end, VTDict, VT0)
                    end, dict:new(), Cls),
  lists:foldl(fun({K, Vs}, S0) ->
                  T = find_lcs(Vs),
                  update_local(S0, [{K, T}])
              end, Scopes0, dict:to_list(VTsDict)).

type_check_if_clause({clause, _, _, Gs, Cls}, S0) ->
  Scopes1 = type_check_clause_guard(Gs, S0),

  {TLastCl, Scopes2} =
    lists:foldl(fun(Expr, {_, SS0}) ->
                    {T, S1} = type_check_expr(Expr, SS0),
                    LineNum = integer_to_list(element(2, Expr)),
                    update_local(S1, LineNum, T)
                end, {nil, Scopes1}, Cls),

  Scopes3 =
    Scopes2#scopes{local =
                     (Scopes2#scopes.local)#local_scope{type = TLastCl}},

  {TLastCl, check_local_scope(Scopes3)}.

type_check_case_clause(TE, {clause, L, Es, Gs, Cls}, S0) ->
  Scopes1 = type_check_clause_guard(Gs, S0),

  VTs = type_internal:eliminate(hd(Es), TE),
  Scopes2 = assert_found_vt(L, Scopes1, VTs),
  Scopes3 = update_local(Scopes2, VTs),

  {TLastCl, Scopes4} =
    lists:foldl(fun(Expr, {_, SS0}) ->
                    {T, S1} = type_check_expr(Expr, SS0),
                    LineNum = integer_to_list(element(2, Expr)),
                    update_local(S1, LineNum, T)
                end, {nil, Scopes3}, Cls),

  Scopes5 =
    Scopes4#scopes{local =
                     (Scopes4#scopes.local)#local_scope{type = TLastCl}},

  {TLastCl, check_local_scope(Scopes5)}.

assert_found_vt(L, S=#scopes{}, VTs) ->
  Errs =
    lists:foldl(fun({{_, _, V}, T}, Errs0) ->
                    case non_recursive_lookup(V, S) of
                      undefined -> Errs0;
                      T -> Errs0;
                      T1 ->
                        [{L, ?TYPE_MSG, {conflicting_clause_var_type, V, T, T1}}
                         | Errs0]
                    end
                end, [], VTs),
  S#scopes{errors = Errs ++ S#scopes.errors}.

find_lcs(TCls) ->
  lists:foldl(fun(T1, T2) ->
                  type_internal:lcs(T1, T2)
              end, nothing, TCls).

create_clause_name(Prefix, Ind, L, Es, Gs, Cls) ->
    {Prefix, L, length(Es), length(Gs), length(Cls), Ind}.

assert_found_fun_type(undefined, L, NN, Ar, S=#scopes{errors = Errs}) ->
  S#scopes{errors = Errs ++ [{L, ?TYPE_MSG, {can_not_infer_type_fun, NN, Ar}}]};
assert_found_fun_type(_, _, _, _, S) ->
  S.

generate_error_for_call(N, Arity, L, NonMatchedArgList) ->
  {Res, _} =
    lists:foldl(
      fun(Arg, {Acc, Ind}) ->
          case Arg of
            true -> {Acc, Ind + 1};
            {false, Got, Expected} ->
              {Acc ++
                 [{L, ?TYPE_MSG,
                   {non_matching_type_fun_call, N, Arity, Ind, Got, Expected}}]
              , Ind + 1}
          end
      end, {[], 1}, NonMatchedArgList),
  Res.

%% Tries to find the exact match between TypedArgs and FType and returns
%% {Boolean, Result, FType} indicating if it was an exact match.
find_exact_match(_, undefined) ->
  {false, [], undefined};
find_exact_match(TypedArgs, {fun_type, Is, _} = FType) ->
  Res = lists:foldl(fun({T1, T2}, L) ->
                        case type_internal:sub_type_of(T1, T2) of
                          true -> L ++ [true];
                          false -> L ++ [{false, T1, T2}]
                        end
                    end, [], lists:zip(TypedArgs, Is)),

  {lists:all(fun(E) -> E =:= true
                 end, Res), Res, FType}.

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


%% Starting from the most inner local scope, tries to find the type
%% for a variable recusively to outer scopes until it finds a type.
%% Returns the first found type.
recursive_lookup(Var, S=#scopes{locals = LS},
                 #local_scope{vars = Vars, outer_scope = OS}) ->
  case dict:find(Var, Vars) of
    {ok, #meta_var{type = T}} -> T;
    error ->
      case OS of
        nil -> undefined;
        ParentLS ->
          case dict:find(ParentLS, LS) of
            {ok, LS1} -> recursive_lookup(Var, S, LS1);
            error -> undefined
          end
      end
  end.

recursive_ls_lookup(Var, LS, Locals) ->
  case recursive_ls_lookup0(Var, LS, Locals) of
    nil ->
      LS;
    Other ->
      Other
  end.

recursive_ls_lookup0(Var,
                 LS=#local_scope{vars = Vars, outer_scope = OS},
                   Locals) ->
  case dict:find(Var, Vars) of
    {ok, _} ->
      LS;
    error ->
      case OS of
        nil ->
          nil;
        ParentLS ->
          case dict:find(ParentLS, Locals) of
            {ok, LS1} -> recursive_ls_lookup0(Var, LS1, Locals);
            error ->
              io:format("WARNING! This should not happen at all, "
                        ++ "a pointer to non-existing local scope?!", []),
              nil
          end
      end
  end.

%% Only look in current local scope
non_recursive_lookup(Var, #scopes{local = LS}) ->
  Vars = LS#local_scope.vars,
  case dict:find(Var, Vars) of
    {ok, #meta_var{type = T}} -> T;
    error -> undefined
  end.

unwrap_list(_, {list_type, T}, _) ->
  T;
unwrap_list(_, {terl_type, 'Any'} = T, _) ->
  T;
unwrap_list(_, undefined, _) ->
  undefined;
unwrap_list(_, T, L) ->
  throw({error, L, {not_list_cons_position, T}}).


assert_list_validity(TH, TT) ->
  case {TH, TT} of
    {undefined, _}          -> undefined;
    {_, undefined}          -> undefined;
    {_, nothing}            -> {list_type, TH};
    {{terl_type, 'Any'}, _} -> {list_type, TT};
    {T1, T2} ->
      {list_type, type_internal:lcs(T1, T2)}
  end.

assert_tuple_validity(TES, _L) ->
  Undefined = lists:filter(fun(T) ->
                   T =:= undefined
               end, TES),
  case length(Undefined) of
    0 -> {tuple_type, TES};
    _ -> undefined
  end.


assert_type_equality(Var, L, Declared, Inferred, S=#scopes{errors = Errs}) ->
  case Inferred of
    undefined ->
      S;
    T ->
      case type_internal:type_equivalent(T, Declared) of
        true -> S;
        false ->
          S#scopes{
            errors = Errs ++
              [{L, ?TYPE_MSG,
                {declared_inferred_not_match, Var, Declared, Inferred}}]}
      end
  end.

%% Tries to infer the type for a variable based on operator application.
infer_from_op(Res, {var, _, _} = Var, undefined, Scopes) ->
  update_local(Scopes, Var, Res);
infer_from_op(Res, {var, _, _} = Var, {union_type, _}, Scopes) ->
  update_local(Scopes, Var, Res);
infer_from_op(_, _, Type, Scopes) ->
  {Type, Scopes}.

assert_operator_validity(Res, Op, TL, TR, L, Scopes=#scopes{errors = Errs}) ->
  InvalidOp = type_internal:invalid_operator(),
  case Res of
    InvalidOp ->
      {undefined,
       Scopes#scopes{errors =
                       Errs ++ [{L, ?TYPE_MSG,
                                 {invalid_operator, Op, TL, TR}}]}};
    R ->
      {R, Scopes}
  end.

assert_operator_validity(Res, Op, TR, L, Scopes=#scopes{errors = Errs}) ->
  InvalidOp = type_internal:invalid_operator(),
  case Res of
    InvalidOp ->
      {undefined,
       Scopes#scopes{errors =
                       Errs ++ [{L, ?TYPE_MSG,
                                 {invalid_operator, Op, TR}}]}};
    R ->
      {R, Scopes}
  end.

%% This is to avoid same variables errored multiple places
type_defined_in_local(Var, S=#scopes{local = LS}) ->
  case recursive_lookup(Var, S, LS) of
    undefined -> false;
    _         -> true
  end.

%% Only returns Scope
update_local(S0, VarTypes) ->
  lists:foldl(fun({K, T}, Acc) ->
                  {_, S1} = update_local(Acc, K, T),
                  S1
             end, S0, VarTypes).

%% Returns {Type, Scopes}
update_local(S=#scopes{local = CurrLS, locals = LsDict}, {var, L, V}, Type) ->
  FoundLS = recursive_ls_lookup(V, CurrLS, LsDict),
  MetaVar = #meta_var{type = Type, line = L},
  FoundLS1 = FoundLS#local_scope{vars =
                                   dict:store(V
                                             , MetaVar
                                             , FoundLS#local_scope.vars)},
  LsDict0 = dict:store(CurrLS#local_scope.name, CurrLS, LsDict),
  LsDict1 = dict:store(FoundLS#local_scope.name, FoundLS1, LsDict0),
  S1 = S#scopes{local = find_ls(CurrLS#local_scope.name, LsDict1),
               locals = LsDict1},

  case Type of
    undefined ->
      debug_log(S, "\t~p :: ?~n", [V]),
      {Type,
       S1#scopes{errors =
                  S#scopes.errors ++
                  [{L, ?TYPE_MSG, {can_not_infer_type, V}}
                   || type_defined_in_local(V, S)]}};
    _ ->
      debug_log(S, "\t~p :: ~s~n", [V, ?TYPE_MSG:pp_type(Type)]),
      {Type, S1}
  end;

%% Special case to store the type for each expression that is identified with
%% its line number
update_local(S=#scopes{}, String, Type) ->
  L = list_to_integer(String),
  MetaVar = #meta_var{type = Type, line = L},
  LS = (S#scopes.local),
  S1 = S#scopes{local =
                  LS#local_scope{vars =
                                   dict:store("Expression at line " ++ String
                                             , MetaVar
                                             , LS#local_scope.vars)}},
  case Type of
    undefined ->
      debug_log(S, "\tExpression @ ~p :: ?~n", [L]),
      {Type, S1};
    _ ->
      {Type, S1}
  end.


update_global(S=#scopes{global = GS}, N, Ar, FTypes) ->
  FsExceptN = case dict:find(N, GS) of
                 {ok, FLists} ->
                   [FList || FList <- FLists, fun_arity(hd(FList)) =/= Ar];
                 error -> []
               end,
  S#scopes{global = dict:store(N, FsExceptN ++ [FTypes], GS)}.

find_fun_type_in_global(N, Ar, #scopes{global = GS}) ->
  case dict:find(N, GS) of
    {ok, FList} ->
      lists:flatten(
        lists:filter(fun(Fs) -> fun_arity(hd(Fs)) =:= Ar end, FList));
    error -> []
  end.

find_fun_type_in_local(N, Ar, #scopes{local = LS}) ->
  case dict:find(N, LS#local_scope.vars) of
    {ok, #meta_var{type = FList}} ->
      case lists:all(fun(E) -> E =:= true end,
                     [fun_arity(Fs) =:= Ar || Fs <- FList]) of
        true  -> FList;
        false -> []
      end;
    error -> []
  end.

%% Returns {fun_sig, L, N, T} | undefined
find_fun_sig(N, A, #scopes{state = State}) ->
  case dict:find(N, State#state.fun_sigs) of
    {ok, Vs} ->
      hd([FS || {fun_sig, _, _, T} = FS <- Vs, fun_arity(T) =:= A]
         ++ [undefined]);
    error -> undefined
  end.

find_fun_type_in_guards(N, A, #scopes{state = State}) ->
  case dict:find(N, State#state.guard_types) of
    {ok, Vs} ->
      hd([FS || {fun_sig, _, _, T} = FS <- Vs, fun_arity(T) =:= A]
         ++ [undefined]);
    error -> undefined
  end.

standard_fun_lookup_priorities() ->
  [ fun find_local_fun_type_in_erlang_types/3
  , fun find_fun_type_in_local/3
  , fun find_fun_type_in_global/3
  , fun find_fun_sig/3].

guard_fun_lookup_priorities() ->
  [ fun find_fun_type_in_guards/3 ].

%% First checks to see if there exits a type definition in erlang types
%% then in global scope and finally in function signature
find_fun_type(N, Ar, Scopes=#scopes{fun_lookup = Priorities}) ->
  case find_fun_type0(Priorities, N, Ar, Scopes) of
    {fun_sig, _, _, T} -> T;
    undefined -> [undefined];
    Other -> Other
  end.

find_fun_type0([], _, _, _) ->
  undefined;
find_fun_type0([F | T], N, Ar, Scopes) ->
  case F(N, Ar, Scopes) of
    [] ->
      find_fun_type0(T, N, Ar, Scopes);
    Other ->
      Other
  end.

find_local_fun_type_in_erlang_types(N, Ar, Scopes) ->
  find_fun_type_in_erlang_types(nil, N, Ar, Scopes).

find_fun_type_in_erlang_types(M, N, Ar, #scopes{state = State}) ->
  Key = case M of
          nil -> atom_to_list(N);
          _   -> atom_to_list(M) ++ "_" ++ atom_to_list(N)
        end,
  ETypes = State#state.erlang_types,
  case dict:find(Key, ETypes) of
    {ok, FList} ->
      lists:flatten(
        lists:filter(fun(Fs) -> fun_arity(hd(Fs)) =:= Ar end, FList));
    error -> []
  end.

%% Either find an already existing local scope or
%% initializes a new one
start_ls(Name, S=#scopes{}) ->
  L = find_ls(Name, S),
  debug_log(S, "\t-------------- ~p -------------- ~n", [Name]),
  S#scopes{local = L}.

%% Same as `start_ls` except it nests the local scope
nest_ls(Name, S=#scopes{local = OuterScope, locals = LS}) ->
  debug_log(S, "\t-------------- ~p -------------- ~n", [Name]),
  OuterScopeName = OuterScope#local_scope.name,
  OuterScope1 =
    OuterScope#local_scope{
      inner_scopes = gb_sets:add(Name, OuterScope#local_scope.inner_scopes)},
  LS1 = dict:store(OuterScopeName, OuterScope1, LS),
  case dict:find(Name, LS) of
    {ok, L} ->
      S#scopes{local = L, locals = LS1};
    _ ->
      S#scopes{local = #local_scope{name = Name, outer_scope = OuterScopeName}
              , locals = LS1}
  end.

%% Save current local scope for later uses
sync_ls(Name, S=#scopes{local = L, locals = LS}) ->
  case L#local_scope.outer_scope of
    nil ->
      S#scopes{locals = dict:store(Name, L, LS)};
    OS ->
      S#scopes{locals = dict:store(Name, L, LS), local = dict:fetch(OS, LS)}
  end.

%% Cache the number of undefined type in current local scope
%% as long as it is zero.
update_undefined_types_in_local(UnDefs, S=#scopes{local = L}) ->
  L1 = L#local_scope{last_nr_undefined = UnDefs},
  S#scopes{local = L1}.

%% Find local scope by its given name
find_ls(Name, #scopes{locals = LS}) ->
  find_ls(Name, LS);

find_ls(Name, LocalsDict) ->
  case dict:find(Name, LocalsDict) of
    {ok, L} ->
      L;
    _ ->
      #local_scope{name = Name}
  end.

debug_log(#scopes{state = State}, Format, Args) ->
  debug_log0(State#state.compiler_opts, Format, Args);
debug_log(#state{compiler_opts = Opts}, Format, Args) ->
  debug_log0(Opts, Format, Args).

debug_log0(CompilerOpts, Format, Args) ->
  case lists:member(type_debug, CompilerOpts) of
    true ->
      io:format(Format, Args);
    false ->
      ok
  end.

dump_local_scopes(LsDict) ->
  io:format("~.55c~n", [$-]),
  LS = dict:to_list(LsDict),
  [dump_local_scope(L) ||
    L <- lists:sort(
           fun({E1, _}, {E2, _}) ->
               element(2, E1) < element(2, E2)
           end, LS)].

dump_local_scope({Name, #local_scope{ vars = Vars
                                    , type = Type
                                    , inner_scopes = IS
                                    , outer_scope = OS}}) ->
  N = io_lib:format("~p", [Name]),
  io:format("Name: ~s~n", [N]),
  io:format("Vars:~n", []),
  Vs = lists:sort(fun({_, #meta_var{line = L1}}, {_, #meta_var{line = L2}}) ->
                      L1 < L2
                  end, dict:to_list(Vars)),
  [ io:format("~30.s :: ~s~n", [V, type_err_msg:pp_type(T)])
   || {V, #meta_var{type = T}} <- Vs],
  io:format("Type:~n", []),
  io:format("~.5c ~s~n", [$ , type_err_msg:pp_type(Type)]),
  case gb_sets:size(IS) of
    0 -> ok;
    _ ->
      io:format("Inner Scopes:~n", []),
      [io:format("~.5c ~p~n", [$ , I]) || I <- gb_sets:to_list(IS)]
  end,
  case OS of
    nil -> ok;
    _ ->
      io:format("Outer Scopes:~n", []),
      io:format("~.5c ~p~n", [$ , OS])
  end,
  io:format("~.55c~n", [$-]).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:











