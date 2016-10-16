
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

%%------------------------------------------------------------------------------

-export([ module/2
        ]).

%%------------------------------------------------------------------------------

-include("type_checker_state.hrl").

%%------------------------------------------------------------------------------

module([{_FileName, _Forms, _Compile} | _] = FileNameForms, Opts) ->
  run_passes(FileNameForms, Opts).

run_passes([{_FileName, _Forms, _Compile} | _] = FileNameForms, Opts) ->
  try
    Opts1 = type_check_compiler_opts:options_of_interest(Opts),
    St0   = state_dl:compiler_opts(state_dl:new_state(), Opts1),
    St1   = state_dl:erlang_types(St0, bootstrap_erlang_types()),
    St2   = state_dl:guard_types(St1, erlang_guard_signature()),
    St3   = state_dl:export_whitelist(St2, export_whitelist()),
    St4   = run_one_time_passes(FileNameForms, St3, Opts),
    St5   = type_check_loop(1, St4, infinity),
    format_result(St5)
  catch
    _:L when is_list(L) -> {error, L, []};
    EE:Err ->
      io:format("Backtrace ~p~n", [erlang:get_stacktrace()]),
      io:format("Something bad happened, type system apologizes: ~p:~p~n"
               , [EE, Err])
  end.

run_one_time_passes(FileNameForms, State, _Opts) ->
  lists:foldl(fun({FileName, Forms, Compile}, St0) ->
                  MS  = state_dl:new_module_scope(FileName, Forms, Compile),
                  St1 = state_dl:current_module(St0, MS),
                  St2 = type_lint(St1),
                  state_dl:save_current_module_scope(St2)
              end, State, FileNameForms).

%% TODO: how many iterations until we give up?
type_check_loop(10, S, _) ->
  S;
type_check_loop(PassN, State=#state{}, PUndefs) ->
  FP = state_dl:first_pass(State),
  debug_log(State,
            ">>>>>>>>>>>>>>>>>>>> PASS ~p <<<<<<<<<<<<<<<<<<<<<~n", [PassN]),

  {State1, Undefs} =
    lists:foldl(fun({_M, MS}, {St0, Undefs0}) ->
                    St1 = state_dl:current_module(St0, MS),
                    St2 = type_check0(St1),
                    St3 = state_dl:save_current_module_scope(St2),
                    UndefinedTypes = count_undefined(St2),
                    NErrors = length(state_dl:errors(St2)),
                    {St3, Undefs0 + UndefinedTypes + NErrors}
                end, {State, 0},
                dict:to_list(state_dl:module_scopes(State))),

  debug_log(State, "Number of undefined types and errors: ~p~n", [Undefs]),

  %% Only for sake of debugging
  debug_log(State,
            "\t~~~~~~~~~~~~~~~~~~~~ Global scope ~~~~~~~~~~~~~~~~~~~~ ~n", []),
  lists:foreach(
    fun({M, MS}) ->
        lists:foreach(fun({N, FTypes}) ->
                          TS = [FT1 || FT <- FTypes, FT1 <- FT],
                          [debug_log(State, "\t~p:~p/~p :: ~s~n",
                                     [M, N, fun_arity(T), ?TYPE_MSG:pp_type(T)])
                           || T <- TS]
                      end, dict:to_list(state_dl:global(MS)))
    end,
    dict:to_list(state_dl:module_scopes(State1))),

  State2 = state_dl:first_pass(State1, false),
  StateWithNoErrors =
    lists:foldl(fun({M, MS}, S0) ->
                    MS1 = state_dl:errors(MS, []),
                    state_dl:module_scope(S0, M, MS1)
                end, State2,
                dict:to_list(state_dl:module_scopes(State2))),

  case Undefs of
    0 -> State2;    %% All types could be inferred
    _ ->
      case FP of
        true ->
          type_check_loop(PassN + 1, StateWithNoErrors, Undefs);
        false ->
          case Undefs =:= PUndefs of
            %% Has number of Undefined not changed from previous and this run?
            true ->
              State2;
            false ->
              type_check_loop(PassN + 1, StateWithNoErrors, Undefs)
          end
      end
  end.

format_result(State=#state{}) ->
  ModuleScopes = state_dl:module_scopes(State),
  {NErrors, Result} =
    lists:foldl(fun({_M, MS}, {Errs0, Acc}) ->
                    FN    = state_dl:filename(MS),
                    Errs  = state_dl:errors(MS),
                    Ws    = state_dl:warnings(MS),
                    C     = state_dl:compile_record(MS),
                    Errs1 = [{FN, Errs} || length(Errs) =/= 0],
                    Ws1   = [{FN, Ws} || length(Ws) =/= 0],
                    {Errs0 + length(Errs), [{Ws1, Errs1, C} | Acc]}
                end, {0, []}, dict:to_list(ModuleScopes)),

  case NErrors of
    0 -> {ok, Result};
    _ -> {error, Result}
  end.

count_undefined(S) ->
  count_undefined_local_scopes(S)
    + count_undefined_global_scope(S).

count_undefined_local_scopes(State=#state{}) ->
  LS = state_dl:locals(State),
  lists:foldl(fun({_, L}, Cnt) ->
                  Cnt + count_undefined_local_scope(L)
              end, 0, dict:to_list(LS)).

count_undefined_local_scope(LS=#local_scope{}) ->
  L = state_dl:vars(LS),
  Type = state_dl:type(LS),
  length([1 || {_, #meta_var{type = T}} <- dict:to_list(L),
          T =:= undefined]) +
    length(type_internal:extract_type_terminals(undefined, Type)).

count_undefined_global_scope(State=#state{}) ->
  GS = state_dl:global(State),
  lists:foldl(
    fun({_, FTypes}, Cnt) ->
        Ts = [FT1 || FT <- FTypes, FT1 <- FT],
        Cnt +
          lists:sum(
            [length(type_internal:extract_type_terminals(undefined, T))
             || T <- Ts])
    end, 0, dict:to_list(GS)).


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
          Key = atom_to_list(M) ++ ":" ++ atom_to_list(N),
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

export_whitelist() ->
  PrivDir = code:lib_dir(type_checker, priv),
  {ok, [Term | _]} = file:consult(filename:join(PrivDir, "export_whitelist")),
  gb_sets:from_list(Term).

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

type_lint(St0) ->
  Forms = state_dl:forms(St0),
  St1   = collect_types(Forms, St0),
  debug_log(St1, "~p~n", [Forms]),
  St2   = check_consistency_type_cons(St1),
  check_consistency_fun_sigs(St2),
  St3   = no_remote_fun_sig_declared(St2),
  Ws1   = check_unsued_user_defined_types(St3),
  St4   = check_undefined_types(St3),
  St5   = match_fun_sig_with_declared_fun(St4),
  St6   = build_record_type_with_declared_record(St5),
  %% TODO: checks for generic types
  %% - RHS usage
  %% - Type expansion: type instances, type aliases
  Errs0 = state_dl:errors(St3),
  Errs  = gb_sets:to_list(gb_sets:from_list(Errs0)),
  St7   = state_dl:errors(St6, Errs),
  state_dl:warnings(St7, Ws1).

%% Collect forms of interest into state record
collect_types([{attribute, _, module, Module} | Forms], St0) ->
  St1 = state_dl:module_name(St0, Module),
  collect_types(Forms, St1);
collect_types([{attribute, _, compile, Opts} | Forms], St0) ->
  St1 = insert_compiler_options(St0, Opts),
  collect_types(Forms, St1);
collect_types([{attribute, _, export, Exports} | Forms], St0) ->
  St1 = state_dl:exports(St0, Exports),
  collect_types(Forms, St1);
collect_types([{attribute, _, record, RecDef} | Forms], St0) ->
  collect_types(Forms, add_record_def(RecDef, St0));
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
collect_types([{record_type_def, _, _, _} = Form | Forms], St0) ->
  collect_types(Forms, add_record_type_def(Form, St0));
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

insert_compiler_options(St=#state{}, Options) ->
  Opts  = state_dl:compiler_opts(St),
  St1   = state_dl:compiler_options(St, Options),
  Opts1 = type_check_compiler_opts:options_of_interest(Options),
  Opts2 = gb_sets:to_list(gb_sets:from_list(Opts ++ Opts1)),
  state_dl:compiler_opts(St1, Opts2).

%% Extract user defined types given at line L and associate each one with
%% line L in the returned state.type_used_loc.
extract_user_defined_types_with_locs(T, L, St=#state{}) ->
  Ts   = type_internal:extract_user_defined_types(T),
  Locs = generate_locs_for_types(Ts, L),
  TU   = gb_sets:union(gb_sets:from_list(Ts), state_dl:type_used(St)),
  St1  = state_dl:type_used(St, TU),
  state_dl:type_used_loc(St1, merge(Locs, state_dl:type_used_loc(St1))).

generate_locs_for_types(Ts, L) ->
  lists:foldl(fun (Tp, D) ->
                  dict:append(Tp, L, D)
              end, dict:new(), Ts).

merge(D1, D2) ->
  dict:merge(fun(_, V1, V2) ->
                 V1 ++ V2
             end, D1, D2).

%% Check if a refered type is undefined in this module
check_undefined_types(State=#state{}) ->
  TA  = state_dl:type_aliases(State),
  TU  = state_dl:type_used(State),
  TUL = state_dl:type_used_loc(State),

  Erros =
    lists:foldl(fun(T, Acc) ->
                    case dict:find(T, TA) of
                      {ok, _} -> Acc;
                      _ ->
                        {ok, L} = dict:find(T, TUL),
                        Locs = gb_sets:to_list(gb_sets:from_list(L)),
                        Acc ++
                          [{L1, ?TYPE_MSG, {undefined_type, T}} || L1 <- Locs]
                    end
                end, [], gb_sets:to_list(TU)),
  state_dl:update_errors(State, Erros).


%% Check if user defined types are used
check_unsued_user_defined_types(State=#state{}) ->
  FileName = state_dl:filename(State),
  TA       = state_dl:type_aliases(State),
  TU       = state_dl:type_used(State),

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
match_fun_sig_with_declared_fun(State=#state{}) ->
  DF = state_dl:declared_fun(State),
  FS = state_dl:fun_sigs(State),

  Errors =
    lists:foldl(fun({_, V}, Acc) ->
                    case V of
                      [_|_] = L -> Acc ++ [match_fun_sig0(L1, DF) || L1 <- L];
                      A         -> [match_fun_sig0(A, DF) | Acc]
                    end
                end, [], dict:to_list(FS)),
  %% filter out oks and keep error tuples
  state_dl:update_errors(State, [E || {_, _, _} = E <- Errors]).

match_fun_sig0({fun_sig, L2, N, _} = F, DF) ->
  SigAr = fun_arity(F),
  case dict:find(N, DF) of
    error ->
      {L2, ?TYPE_MSG, {no_fun_decl_found_for_sig, N, L2}};
    {ok, [_|_] = L} ->
      case length([E || {function, _, _, Ar, _} = E <- L, SigAr =:= Ar])
      of
        0 ->
          {L2, ?TYPE_MSG, {no_matching_fun_decl_for_fun_sig, N, SigAr, L2}};
        1 ->
          ok;
        _ ->
          {L2, ?TYPE_MSG, {multi_match_fun_decl_for_fun_sig, N, L2}}
      end;
    {ok, {function, _, _, Ar, _}} ->
      case SigAr =:= Ar of
        false ->
          {L2, ?TYPE_MSG, {no_matching_fun_decl_for_fun_sig, N, SigAr, L2}};
        true ->
          ok
      end
    end.

build_record_type_with_declared_record(St=#state{}) ->
  RT = state_dl:record_types(St),
  Rs = state_dl:records(St),

  {Errs, NRT} =
    lists:foldl(fun({N, {record_type_def, L, _, T}}, {Errs, Mapped}) ->
                  case dict:find(N, Rs) of
                    {ok, RecDef} ->
                      M = merge_rec_def_with_type(N, L, RecDef, T),
                      {Errs, [M | Mapped]};
                    error ->
                      Err = [{L, ?TYPE_MSG, {no_record_definition, N}} | Errs],
                      {Err, Mapped}
                  end
              end, {[], []}, dict:to_list(RT)),
  St1 = state_dl:record_types(St, dict:from_list(NRT)),
  state_dl:update_errors(St1, Errs).

merge_rec_def_with_type(N, L, RecDef, Ts) ->
  case length(RecDef) =:= length(Ts) of
    false ->
      throw({error, L, {none_matching_record_type, N}});
    true ->
      M = lists:map(fun({{record_field, _, {atom, _, FN}}, T}) ->
                        {FN, T}
                    end, lists:zip(RecDef, Ts)),
      {N, M}
  end.

%% Adds a new function signature to state record or adds an error if
%% this function signature is already defined.
add_fun_sigs({fun_sig, L2, N, _} = F, St=#state{}) ->
  FS = state_dl:fun_sigs(St),

  case dict:find(N, FS) of
    {ok, Vs} ->
      Ar = fun_arity(F),
      case [V || V <- Vs, fun_arity(V) =:= Ar] of
        [{_, L1, _, _} | _] ->
          state_dl:update_errors(St, L2, {duplicate_fun_sig_decl, N, L1, L2});
        _ ->
          state_dl:fun_sigs(St, dict:append(N, F, FS))
      end;
    error ->
      state_dl:fun_sigs(St, dict:append(N, F, FS))
  end.

%% Adds a new remote function signature to state record or adds
%% an error if this function signature is already defined.
add_remote_fun_sigs({fun_remote_sig, L2, M, N, _} = F, St=#state{}) ->
  FS = state_dl:remote_fun_sigs(St),
  Key = atom_to_list(M) ++ ":" ++ atom_to_list(N),
  case dict:find(Key, FS) of
    {ok, Vs} ->
      Ar = fun_arity(F),
      case [V || V <- Vs, fun_arity(V) =:= Ar] of
        [{_, L1, _, _} | _] ->
          state_dl:update_errors(St
                                , L2
                                , {duplicate_fun_sig_decl, M, N, L1, L2});
        _ ->
          state_dl:remote_fun_sigs(St, dict:append(Key, F, FS))
      end;
    error ->
      state_dl:remote_fun_sigs(St, dict:append(Key, F, FS))
  end.

%% Adds a new type alias to state record or adds an error if this
%% type alias is already defined.
add_type_alias({type_alias, L2, N, _} = F, St=#state{}) ->
  TA = state_dl:type_aliases(St),
  case dict:find(N, TA) of
    {ok, {_, L1, _, _}} ->
      state_dl:update_errors(St, L2, {duplicate_type_alias_decl, N, L1, L2});
    error ->
      state_dl:type_aliases(St, dict:store(N, F, TA))
  end.

%% Adds a new type constructor to state record or adds an error if this
%% type cons is already defined by another type cons or type alias.
add_type_cons({type_cons, L2, N, _P, _T} = F, St=#state{}) ->
  TC = state_dl:type_cons(St),
  TA = state_dl:type_aliases(St),
  case dict:find(N, TC) of
    {ok, {_, L1, _, _, _}} ->
      state_dl:update_errors(St, L2, {duplicate_type_cons_decl, N, L1, L2});
    error ->
      case dict:find(N, TA) of
        {ok, {_, L3, _, _, _}} ->
          state_dl:update_errors(St, L2, {duplicate_type_cons_decl, N, L2, L3});
        error ->
          state_dl:type_cons(St, dict:store(N, F, TC))
      end
  end.

add_record_type_def({record_type_def, L, N, _T} = R, St=#state{}) ->
  RT = state_dl:record_types(St),
  case dict:find(N, RT) of
    {ok, {_, L1, _, _}} ->
      state_dl:update_errors(St, L, {duplicate_record_type, N, L1, L});
    error ->
      state_dl:record_types(St, dict:store(N, R, RT))
  end.

add_record_def({N, Def}, St=#state{}) ->
  Rs = state_dl:records(St),
  state_dl:records(St, dict:store(N, Def, Rs)).

add_declared_fun({function, _, N, _, _} = F, St=#state{}) ->
  DF = state_dl:declared_fun(St),
  state_dl:declared_fun(St, dict:append(N, F, DF)).

check_consistency_type_cons(State=#state{}) ->
  TC = state_dl:type_cons(State),
  lists:foldl(fun({_, E}, St) ->
                  St1 = check_consistency_type_cons_lhs_rhs(E, St),
                  no_terl_type_used_lhs(E, St1)
              end, State, dict:to_list(TC)).

no_remote_fun_sig_declared(St=#state{}) ->
  FSigs  = state_dl:remote_fun_sigs(St),
  Errors = [{L, ?TYPE_MSG, {no_remote_fun_sig_allowed, M, N}} ||
             {_, RFS} <- dict:to_list(FSigs),
             {fun_remote_sig, L, M, N, _} <- RFS],
  state_dl:update_errors(St, Errors).

%% All fun sigs clauses must have same arity
check_consistency_fun_sigs(State=#state{}) ->
  FS = state_dl:fun_sigs(State),
  lists:foldl(fun({_, Sigs}, St) ->
                  check_consistency_fun_sigs0(St, Sigs)
              end, State, dict:to_list(FS)).

check_consistency_fun_sigs0(State=#state{}, Sigs) ->
  lists:foldl(fun(Sig, St) ->
                  check_consistency_fun_sig(St, Sig)
              end, State, Sigs).

check_consistency_fun_sig(State=#state{}, {fun_sig, L, N, Cls}) ->
  SetArity = gb_sets:from_list([fun_arity(Cl) || Cl <- Cls]),
  case gb_sets:size(SetArity) of
    1 ->
      State;
    _ ->
      state_dl:update_errors(State, L, {fun_sig_clause_arity_not_match, N})
  end.

%% Check if that all defined generic type parameters in the left hand side of
%% type constructor is used in the right hand side and vice versa
check_consistency_type_cons_lhs_rhs({type_cons, L, _N, Is, O}
                                   , State=#state{}) ->
  GTI0 = lists:flatten([type_internal:extract_generic_types(I) || I <- Is]),
  GTO0 = type_internal:extract_generic_types(O),
  GTI1 = gb_sets:to_list(gb_sets:from_list(GTI0)),
  GTO1 = gb_sets:to_list(gb_sets:from_list(GTO0)),
  NotUsedRhs = GTI1 -- GTO1,
  NotUsedLhs = GTO1 -- GTI1,
  case NotUsedLhs =:= NotUsedRhs of
    true -> State;
    false ->
      Errs = [{L, ?TYPE_MSG, {tc_generic_type_not_used_lhs, NotUsedLhs}}
       || length(NotUsedLhs) =/= 0] ++
        [{L, ?TYPE_MSG, {tc_generic_type_not_used_rhs, NotUsedRhs}}
         || length(NotUsedRhs) =/= 0],
     state_dl:update_errors(State, Errs)
  end.

no_terl_type_used_lhs({type_cons, L, _N, Is, _O}, State=#state{}) ->
  TI0 = lists:flatten([type_internal:type_terminals(I) || I <- Is]),
  TI1 = lists:filter(fun({Tag, _}) -> Tag =/= terl_generic_type end, TI0),
  case length(TI1) of
    0 ->
      State;
    _ ->
      TI2 = lists:map(fun(E) -> element(2, E) end, TI1),
      state_dl:update_errors(State, L, {tc_only_generic_type_lhs, TI2})
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

type_check0(S0=#state{}) ->
  Opts   = state_dl:compiler_opts(S0),
  Forms  = state_dl:forms(S0),
  S1     = type_check1(Forms, S0),
  Errs0  = state_dl:errors(S1),
  LS     = state_dl:locals(S1),
  Name   = state_dl:module_name(S1),

  case type_check_compiler_opts:dump_local_scopes(Opts) of
    true -> dump_local_scopes(Name, LS);
    false -> ok
  end,

  debug_log(S1, ">>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<~n", []),
  debug_log(S1, "Number of errors: ~p~n", [length(Errs0)]),

  Errs = gb_sets:to_list(gb_sets:from_list(Errs0)),

  state_dl:errors(S1, Errs).

type_check1([], State) ->
  State;

type_check1([{function, L, N, A, Cls} | Forms], State) ->
  {ClauseSig, State1} = match_clauses_with_sig(N, A, Cls, State),
  %% InferredTypeFSig =:= [{Line, fun_type, fun_type}]
  {_, InferredTypeFSig, State2} =
    lists:foldl(fun({Cl, Sig}, {Ind, Ts, S0}) ->
                    L2 = element(2, Cl),
                    LsName = {N, L2, Ind},
                    debug_log(S0, "\t-------------- ~p -------------- ~n"
                             , [LsName]),
                    S1 = state_dl:start_ls(LsName, S0),
                    {T, S2} = type_check_clause(Sig, Cl, S1),
                    S3 = state_dl:sync_ls(LsName, S2),
                    {Ind + 1, [{L2, T, Sig} | Ts], S3}
                end, {0, [], State1}, ClauseSig),

  {FTypes, State3} = infer_function_type({N, A}, InferredTypeFSig, State2),
  State4 = validate_fun_type({N, A, L}, FTypes, State3),
  State5 = update_global(State4, N, A, FTypes),
  type_check1(Forms, State5);

type_check1([_ | Fs], State) ->
  type_check1(Fs, State).


%% Returns [{clause, fun_type}]
match_clauses_with_sig(N, A, Cls, State) ->
  Sig = find_fun_sig([N, A, State]),
  case Sig of
    undefined ->
      {lists:map(fun(E) -> {E, undefined} end, Cls), State};
    FSs ->
      {FSs1, State1} = assert_and_fix_size_inequality(Cls, FSs, State),
      match_clauses_with_sig0(Cls, FSs1, [], State1)
  end.

assert_and_fix_size_inequality(Cls, {fun_sig, L, Name, Sigs} = FSs, State) ->
  case {length(Cls), length(Sigs)} of
    {N, N} -> {FSs, State};
    {N, 1} ->
      {{fun_sig, L, Name, [hd(Sigs) || _ <- lists:seq(1, N)]}, State};
    {N, M} ->
      Msg = {fun_head_fun_sig_size_mismatch, N, M, Name, fun_arity(Sigs)},
      State1 = state_dl:update_errors(State, L, Msg),
      {FSs, State1}
  end.

match_clauses_with_ftype(Cls, L, State=#state{}) ->
  LS = state_dl:local(State),
  State1 = state_dl:local(State, state_dl:last_ftype(LS, undefined)),
  case state_dl:last_ftype(LS) of
    undefined ->
      {lists:map(fun(E) -> {E, undefined} end, Cls), State1};
    FSs0 when is_list(FSs0) ->
      match_clauses_with_sig0(Cls, {fun_sig, L, noname, FSs0}, [], State1);
    FSs1 ->
      match_clauses_with_sig0(Cls, {fun_sig, L, noname, [FSs1]}, [], State1)
  end.

%% Returns [{clause, fun_Type}]
match_clauses_with_sig0([], _, Res, State) ->
  {Res, State};
match_clauses_with_sig0([{clause, _, CArg, _, _} = C | Cls],
                        {fun_sig, L, N, Sigs}, Res, State) ->
  BestMatch = find_the_best_match(CArg, Sigs),
  match_clauses_with_sig0(Cls, {fun_sig, L, N, Sigs -- [BestMatch]},
                          Res ++ [{C, BestMatch}], State).

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
  case SortedScores of
    [] -> undefined;
    _ ->
      {_Rank, BestMatch} = hd(SortedScores),
      BestMatch
  end.

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
infer_function_type(NA, InferredTypeFSigList, State) ->
  lists:foldl(fun({L, FType, FSig}, {Ts, S0}) ->
                  {N, A} = NA,
                  {FT, S1} = infer_function_clause({N, A, L}, FType, FSig, S0),
                  {Ts ++ [FT], S1}
              end, {[], State}, InferredTypeFSigList).

infer_function_clause(_, FT, undefined, S) ->
  {FT, S};
infer_function_clause(NAL, Inferred, Declared, S=#state{}) ->
  case type_internal:sub_type_of(Declared, Inferred) of
    true -> {Declared, S};
    false ->
      {N, A, L} = NAL,
      Err =
        {L, ?TYPE_MSG,
         {declared_inferred_fun_type_do_not_match, N, A, Declared, Inferred}},
      {Inferred, state_dl:update_errors(S, [Err])}
  end.

validate_fun_type(NAL, FTypes, State) ->
  lists:foldl(fun(FT, S0) ->
                  validate_fun_type0(NAL, FT, S0)
              end, State, FTypes).

validate_fun_type0(NAL, FType, S=#state{}) ->
  Undefs = type_internal:extract_type_terminals(undefined, FType),
  case length(Undefs) of
    0 -> S;
    _ ->
      {N, A, L} = NAL,
      Err = {L, ?TYPE_MSG, {can_not_infer_fun_type, N, A, FType}},
      state_dl:update_errors(S, [Err])
  end.

type_check_clause(FSig, Cls, S=#state{}) ->
  FP = state_dl:first_pass(S),
  LS = state_dl:local(S),
  case FP of
    true ->
      {Res, S1} = type_check_clause0(FSig, Cls, S),
      S2 = update_undefined(S1),
      {Res, S2};
    false ->
      UnDefs0 = state_dl:last_nr_undefined(LS),
      case UnDefs0 of
        0 ->
          {state_dl:type(LS), S};
        _ ->
          {Res, S1} = type_check_clause0(FSig, Cls, S),
          S2        = update_undefined(S1),
          {Res, S2}
      end
  end.

%% Update the number of undefined types in local scope
%% to save cost of calculation to one time so later passes
%% won't calculate this if it's already 0.
update_undefined(S0=#state{}) ->
  L = state_dl:local(S0),
  InnerScopes =
    [state_dl:find_ls(In, S0) ||
      In <- gb_sets:to_list(state_dl:inner_scopes(L))],
  UnDefsInnerScopes =
    lists:sum([count_undefined_local_scope(IS) || IS <- InnerScopes]),
  UnDefs1 = count_undefined_local_scope(L),
  Errors = length(state_dl:errors(S0)),
  state_dl:update_undefined_types_in_local(UnDefs1 + UnDefsInnerScopes + Errors,
                                           S0).

type_check_clause0(undefined, {clause, _L, Args, G, _E} = Cl, State0) ->
  State1  = type_check_clause_guard(G, State0),
  State2  = arg_type_elimination(Args, State1),
  VarArgs = lists:foldl(fun(Arg, Acc) ->
                            type_internal:var_terminals(Arg) ++ Acc
                        end, [], Args),
  State3  = lists:foldl(fun(Var, S0) ->
                           insert_args(Var, undefined, S0)
                       end, State2, VarArgs),
  type_check_clause1(Cl, State3);

type_check_clause0({fun_type, Is, _},
                  {clause, _, Args, G, _} = Cl, State0) ->
  State1   = type_check_clause_guard(G, State0),
  State2   = arg_type_elimination(Args, State1),
  VarTypes = lists:foldl(fun({LHS, RHS}, Acc) ->
                             type_internal:eliminate(LHS, RHS, Acc, State2)
                         end, [], lists:zip(Args, Is)),
  %% TODO: validity of declared types for args
  State3   = lists:foldl(fun({V, T}, S0) ->
                            insert_args(V, T, S0)
                        end, State2, VarTypes),
  type_check_clause1(Cl, State3).

arg_type_elimination(Args, State0) ->
  VarTypes = lists:foldl(fun(Arg, Acc) ->
                             record_type_elimination(Arg, State0) ++ Acc
                         end, [], Args),
  update_local(State0, VarTypes).

record_type_elimination({match, _, {record, _, N, _} = R, T}, State) ->
  type_internal:eliminate(R, T, State) ++
    type_internal:eliminate(T, {record_type, N}, State);
record_type_elimination({match, _, T, {record, _, N, _} = R}, State) ->
  type_internal:eliminate(R, T, State) ++
    type_internal:eliminate(T, {record_type, N}, State);
record_type_elimination({record, _, N, _} = R, State) ->
  type_internal:eliminate(R, {record_type, N}, State);
record_type_elimination(_, _) ->
  [].

type_check_clause_guard(Gs, State0=#state{}) ->
  State1 = state_dl:fun_lookup(State0, guard_fun_lookup_priorities()),
  GSeqs  = [G1 || G0 <- Gs, G1 <- G0],
  State2 = lists:foldl(fun(G, S0) ->
                       {TG, S1} = type_check_expr(G, S0),
                       assert_guard_type(G, TG, S1)
                   end, State1, GSeqs),
  state_dl:fun_lookup(State2, nil).

%% function clause
type_check_clause1({clause, _L, Args, _G, Exprs}, State0) ->
  {TOut, State1} =
    lists:foldl(fun(Expr, {_, S0}) ->
                    {T, S1} = type_check_expr(Expr, S0),
                    LineNum = element(2, Expr),
                    update_local(S1, "Expression", LineNum, T)
                end, {nil, State0}, Exprs),
  TIn = [element(1, type_of(Arg, State1)) || Arg <- Args],
  ClauseType = {fun_type, TIn, TOut},
  State2 = state_dl:update_type_in_local_scope(ClauseType, State1),
  {ClauseType, check_local_scope(State2)}.


type_check_expr({match, L, {record, _, N, _} = R, RHS}, State0) ->
  {TRHS, State1} = type_of(RHS, State0),
  VarTypes       = type_internal:eliminate(R, {record_type, N}, State1),
  State2         = update_local(State1, VarTypes),
  State3         = assert_type_equality(RHS, L, {record_type, N}, TRHS, State2),
  {TRHS, State3};

type_check_expr({match, _L, LHS, {'fun', _, _} = RHS}, State0) ->
  {TLHS, State1}     = type_of(LHS, State0),
  State2             = check_for_fun_type(TLHS, State1),
  {Inferred, State3} = type_of(RHS, State2),
  State4             = reset_last_ftype(State3),
  VarTypes           = type_internal:eliminate(LHS, Inferred, State4),
  State5             = update_local(State4, VarTypes),
  State6             = type_of_lhs(LHS, State5),
  {Inferred, State6};

type_check_expr({match, _L, LHS, RHS}, State0) ->
  {Inferred, State1} = type_of(RHS, State0),
  VarTypes           = type_internal:eliminate(LHS, Inferred, State1),
  State2             = update_local(State1, VarTypes),
  State3             = type_of_lhs(LHS, State2),
  {Inferred, State3};

%% When there is type declaration
type_check_expr({match, L, {var, _, Var} = V, Type, RHS}, State0) ->
  State1             = check_for_fun_type(Type, State0),
  {Inferred, State2} = type_of(RHS, State1),
  State3             = reset_last_ftype(State2),
  State4             = assert_type_equality(Var, L, Type, Inferred, State3),
  {_, State5}        = update_local(State4, V, Inferred),
  {Inferred, State5};

type_check_expr(E, State0) ->
  type_of(E, State0).

reset_last_ftype(State=#state{}) ->
  LS = state_dl:local(State),
  state_dl:local(State, state_dl:last_ftype(LS, undefined)).

type_of_lhs({bin, _, _} = Bin, State0) ->
  {_, State1} = type_of(Bin, State0),
  State1;
type_of_lhs(_, State) ->
  State.

check_for_fun_type(Type, State=#state{}) ->
  L = state_dl:local(State),
  case Type of
    FTs when is_list(FTs) ->
      state_dl:local(State, state_dl:last_ftype(L, FTs));
    {fun_type, _, _} ->
      state_dl:local(State, state_dl:last_ftype(L, Type));
    {untyped_fun, _, _} ->
      state_dl:local(State, state_dl:last_ftype(L, Type));
    _ ->
      state_dl:local(State, state_dl:last_ftype(L, undefined))
  end.

%% Insert argument types into local scope
insert_args({var, _, '_'}, _, S) ->
  S;
insert_args({var, L, Var}, Type, S=#state{}) ->
  case {non_recursive_lookup(Var, S), Type} of
    {undefined, undefined} -> insert_args0(Var, L, Type, S);
    {undefined, _}         -> insert_args0(Var, L, Type, S);
    {_,         undefined} -> S;
    {_,         _}         -> insert_args0(Var, L, Type, S)
  end.

insert_args0(Var, L, Type, S=#state{}) ->
  debug_log(S, "\t~p :: ~s~n", [Var, ?TYPE_MSG:pp_type(Type)]),
  LS      = state_dl:local(S),
  MetaVar = state_dl:meta_var(Type, L),
  NLS     = dict:store(Var, MetaVar, state_dl:vars(LS)),
  state_dl:local(S, state_dl:vars(LS, NLS)).

%% Generate type error for undefined types in local scope
check_local_scope(S=#state{}) ->
  LS     = state_dl:local(S),
  Vars   = state_dl:vars(LS),
  Undefs = [{L, ?TYPE_MSG, {can_not_infer_type, V}}
            || {V, #meta_var{type = T, line = L}}
                 <- dict:to_list(Vars), T =:= undefined],
  state_dl:update_errors(S, Undefs).


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

type_of({var, _L, '_'}, State0) ->
  {type_internal:tag_built_in('Any'), State0};

type_of({op, L, Op, LHS, RHS}, State0) ->
  {TL0, State1} = type_of(LHS, State0),
  {TR0, State2} = type_of(RHS, State1),
  Res           = dispatch(TL0, Op, TR0),
  {TL1, State3} = infer_from_op(Res, LHS, TL0, State2),
  {TR1, State4} = infer_from_op(Res, RHS, TR0, State3),
  assert_operator_validity(Res, Op, TL1, TR1, L, State4);

type_of({op, L, Op, RHS}, State0) ->
  {TR, State1} = type_of(RHS, State0),
  Res          = dispatch(Op, TR),
  assert_operator_validity(Res, Op, TR, L, State1);

type_of({type_anno, _, V, T}, State0) ->
  update_local(State0, V, T);

type_of({var, _L, Var}, S=#state{}) ->
  LS = state_dl:local(S),
  {recursive_lookup(Var, S, LS), S};

type_of({cons, L, H, T}, State0) ->
  {TH, State1}  = type_of(H, State0),
  {TT0, State2} = type_of(T, State1),
  TT1           = unwrap_list(TT0, L),
  {assert_list_validity(TH, TT1), State2};

type_of({tuple, L, Es}, State0) ->
  {TEs, State1} = lists:foldl(fun(E, {Ts, State}) ->
                                  {T, State1} = type_of(E, State),
                                  {Ts ++ [T], State1}
                              end, {[], State0}, Es),
  {assert_tuple_validity(TEs, L), State1};

type_of({'fun', L, {function, N, A}}, State0) ->
  case find_fun_type_in_global([N, A, State0]) of
    [] ->
      Msg = {function_pointer_not_found, N, A},
      {undefined, state_dl:update_errors(State0, L, Msg)};
    FType ->
      {FType, State0}
  end;

type_of({'fun', L, {clauses, [{clause, _, Args, _, _} | _] = Cls}}, State0) ->
  {ClauseSig, State1} = match_clauses_with_ftype(Cls, L, State0),
  N = list_to_atom("anonymous_fun_at_" ++ integer_to_list(L)),
  A = length(Args),
  {_, InferredTypeFSig, State2} =
    lists:foldl(fun({Cl, Sig}, {Ind, Ts, S0}) ->
                    L2 = element(2, Cl),
                    LsName = {N, L2, Ind},
                    debug_log(S0, "\t-------------- ~p -------------- ~n"
                             , [LsName]),
                    S1      = state_dl:nest_ls(LsName, S0),
                    {T, S2} = type_check_clause(Sig, Cl, S1),
                    S3      = state_dl:sync_ls(LsName, S2),
                    {Ind + 1, [{L2, T, Sig} | Ts], S3}
                end, {0, [], State1}, ClauseSig),

  {FTypes, State3} = infer_function_type({N, A}, InferredTypeFSig, State2),
  {FTypes, validate_fun_type({N, A, L}, FTypes, State3)};

%% remote calls
type_of({call, L, {remote, _, M0, F0}, Args}, State0) ->
  Arity = length(Args),
  {M00, State01} = type_of(M0, State0),
  {F00, State1} = type_of(F0, State01),

  M = case M00 of
        undefined -> undefined;
        {_, M1} -> M1;
        _ -> element(3, M0)
      end,
  F = case F00 of
        undefined -> undefined;
        {_, F1} -> F1;
        _ -> element(3, F0)
      end,
  FunLookup =
    case state_dl:fun_lookup(State1) of
      nil     -> standard_remote_fun_lookup_priorities();
      FLookup -> FLookup
    end,
  {TypedArgs, State2} = lists:foldl(fun(Arg, {Ts, S0}) ->
                                         {T, S1} = type_of(Arg, S0),
                                         {Ts ++ [T], S1}
                                     end, {[], State1}, Args),
  FTypes0 = find_fun_type([M, F, Arity, State2], FunLookup),
  {State21, FTypes}  = materialise_if_generic(FTypes0, TypedArgs, L, State2),
  State3 = assert_found_remote_fun_type(FTypes, L, M, F, Arity, State21),

  Res = [find_exact_match(TypedArgs, FType) || FType <- FTypes],
  Matches = lists:filter(fun({R, _, _}) -> R =:= true end, Res),

  case length(Matches) of
    0 ->
      {undefined,
       state_dl:update_errors(State3, L,
                              {can_not_infer_type_fun, M, F, Arity})};
    1 ->
      {fun_type, _, O} = element(3, hd(Matches)),
      {O, State3};
    _ ->
      MatchingTypes = lists:map(fun(E) -> element(3, E) end, Matches),
      Err = {L, ?TYPE_MSG, {multiple_match_for_function_call, MatchingTypes}},
      {undefined, state_dl:update_errors(State3, [Err])}
  end;

%% local calls
type_of({call, L, NN, Args}, State0) ->
  Arity = length(Args),
  {N0, State1} = type_of(NN, State0),
  N = case N0 of
        undefined -> undefined;
        {_, N1} -> N1;
        _ -> element(3, NN)
      end,
  FunLookup =
    case state_dl:fun_lookup(State1) of
      nil -> standard_fun_lookup_priorities();
      FLookup -> FLookup
    end,
  {TypedArgs, State2} =
    lists:foldl(fun(Arg, {Ts, S0}) ->
                    {T, S1} = type_of(Arg, S0),
                    {Ts ++ [T], S1}
                end, {[], State1}, Args),
  FTypes0 = find_fun_type([N, Arity, State2], FunLookup),
  {State21, FTypes}  = materialise_if_generic(FTypes0, TypedArgs, L, State2),
  State3  = assert_found_fun_type(FTypes, L, NN, Arity, State21),

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
                            VarTypes = type_internal:eliminate(A, T2, State3),
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
          {O, update_local(State3, VartTypes)};
        {false, _, _} ->
          Errs =
            lists:flatten(
              [generate_error_for_call(N, Arity, L, NonMatch)
               || {_, NonMatch, _} <- Res]),
          {undefined, state_dl:update_errors(State3, Errs)}
      end;
    1 ->
      {fun_type, _, O} = element(3, hd(Matches)),
      {O, State3};
    _ ->
      MatchingTypes = lists:map(fun(E) -> element(3, E) end, Matches),
      Err = {L, ?TYPE_MSG, {multiple_match_for_function_call, MatchingTypes}},
      {undefined, state_dl:update_errors(State3, [Err])}
  end;

type_of({'case', _, E, Cls}, State0) ->
  {TE, State1} = type_of(E, State0),
  State2 = case TE of
              undefined ->
                eliminate_based_on_clauses(E, Cls, State1);
              _ ->
                State1
            end,
  {_, TCls, State3} =
    lists:foldl(fun({clause, L1, Es, Gs, Cs} = Cl, {Ind, Ts, S0}) ->
                    Name =
                      create_clause_name("case_clause", Ind, L1, Es, Gs, Cs),
                    S1       = state_dl:nest_ls(Name, S0),
                    {TC, S2} = type_check_case_clause(TE, Cl, S1),
                    S3       = state_dl:sync_ls(Name, S2),
                    {Ind + 1, Ts ++ [TC], S3}
                end, {0, [], State2}, Cls),
  Tlub = find_lub(TCls),
  {Tlub, State3};

type_of({'if', _, Cls}, State0) ->
  {_, TCls, State1} =
    lists:foldl(fun({clause, L1, _, Gs, Exprs} = Cl, {Ind, Ts, S0}) ->
                    Name =
                      create_clause_name("if_clause", Ind, L1, [], Gs, Exprs),
                    S1       = state_dl:nest_ls(Name, S0),
                    {TC, S2} = type_check_if_clause(Cl, S1),
                    S3 = state_dl:sync_ls(Name, S2),
                    {Ind + 1, Ts ++ [TC], S3}
                end, {0, [], State0}, Cls),
  Tlub = find_lub(TCls),
  {Tlub, State1};

type_of({generate, L, P, E}, State0) ->
  {TE, State1} = type_of(E, State0),
  VTs          = type_internal:eliminate(P, unwrap_list(TE, L), State1),
  {TE, update_local(State1, VTs)};

type_of({b_generate, L, P, E}, State0) ->
  {TE, State1} = type_of(E, State0),
  {TP, State2} = type_of(P, State1),
  State3       = assert_binary_type(E, TE, L, State2),
  State4       = assert_binary_type(P, TP, L, State3),
  {TE, State4};

type_of({lc, L, E, Qs}, State0) ->
  Name   = {"lc", L, length(Qs)},
  State1 = state_dl:nest_ls(Name, State0),
  State2 = lists:foldl(fun(Q, S0) ->
                            {_, S1} = type_of(Q, S0),
                            S1
                        end, State1, Qs),
  {TE, State3} = type_of(E, State2),
  LCType = {list_type, TE},
  State4 = state_dl:update_type_in_local_scope(LCType, State3),
  State5 = state_dl:sync_ls(Name, State4),
  {LCType, State5};

type_of({bc, L, E, Qs}, State0) ->
  Name = {"bc", L, length(Qs)},
  State1 = state_dl:nest_ls(Name, State0),
  State2 = lists:foldl(fun(Q, S0) ->
                            {_, S1} = type_of(Q, S0),
                            S1
                        end, State1, Qs),
  {TE, State3} = type_of(E, State2),
  State4       = state_dl:update_type_in_local_scope(TE, State3),
  State5       = state_dl:sync_ls(Name, State4),
  {TE, State5};

type_of({block, L, Exprs}, State0) ->
  Name   = {"block", L, length(Exprs)},
  State1 = state_dl:nest_ls(Name, State0),
  {TLastExpr, State2} =
    lists:foldl(fun(Expr, S0) ->
                    type_of(Expr, S0)
                end, State1, Exprs),

  State3 = state_dl:update_type_in_local_scope(TLastExpr, State2),
  State4 = state_dl:sync_ls(Name, State3),
  {TLastExpr, State4};

type_of({bin, _,  BinSegments}, State0) ->
  State1 = lists:foldl(fun(Seg, S0) ->
                            {_, S1} = type_of(Seg, S0),
                            S1
                        end, State0, BinSegments),
  {{terl_type, 'Binary'}, State1};

type_of({bin_element, L, {var, _, Var} = V, _, TSLs}, State0) ->
  {TSL, State1} =
    case terl_binary:type_specifier_list(TSLs) of
      [T] -> {T, State0};
      Ts   ->
        {undefined,
         state_dl:update_errors( State0
                               , L
                               , {bin_segment_conflicting_types, Var, Ts})}
    end,
  update_local(State1, V, TSL);

type_of({bin_element, _, _, _, _}, State0) ->
 {{terl_type, 'Integer'}, State0};

type_of({record, L, N, Fs}, St=#state{}) ->
  TN     = type_internal:find_record_type(N, St),
  State1 = assert_found_record_type(N, TN, L, St),
  State2 =
    lists:foldl(fun(F, S0) ->
                    {_, S1} = type_check_record_field(N, TN, F, S0),
                    S1
                end, State1, Fs),
  {{record_type, N}, State2};

type_of({record_field, L, V, N, {atom,_, F}}, State0=#state{}) ->
  St           = state_dl:state(State0),
  {TV, State1} = type_of(V, State0),
  {_, State2}  = update_local(State1, V, TV),
  State3       = assert_type_equality(V, L, {record_type, N}, TV, State2),
  TR           = type_internal:find_record_type(N, St),
  State4       = assert_found_record_type(N, TR, L, State3),
  TF           = type_internal:find_record_field_type(F, TR),
  {TF, State4};

type_of({match, _, _, _} = M, State0) ->
  type_check_expr(M, State0);

type_of(T, State) ->
  debug_log(State, "type_of ~p not implemented~n", [T]),
  {undefined, State}.

materialise_if_generic(FTypes, TypedArgs, L, State) ->
  lists:foldl(
    fun(FType, {St, Acc}) ->
        {FT, Mps, Errs} =
          type_internal:generic_materialisation(FType, TypedArgs),
        case length(Errs) of
          0 -> {State, Acc ++ [FT]};
          _ ->
            case dict:size(Mps) > 0 of
              true -> %% there were generic types involved
                St1 =
                  lists:foldl(fun(Err, S0) ->
                                  state_dl:update_errors(S0, L, Err)
                              end, St, Errs),
                {St1, Acc ++ [FT]};
              false ->
                {State, Acc ++ [FType]}
            end
        end
    end, {State, []}, FTypes).


type_check_record_field(N, TN
                       , {record_field, L, {atom, _, F}, V}, State0) ->
  {TV, State1} = type_of(V, State0),
  State2       = update_field_type(N, F, L, TV, State1),
  TF           = type_internal:find_record_field_type(F, TN),
  State3       = assert_record_field_type_equality(N, L, F, TF, TV, State2),
  {TF, State3}.

update_field_type(N, F, L, T, State) ->
  Name        = io_lib:format("#~p.~p", [N, F]),
  {_, State1} = update_local(State, Name, L, T),
  State1.

eliminate_based_on_clauses(E, Cls, State0) ->
  VTsDict = lists:foldl(fun({clause, _, Es, _, _}, VTDict) ->
                        {TES, _} = type_of(hd(Es), State0),
                        VT0 = type_internal:eliminate(E, TES, State0),
                        lists:foldl(fun({K,V}, Dict) ->
                                        dict:append(K, V, Dict)
                                    end, VTDict, VT0)
                    end, dict:new(), Cls),
  lists:foldl(fun({K, Vs}, S0) ->
                  T = find_lub(Vs),
                  update_local(S0, [{K, T}])
              end, State0, dict:to_list(VTsDict)).

type_check_if_clause({clause, _, _, Gs, Cls}, S0) ->
  State1 = type_check_clause_guard(Gs, S0),

  {TLastCl, State2} =
    lists:foldl(fun(Expr, {_, SS0}) ->
                    {T, S1} = type_check_expr(Expr, SS0),
                    LineNum = element(2, Expr),
                    update_local(S1, "Expression", LineNum, T)
                end, {nil, State1}, Cls),

  State3 = state_dl:update_type_in_local_scope(TLastCl, State2),
  {TLastCl, check_local_scope(State3)}.

type_check_case_clause(TE, {clause, L, Es, Gs, Cls}, S0) ->
  State1 = type_check_clause_guard(Gs, S0),

  VTs    = type_internal:eliminate(hd(Es), TE, State1),
  State2 = assert_found_vt(L, State1, VTs),
  State3 = update_local(State2, VTs),

  {TLastCl, State4} =
    lists:foldl(fun(Expr, {_, SS0}) ->
                    {T, S1} = type_check_expr(Expr, SS0),
                    LineNum = element(2, Expr),
                    update_local(S1, "Expression", LineNum, T)
                end, {nil, State3}, Cls),

  State5 = state_dl:update_type_in_local_scope(TLastCl, State4),
  {TLastCl, check_local_scope(State5)}.

find_lub(TCls) ->
  lists:foldl(fun(T1, T2) ->
                  type_internal:lub(T1, T2)
              end, nothing, TCls).

create_clause_name(Prefix, Ind, L, Es, Gs, Cls) ->
    {Prefix, L, length(Es), length(Gs), length(Cls), Ind}.

generate_error_for_call(N, Arity, L, NonMatchedArgList) ->
  {Res, _} =
    lists:foldl(
      fun(Arg, {Acc, Ind}) ->
          case Arg of
            {true, _, _} -> {Acc, Ind + 1};
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
  Res =
    lists:foldl(fun({T1, T2}, L) ->
                    L ++ [{type_internal:sub_type_of(T1, T2), T1, T2}]
                end, [], lists:zip(TypedArgs, Is)),

  {lists:all(fun({E, _, _}) -> E =:= true end, Res), Res, FType}.

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
recursive_lookup(Var, S=#state{}, LocalS=#local_scope{}) ->
  LS = state_dl:locals(S),
  Vars = state_dl:vars(LocalS),
  OS = state_dl:outer_scope(LocalS),
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

recursive_ls_lookup0(Var, LS=#local_scope{}, Locals) ->
  Vars = state_dl:vars(LS),
  OS = state_dl:outer_scope(LS),
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
non_recursive_lookup(Var, State=#state{}) ->
  LS   = state_dl:local(State),
  Vars = state_dl:vars(LS),
  case dict:find(Var, Vars) of
    {ok, #meta_var{type = T}} -> T;
    error -> undefined
  end.

unwrap_list({list_type, T}, _) ->
  T;
unwrap_list({terl_type, 'Any'} = T, _) ->
  T;
unwrap_list(undefined, _) ->
  undefined;
unwrap_list(T, L) ->
  throw({error, L, {not_list_cons_position, T}}).


%% Tries to infer the type for a variable based on operator application.
infer_from_op(Res, {var, _, _} = Var, undefined, State) ->
  update_local(State, Var, Res);
infer_from_op(Res, {var, _, _} = Var, {union_type, _}, State) ->
  update_local(State, Var, Res);
infer_from_op(_, _, Type, State) ->
  {Type, State}.

assert_found_vt(L, S=#state{}, VTs) ->
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
  state_dl:update_errors(S, Errs).

assert_found_remote_fun_type(undefined, L, M, N, Ar, S=#state{}) ->
  state_dl:update_errors(S,
                         [{L, ?TYPE_MSG, {can_not_infer_type_fun, M, N, Ar}}]);
assert_found_remote_fun_type(_FTypes, L, M, N, Ar, S=#state{}) ->
  case gb_sets:is_member(M, state_dl:export_whitelist(S)) of
    false -> assert_export_function(M, N, Ar, L, S);
    true  -> S
  end.

assert_export_function(M, N, Ar, L, S=#state{}) ->
  case state_dl:module_scope(M, S) of
    nil ->
      S;
    MS  ->
      Exports = state_dl:exports(MS),
      Exported = gb_sets:is_member({N, Ar}, Exports) orelse
        gb_sets:is_member(export_all, state_dl:compiler_options(MS)),
      case Exported of
        false ->
          state_dl:update_errors(S, L, {function_not_exported, M, N, Ar});
        true ->
          S
      end
  end.

assert_found_fun_type(undefined, L, NN, Ar, S=#state{}) ->
  state_dl:update_errors(S, [{L, ?TYPE_MSG, {can_not_infer_type_fun, NN, Ar}}]);
assert_found_fun_type(_, _, _, _, S) ->
  S.

assert_binary_type(Expr, T, L, State0) ->
  case T of
    {terl_type, 'Binary'} ->
      State0;
    TWrong ->
      state_dl:update_errors(State0, L, {expected_binary_type, Expr, TWrong})
  end.

assert_found_record_type(N, T, L, State=#state{}) ->
  case T of
    undefined ->
      state_dl:update_errors(State, L, {record_type_not_found, N});
    _ ->
      State
  end.

assert_record_field_type_equality(N, L, F, TF, TV, State0=#state{}) ->
  case type_internal:type_equivalent(TF, TV) of
    true ->
      State0;
    false ->
      state_dl:update_errors(State0,
                             L,
                             {wrong_record_field_type, N, F, TF, TV})
  end.

assert_guard_type(G, T, State=#state{}) ->
  case T of
    {terl_type, 'Boolean'} ->
      State;
    WrongType ->
      state_dl:update_errors(State,
                             element(2, G),
                             {wrong_guard_type, G, WrongType})
  end.

assert_list_validity(TH, TT) ->
  case {TH, TT} of
    {undefined, _}          -> undefined;
    {_, undefined}          -> undefined;
    {_, nothing}            -> {list_type, TH};
    {{terl_type, 'Any'}, _} -> {list_type, TT};
    {T1, T2} ->
      {list_type, type_internal:lub(T1, T2)}
  end.

assert_tuple_validity(TES, _L) ->
  Undefined = lists:filter(fun(T) ->
                   T =:= undefined
               end, TES),
  case length(Undefined) of
    0 -> {tuple_type, TES};
    _ -> undefined
  end.


assert_type_equality(Var, L, Declared, Inferred, S=#state{}) ->
  case Inferred of
    undefined ->
      S;
    T ->
      case type_internal:type_equivalent(T, Declared) of
        true -> S;
        false ->
          state_dl:update_errors(S, L,
                                 {declared_inferred_not_match
                                 , Var, Declared, Inferred})
      end
  end.

assert_operator_validity(Res, Op, TL, TR, L, State=#state{}) ->
  InvalidOp = type_internal:invalid_operator(),
  case Res of
    InvalidOp ->
      {undefined,
       state_dl:update_errors(State, L, {invalid_operator, Op, TL, TR})};
    R ->
      {R, State}
  end.

assert_operator_validity(Res, Op, TR, L, State=#state{}) ->
  InvalidOp = type_internal:invalid_operator(),
  case Res of
    InvalidOp ->
      {undefined,
       state_dl:update_errors(State, L, {invalid_operator, Op, TR})};
    R ->
      {R, State}
  end.

%% This is to avoid same variables errored multiple places
type_defined_in_local(Var, S=#state{}) ->
  LS = state_dl:local(S),
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

assert_and_update_type(V, L, NewType, Dict) ->
  OldType = case dict:find(V, Dict) of
              {ok, #meta_var{type = T}} -> T;
              error                     -> undefined
            end,
  MetaVar = state_dl:meta_var(NewType, L),
  case {OldType, NewType} of
    {undefined, _} ->
      {[], dict:store(V, MetaVar, Dict)};
    {T1, T1} ->
      {[], Dict};
    {T1, T2} ->
      case type_internal:type_equivalent(T1, T2) of
        true ->
          {[], Dict};
        false ->
          {[{L, ?TYPE_MSG, {inferred_conflicting_types, V, T1, T2}}], Dict}
      end
  end.

%% Returns {Type, State}
update_local(S=#state{}, {var, L, V}, Type) ->
  CurrLS   = state_dl:local(S),
  LsDict   = state_dl:locals(S),
  FoundLS  = recursive_ls_lookup(V, CurrLS, LsDict),
  {Errors, NewVars} =
    assert_and_update_type(V, L, Type, state_dl:vars(FoundLS)),
  FoundLS1 = state_dl:vars(FoundLS, NewVars),
  LsDict0  = dict:store(state_dl:local_scope_name(CurrLS), CurrLS, LsDict),
  LsDict1  = dict:store(state_dl:local_scope_name(FoundLS), FoundLS1, LsDict0),
  S1 =
    state_dl:local(S,
                   state_dl:find_ls(state_dl:local_scope_name(CurrLS),
                                    LsDict1)),
  S2 = state_dl:locals(S1, LsDict1),

  case Type of
    undefined ->
      debug_log(S, "\t~p :: ?~n", [V]),
      {Type,
       state_dl:update_errors
         (S2, Errors ++
            [{L,
              ?TYPE_MSG,
              {can_not_infer_type, V}} || type_defined_in_local(V, S)])};
    _ ->
      debug_log(S, "\t~p :: ~s~n", [V, ?TYPE_MSG:pp_type(Type)]),
      {Type, state_dl:update_errors(S2, Errors)}
  end.

%% Special case to store the type for each expression/record field
%% that is identified with its line number
update_local(S=#state{}, Name, L, Type) ->
  Key     = Name ++ " at line " ++ integer_to_list(L),
  MetaVar = state_dl:meta_var(Type, L),
  LS      = state_dl:local(S),
  NVar    = dict:store(Key, MetaVar, state_dl:vars(LS)),
  S1      = state_dl:local(S, state_dl:vars(LS, NVar)),

  case Type of
    undefined ->
      debug_log(S, "\t~s @ ~p :: ?~n", [Name, L]),
      {Type, S1};
    _ ->
      {Type, S1}
  end.


update_global(S=#state{}, N, Ar, FTypes) ->
  GS        = state_dl:global(S),
  FsExceptN = case dict:find(N, GS) of
                 {ok, FLists} ->
                   [FList || FList <- FLists, fun_arity(hd(FList)) =/= Ar];
                 error -> []
               end,
  state_dl:global(S, dict:store(N, FsExceptN ++ [FTypes], GS)).

find_fun_type_in_global([N, Ar, State=#state{}]) ->
  GS = state_dl:global(State),
  find_fun_type_in_global0(N, Ar, GS).

find_fun_type_in_global0(N, Ar, GS) ->
  case dict:find(N, GS) of
    {ok, FList} ->
      lists:flatten(
        lists:filter(fun(Fs) -> fun_arity(hd(Fs)) =:= Ar end, FList));
    error -> []
  end.

find_fun_type_in_remote([M, N, Ar, State=#state{}]) ->
  case state_dl:module_scope(M, State) of
    nil -> [];
    MS  ->
      GS = state_dl:global(MS),
      find_fun_type_in_global0(N, Ar, GS)
  end.

find_fun_type_in_local([N, Ar, State=#state{}]) ->
  LS = state_dl:local(State),
  case dict:find(N, state_dl:vars(LS)) of
    {ok, #meta_var{type = FList}} ->
      case lists:all(fun(E) -> E =:= true end,
                     [fun_arity(Fs) =:= Ar || Fs <- FList]) of
        true  -> FList;
        false -> []
      end;
    error -> []
  end.

%% Returns {fun_sig, L, N, T} | undefined
find_fun_sig([N, A, State=#state{}]) ->
  case dict:find(N, state_dl:fun_sigs(State)) of
    {ok, Vs} ->
      hd([FS || {fun_sig, _, _, T} = FS <- Vs, fun_arity(T) =:= A]
         ++ [undefined]);
    error -> undefined
  end.

find_fun_type_in_guards([N, A, State=#state{}]) ->
  case dict:find(N, state_dl:guard_types(State)) of
    {ok, Vs} ->
      hd([FS || {fun_sig, _, _, T} = FS <- Vs, fun_arity(T) =:= A]
         ++ [undefined]);
    error -> undefined
  end.

standard_remote_fun_lookup_priorities() ->
  [ fun find_fun_type_in_erlang_types/1
  , fun find_fun_type_in_remote/1
  ].

standard_fun_lookup_priorities() ->
  [ fun find_local_fun_type_in_erlang_types/1
  , fun find_fun_type_in_local/1
  , fun find_fun_type_in_global/1
  , fun find_fun_sig/1
  ].

guard_fun_lookup_priorities() ->
  [ fun find_fun_type_in_guards/1 ].

%% First checks to see if there exits a type definition in erlang types
%% then in global scope and finally in function signature
find_fun_type(Args, Priorities) ->
  case find_fun_type0(Priorities, Args) of
    {fun_sig, _, _, T} -> T;
    undefined -> [undefined];
    Other -> Other
  end.

find_fun_type0([], _) ->
  undefined;
find_fun_type0([F | T], Args) ->
  case F(Args) of
    [] ->
      find_fun_type0(T, Args);
    Other ->
      Other
  end.

find_local_fun_type_in_erlang_types([N, Ar, State]) ->
  find_fun_type_in_erlang_types([nil, N, Ar, State]).

find_fun_type_in_erlang_types([M, N, Ar, State=#state{}]) ->
  Key = case M of
          nil -> atom_to_list(N);
          _   -> atom_to_list(M) ++ ":" ++ atom_to_list(N)
        end,
  ETypes = state_dl:erlang_types(State),
  case dict:find(Key, ETypes) of
    {ok, FList} ->
      lists:flatten(
        lists:filter(fun(Fs) -> fun_arity(hd(Fs)) =:= Ar end, FList));
    error -> []
  end.

debug_log(S=#state{}, Format, Args) ->
  Opts = state_dl:compiler_opts(S),
  debug_log0(Opts, Format, Args).

debug_log0(CompilerOpts, Format, Args) ->
  case lists:member(type_debug, CompilerOpts) of
    true ->
      io:format(Format, Args);
    false ->
      ok
  end.

dump_local_scopes(Name, LsDict) ->
  io:format("~.10c ~p ~.70c~n", [$=, Name, $=]),
  io:format("~.55c~n", [$-]),
  LS = dict:to_list(LsDict),
  [dump_local_scope(L) ||
    L <- lists:sort(
           fun({E1, _}, {E2, _}) ->
               element(2, E1) < element(2, E2)
           end, LS)].

dump_local_scope({Name, LS=#local_scope{}}) ->
  Vars = state_dl:vars(LS),
  Type = state_dl:type(LS),
  IS   = state_dl:inner_scopes(LS),
  OS   = state_dl:outer_scope(LS),
  N    = io_lib:format("~p", [Name]),
  io:format("Name: ~s~n", [N]),
  io:format("Vars:~n", []),
  Vs = lists:sort(fun({_, #meta_var{line = L1}}, {_, #meta_var{line = L2}}) ->
                      L1 < L2
                  end, dict:to_list(Vars)),
  [ io:format("~40.s :: ~s~n", [V, type_err_msg:pp_type(T)])
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
