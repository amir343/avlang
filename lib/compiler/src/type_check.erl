-module(type_check).

-export([ module/3
        , format_error/1
        ]).

-record(state, { fun_sigs      = dict:new()
               , type_aliases  = dict:new()
               , declared_fun  = dict:new()
               , type_used     = gb_sets:new()
               , type_used_loc = dict:new()
               }).

-define(TYPE_MSG, type_check).

module(Forms, FileName, Opts) ->
  run_passes(standard_passes(), Forms, FileName, Opts, []).

run_passes([P | Ps], Fs, Fn, Opts, Ws0) ->
  try
    case P(Fs, Fn, Opts) of
      {ok, Ws} ->
        run_passes(Ps, Fs, Fn, Opts, Ws0 ++ Ws);
      {error, _, _} = E ->
        E
    end
  catch
    _:{error, L, Desc} -> {error, [{Fn, [{L, ?TYPE_MSG, Desc}]}], []};
    _:L when is_list(L) -> {error, L, []};
    _:Err -> io:format("Something bad happened, type system apologizes: ~p~n"
                    , [Err])
  end;
run_passes([], _, _, _, Ws) ->
  {ok, Ws}.

standard_passes() ->
  [fun type_lint/3].

type_lint(Forms, FileName, _Opts) ->
  io:format("~p~n", [Forms]),
  St = collect_types(Forms, #state{}),
  match_fun_sig_with_declared_fun(St),
  Ws1 = check_unsued_user_defined_types(FileName, St),
  check_undefined_types(FileName, St),
  %% TODO: checks for generic types
  %% - RHS usage
  %% - Expressions with generic types
  {ok, Ws1}.


%% Collect forms of interest into state record
collect_types([{fun_sig, L, _, T} = Form | Forms], St0) ->
  St1 = extract_user_defined_types_with_locs(T, L, St0),
  collect_types(Forms, add_fun_sigs(Form, St1));
collect_types([{type_alias, L, _, T} = Form | Forms], St0) ->
  St1 = extract_user_defined_types_with_locs(T, L, St0),
  collect_types(Forms, add_type_alias(Form, St1));
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

add_declared_fun({function, _, N, _, _} = F, St=#state{declared_fun = DF}) ->
  St#state{declared_fun = dict:append(N, F, DF)}.

fun_arity({fun_sig, _, _, {fun_type, I, _}}) ->
  length(I).

extract_user_defined_types({fun_type, Is, O}) ->
  lists:flatten([extract_user_defined_types(I) || I <- Is])
    ++ extract_user_defined_types(O);
extract_user_defined_types({terl_user_defined, T}) ->
  [T];
extract_user_defined_types({union_type, Ts}) ->
  lists:flatten([extract_user_defined_types(T) || T <- Ts]);
extract_user_defined_types({list_type, T}) ->
  extract_user_defined_types(T);
extract_user_defined_types({tuple_type, Ts}) ->
  lists:flatten([extract_user_defined_types(T) || T <- Ts]);
extract_user_defined_types({record_type, _, Ts}) ->
  lists:flatten([extract_user_defined_types(T) || T <- Ts]);
extract_user_defined_types({terl_type, _}) ->
  [];
extract_user_defined_types({terl_generic_type, _}) ->
  [];
extract_user_defined_types({terl_type_ref, _, _}) ->
  [];
extract_user_defined_types(W) ->
  throw({fatal_error, not_recognized, W}).


%%% Format errors
format_error({duplicate_fun_sig_decl, N, L1, L2}) ->
  io_lib:format(
    "Duplicate function signature definitions '~w' at line ~p and ~p.",
    [N, L1, L2]);
format_error({duplicate_type_alias_decl, N, L1, L2}) ->
  io_lib:format(
    "Duplicate type alias definitions '~w' at line ~p and ~p.",
    [N, L1, L2]);
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
format_error(W) ->
  io_lib:format("Undefined Error in type system: ~p ", [W]).
