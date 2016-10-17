-module(type_err_msg).

-export([ format_error/1
        , pp_type/1
        ]).

format_error(T) ->
  try
    format_error0(T)
  catch
    E:_ ->
      io:format("Failed to do format_error for ~p, got ~p~n~p~n",
                [T, E, erlang:get_stacktrace()])
  end.


format_error0({no_remote_fun_sig_allowed, M, N}) ->
  io_lib:format(
    "No remote function signature is allowed: '~p:~p'",
    [M, N]);

format_error0({duplicate_fun_sig_decl, N, L1, L2}) ->
  io_lib:format(
    "Duplicate function signature definitions '~w' at line ~p and ~p.",
    [N, L1, L2]);

format_error0({duplicate_type_alias_decl, N, L1, L2}) ->
  io_lib:format(
    "Type alias definition '~w' at line ~p is already defined with "
    "the same name at ~p.",
    [N, L2, L1]);

format_error0({duplicate_type_cons_decl, N, L1, L2}) ->
  io_lib:format(
    "Type constructor definition '~w' at line ~p is already defined"
    " with same name at ~p.",
    [N, L2, L1]);

format_error0({no_fun_decl_found_for_sig, N, L2}) ->
  io_lib:format(
    "No function implementation found for declared function signature '~w' "
    "at line ~p.",
    [N, L2]);

format_error0({no_matching_fun_decl_for_fun_sig, N, Ar, L2}) ->
  io_lib:format(
    "No function implementation matched with declared function signature "
    "'~w'/~p at line ~p.",
    [N, Ar, L2]);

format_error0({fun_sig_clause_arity_not_match, N}) ->
  io_lib:format(
    "Function signature ~w has clauses with different arity.",
    [N]);

format_error0({non_matching_fun_args, FT1, FT2}) ->
  io_lib:format(
    "Function '~s' has different arity than function '~s' when trying to "
    "materialise generic types",
    [pp_type(FT1), pp_type(FT2)]);

format_error0({non_matching_tuple_length, TT1, TT2}) ->
  io_lib:format(
    "Tuple '~s' has different size than tuple '~s' when trying to "
    "materialise generic types",
    [pp_type(TT1), pp_type(TT2)]);

format_error0({can_not_instantiate_generic_type, T, Vs}) ->
  io_lib:format(
    "Type parameter ~p can be materialised to several types in "
    "function call: ~s",
    [T, list_to_string_sep([pp_type(V) || V <- Vs], ", ")]);

format_error0({multi_match_fun_decl_for_fun_sig, N, L2}) ->
  io_lib:format(
    "Multiple function implementations matched with declared function"
    " signature '~w' at line ~p. This is a fatal error in compiler!",
    [N, L2]);

format_error0({duplicate_record_type, N, L1, L2}) ->
  io_lib:format(
    "Record type duplication: #~p is defined at line ~p and ~p",
    [N, L1, L2]);

format_error0({function_pointer_not_found, N, A}) ->
  io_lib:format(
    "Can no infer type for function pointer ~p/~p",
    [N, A]);

format_error0({no_record_definition, N}) ->
  io_lib:format(
    "No record definition found for type #~p",
    [N]);

format_error0({none_matching_record_type, N}) ->
  io_lib:format(
    "Record type definition #~p has non-matching number of elements " ++
      "with record definition",
    [N]);

format_error0({fun_head_fun_sig_size_mismatch, N, M, Name, Arity}) ->
  io_lib:format(
    "Declared function signature for ~p/~p has different head size than"
    " the function declaration: ~p vs. ~p",
    [Name, Arity, M, N]);

format_error0({inferred_conflicting_types, V, T1, T2}) ->
  io_lib:format(
    "~s has conflicting types: '~s' and '~s'",
    [pp_expr(V), pp_type(T1), pp_type(T2)]);

format_error0({type_alias_defined_not_used, N}) ->
  io_lib:format(
    "Type alias ~w defined but never used",
    [N]);

format_error0({undefined_type, N}) ->
  io_lib:format(
    "Undefined type '~w'",
    [N]);

format_error0({tc_generic_type_not_used_rhs, Ts}) ->
  io_lib:format(
    "Generic type parameter(s) ~s is not used in the right hand side of"
    " type constructor",
    [list_to_string(Ts, "")]);

format_error0({tc_generic_type_not_used_lhs, Ts}) ->
  io_lib:format(
    "Generic type parameter(s) ~s is not defined in the left hand side of"
    " type constructor",
    [list_to_string(Ts, "")]);

format_error0({tc_only_generic_type_lhs, TI}) ->
  io_lib:format(
    "Only generic type parameters are allowed in left hand side of "
    "type constructor definitions. ~s has/have violated this rule.",
    [list_to_string(TI, "")]);

format_error0({declared_inferred_not_match, Var, Declared, Inferred}) ->
  io_lib:format(
    "Expected variable ~p to be of type '~s' but is '~s'",
    [Var, pp_type(Declared), pp_type(Inferred)]);

format_error0({expected_binary_type, Var, WrongType}) ->
  io_lib:format(
    "Expected ~p to be of type 'Binary' but is '~s'",
    [pp_expr(Var), pp_type(WrongType)]);

format_error0({invalid_operator, Op, TL, TR}) ->
  io_lib:format(
    "Invalid operator ~p on types ~s and ~s",
    [Op, pp_type(TL), pp_type(TR)]);

format_error0({invalid_operator, Op, TR}) ->
  io_lib:format(
    "Illegal operator ~p on type ~s",
    [Op, pp_type(TR)]);

format_error0({can_not_infer_type, E}) ->
  io_lib:format(
    "Could not infer the type for ~s",
    [pp_expr(E)]);

format_error0({not_list_cons_position, T}) ->
  io_lib:format(
    "Expected a type of list in cons position but found ~s",
    [pp_type(T)]);

format_error0({match_on_unequally_sized_tuple, T}) ->
  io_lib:format(
    "Match on different tuple sizes. Right hand side tuple ~s is " ++
      "different from left hand side",
    [pp_type(T)]);

format_error0({can_not_infer_fun_type, N, A, FType}) ->
  io_lib:format(
    "Type system did its best to infer the type for '~p/~p' " ++
      "and what it got was '~s'.",
    [N, A, pp_type(FType)]);

format_error0({declared_inferred_fun_type_do_not_match, N, A, Sig, FType}) ->
  io_lib:format(
    "Declared function signature for '~p/~p' does not match the inferred " ++
      "one.~n\t\tDeclared:~n\t\t\t'~s'~n\t\tbut inferred:~n\t\t\t'~s'.",
    [N, A, pp_type(Sig), pp_type(FType)]);

format_error0({multiple_inferred_type, Expr, Ts}) ->
  io_lib:format(
    "Multiple types can be inferred for '~s':~n\t\t~s",
    [pp_expr(Expr), list_to_string_sep([pp_type(T) || T <- Ts], ", ")]);

format_error0({function_not_exported, M, N, Ar}) ->
  io_lib:format(
    "Calling non-exported function ~p:~p/~p",
    [M, N, Ar]);

format_error0({can_not_infer_type_fun, NN, Ar}) ->
  io_lib:format(
    "Can not infer type for ~s/~p or function does not exist",
    [pp_expr(NN), Ar]
   );

format_error0({can_not_infer_type_fun, M, NN, Ar}) ->
  io_lib:format(
    "Can not infer type for ~p:~s/~p or function does not exist",
    [M, pp_expr(NN), Ar]
   );

format_error0({non_matching_type_fun_call, N, Arity, Ind, Got, Expected}) ->
  io_lib:format(
    "~s argument in function call '~p/~p' has non-matching types, " ++
      "expected: ~s, but got: ~s",
    [ind_presentation(Ind), N, Arity, pp_type(Expected), pp_type(Got)]
   );

format_error0({multiple_match_for_function_call, MatchingTypes}) ->
  Matches = [pp_type(T) || T <- MatchingTypes],
  io_lib:format(
    "Function call can be matched with mulitple types:~n\t~s",
    [list_to_string_sep(Matches, "~n\t")]
   );

format_error0({conflicting_clause_var_type, V, T, T1}) ->
  io_lib:format(
    "Var ~p must be of type ~s but is ~s",
    [V, pp_type(T1), pp_type(T)]
   );

format_error0({type_error_case_expression, E}) ->
  io_lib:format(
    "Can not infer type for case expression ~s",
    [pp_expr(E)]
   );

format_error0({wrong_guard_type, G, WrongType}) ->
  io_lib:format(
    "Expected guard ~s to have type of 'Boolean' but has '~s'",
    [pp_expr(G), pp_type(WrongType)]
   );

format_error0({wrong_record_field_type, N, F, TF, TV}) ->
  io_lib:format(
    "Expected record field #~p.~p to have type ~s but has ~s",
    [N, F, pp_type(TF), pp_type(TV)]
   );

format_error0({record_type_not_found, N}) ->
  io_lib:format(
    "Record type #~p not found",
    [N]
   );

format_error0({bin_segment_conflicting_types, Var, Ts}) ->
  io_lib:format(
    "Variable ~p has conflicting types: ~s",
    [Var, list_to_string_sep([pp_type(T) || T <- Ts], $,)]
   );

format_error0(W) ->
  io_lib:format("Undefined Error in type system: ~p ", [W]).


ind_presentation(N) ->
  integer_to_list(N) ++ ind_presentation0(N rem 10).

ind_presentation0(1) ->
  "st";
ind_presentation0(2) ->
  "nd";
ind_presentation0(3) ->
  "rd";
ind_presentation0(_) ->
  "th".

list_to_string([], Res) ->
  Res;
list_to_string([H], Res) ->
  list_to_string([], io_lib:format("~s~p", [Res, H]));
list_to_string([H|[_, _] = T], Res) ->
  list_to_string(T, io_lib:format("~s~p, ", [Res, H]));
list_to_string([H|[_] = T], Res) ->
  list_to_string(T, io_lib:format("~s~p and ", [Res, H])).


pp_type({terl_type, T}) ->
  io_lib:format("~s", [T]);

pp_type({terl_generic_type, T}) ->
  io_lib:format("'~s'", [T]);

pp_type({list_type, T}) ->
  io_lib:format("[~s]", [pp_type(T)]);

pp_type({tuple_type, Ts}) ->
  TT = [pp_type(T) || T <- Ts],
  io_lib:format("{~s}", [list_to_string_sep(TT, ", ")]);

pp_type({fun_type, Is, O}) ->
  TIs = [pp_type(I) || I <- Is],
  io_lib:format("(~s) -> ~s", [list_to_string_sep(TIs, ", "), pp_type(O)]);

pp_type({untyped_fun, nil, nil}) ->
  "(..) -> ..";

pp_type({union_type, Ts}) ->
  TEs = [pp_type(T) || T <- Ts],
  io_lib:format("~s", [list_to_string_sep(TEs, " | ")]);

pp_type({terl_atom_type, T}) ->
  io_lib:format("~s", [T]);

pp_type({record_type, N}) ->
  io_lib:format("#~s", [N]);

pp_type(T) when is_list(T) ->
  Ts = [pp_type(TT) || TT <- T],
  io_lib:format("~s", [list_to_string_sep(Ts, "; ")]);

pp_type(undefined) ->
  "?";

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

pp_expr(S) when is_list(S) ->
  io_lib:format("~s", [S]);

pp_expr(V) ->
  io_lib:format("~p", [V]).


list_to_string_sep([], _) ->
  "";
list_to_string_sep(List, Sep) ->
  lists:flatten(lists:reverse(list_to_string_sep1(List, Sep, []))).

list_to_string_sep1([Head | []], _Sep, Acc) ->
  [Head | Acc];
list_to_string_sep1([Head | Tail], Sep, Acc) ->
  list_to_string_sep1(Tail, Sep, [Sep, Head | Acc]).
