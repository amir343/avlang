-module(type_internal).

-export([ built_in/1
        , dispatch/2
        , dispatch/3
        , invalid_operator/0
        , tag_built_in/1
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
