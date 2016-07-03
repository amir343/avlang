-module(type_internal).

-export([ built_in/1
        , dispatch/2
        , dispatch/3
        , invalid_operator/0
        , tag_built_in/1
        , type_tag/1
        ]).

%% Binary operator dispatcher to respective types

dispatch({_, integer}, Op, {_, T}) ->
  terl_integer:op(Op, T);
dispatch({_, float}, Op, {_, T}) ->
  terl_float:op(Op, T);
dispatch({_, boolean}, Op, {_, T}) ->
  terl_boolean:op(Op, T);
dispatch({list_type, T1}, Op, T2) ->
  terl_list:op(Op, T1, T2);
dispatch(T, Op, T2) ->
  io:format("No dispatcher defined for Operator "
            ++ "~p and Type ~p and ~p~n", [Op, T, T2]),
  undefined.

%% Unary operator dispatcher to respective types

dispatch(Op, {_, boolean}) ->
  terl_boolean:op(Op);
dispatch(Op, {_, float}) ->
  terl_float:op(Op);
dispatch(Op, {_, integer}) ->
  terl_integer:op(Op);
dispatch(Op, {_, list_type}) ->
  terl_list:op(Op);
dispatch(Op, T) ->
  io:format("No dispatcher defined for Operator ~p and Type ~p~n", [Op, T]),
  undefined.

type_tag(Type) ->
  case built_in(Type) of
    true -> {terl_type, Type};
    false -> {terl_user_defined, Type}
  end.

invalid_operator() ->
  invalid_operator.

tag_built_in(Type) ->
  {terl_type, Type}.

built_in(any) -> true;
built_in(none) -> true;
built_in(atom) -> true;
built_in(integer) -> true;
built_in(pid) -> true;
built_in(reference) -> true;
built_in(binary) -> true;
built_in(string) -> true;
built_in(boolean) -> true;
built_in(float) -> true;

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
