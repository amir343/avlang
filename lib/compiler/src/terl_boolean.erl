-module(terl_boolean).

-export([op/2]).

op('/', _) ->
  type_internal:invalid_operator();
op('+', _) ->
  type_internal:invalid_operator();
op('-', _) ->
  type_internal:invalid_operator();
op('*', _) ->
  type_internal:invalid_operator();

op('div', _) ->
  type_internal:invalid_operator();
op('rem', _) ->
  type_internal:invalid_operator();

op('==', _) ->
  {terl_type, boolean};
op('=:=', _) ->
  {terl_type, boolean};

op('/=', _) ->
  {terl_type, boolean};
op('=/=', _) ->
  {terl_type, boolean};

op('>=', _) ->
  type_internal:invalid_operator();

op('=<', _) ->
  type_internal:invalid_operator();

op('<', _) ->
  type_internal:invalid_operator();

op('>', _) ->
  type_internal:invalid_operator();

op(_, _) ->
  type_internal:invalid_operator().
