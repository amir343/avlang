-module(terl_float).

-export([op/2]).

op('/', float) ->
  {terl_type, float};
op('+', float) ->
  {terl_type, float};
op('-', float) ->
  {terl_type, float};
op('*', float) ->
  {terl_type, float};

op('+', integer) ->
  {terl_type, float};
op('*', integer) ->
  {terl_type, float};
op('/', integer) ->
  {terl_type, float};
op('-', integer) ->
  {terl_type, float};

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

op('>=', integer) ->
  {terl_type, boolean};
op('>=', float) ->
  {terl_type, boolean};

op('=<', integer) ->
  {terl_type, boolean};
op('=<', float) ->
  {terl_type, boolean};

op('<', integer) ->
  {terl_type, boolean};
op('<', float) ->
  {terl_type, boolean};

op('>', integer) ->
  {terl_type, boolean};
op('>', float) ->
  {terl_type, boolean};

op(_, _) ->
  type_internal:invalid_operator().
