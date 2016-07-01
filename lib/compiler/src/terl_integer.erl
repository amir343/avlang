-module(terl_integer).

-export([op/2]).

op('/', integer) ->
  {terl_type, integer};
op('+', integer) ->
  {terl_type, integer};
op('-', integer) ->
  {terl_type, integer};
op('*', integer) ->
  {terl_type, integer};

op('+', float) ->
  {terl_type, float};
op('-', float) ->
  {terl_type, float};
op('/', float) ->
  {terl_type, float};
op('*', float) ->
  {terl_type, float};

op('div', integer) ->
  {terl_type, integer};
op('div', float) ->
  type_internal:invalid_operator();

op('rem', integer) ->
  {terl_type, integer};
op('rem', float) ->
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
