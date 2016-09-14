-module(terl_string).

-export([ op/1
        , op/2
        , lub/1
        ]).

-include("type_macros.hrl").

op('++', ?STRING) -> ?STRING;
op('--', ?STRING) -> ?STRING;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

op('++', undefined) -> ?STRING;
op('--', undefined) -> ?STRING;

op(_, undefined) -> ?UNDEFINED;

op(_, _)         -> ?INVALID.

%%--- Unary ---------------------------------

op(_)            -> ?INVALID.

%%-- least common supertype ----------------------

lub(?STRING)     -> ?STRING;
lub(_)           -> ?ANY.
