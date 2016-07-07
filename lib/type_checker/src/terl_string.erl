-module(terl_string).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

op('++', string) -> ?STRING;
op('--', string) -> ?STRING;

op('++', undefined) -> ?STRING;
op('--', undefined) -> ?STRING;

op(_, undefined) -> ?UNDEFINED;

op(_, _)         -> ?INVALID.

%%--- Unary ---------------------------------

op(_)            -> ?INVALID.
