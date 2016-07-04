-module(terl_string).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

op(_, undefined) -> ?UNDEFINED;

op('++', string) -> ?STRING;
op('--', string) -> ?STRING;

op(_, _)         -> ?INVALID.

%%--- Unary ---------------------------------

op(_)            -> ?INVALID.
