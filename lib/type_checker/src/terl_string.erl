-module(terl_string).

-export([ op/1
        , op/2
        , lcs/1
        ]).

-include("type_macros.hrl").

op('++', 'String') -> ?STRING;
op('--', 'String') -> ?STRING;

op('++', undefined) -> ?STRING;
op('--', undefined) -> ?STRING;

op(_, undefined) -> ?UNDEFINED;

op(_, _)         -> ?INVALID.

%%--- Unary ---------------------------------

op(_)            -> ?INVALID.

%%-- least common supertype ----------------------

lcs(?STRING)     -> ?STRING;
lcs(_)           -> ?ANY.
