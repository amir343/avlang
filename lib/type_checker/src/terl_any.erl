-module(terl_any).

-export([ op/1
        , op/2
        , lcs/1
        ]).

-include("type_macros.hrl").

op('==', _)          -> ?BOOLEAN;
op('=:=', _)         -> ?BOOLEAN;

op('/=', _)          -> ?BOOLEAN;
op('=/=', _)         -> ?BOOLEAN;

op(_, _) -> ?INVALID.

op(_)    -> ?INVALID.

lcs(_)   -> ?ANY.
