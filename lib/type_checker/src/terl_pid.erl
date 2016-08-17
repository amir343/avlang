-module(terl_pid).

-export([ op/1
        , op/2
        , lcs/1
        ]).

-include("type_macros.hrl").

op('!', T)  -> T;
op(_, _)    -> ?INVALID.


op(_)       -> ?INVALID.

lcs(?PID)   -> ?PID;
lcs(_)      -> ?ANY.
