-module(terl_pid).

-export([ op/1
        , op/2
        , lub/1
        ]).

-include("type_macros.hrl").

op('!', T)  -> T;
op(_, _)    -> ?INVALID.


op(_)       -> ?INVALID.

lub(?PID)   -> ?PID;
lub(_)      -> ?ANY.
