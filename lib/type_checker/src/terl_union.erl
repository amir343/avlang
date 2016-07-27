-module(terl_union).

-export([ op/1
        , op/2
        , lcs/1
        ]).

-include("type_macros.hrl").

op(_, _) -> ?INVALID.

op(_)    -> ?INVALID.

lcs(_)   -> ?ANY.
