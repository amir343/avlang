-module(terl_float).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

op(_, undefined)  -> ?UNDEFINED;

op('/', float)    -> ?FLOAT;
op('+', float)    -> ?FLOAT;
op('-', float)    -> ?FLOAT;
op('*', float)    -> ?FLOAT;

op('+', integer)  -> ?FLOAT;
op('*', integer)  -> ?FLOAT;
op('/', integer)  -> ?FLOAT;
op('-', integer)  -> ?FLOAT;

op('div', _)      -> ?INVALID;
op('rem', _)      -> ?INVALID;

op('==', _)       -> ?BOOLEAN;
op('=:=', _)      -> ?BOOLEAN;

op('/=', _)       -> ?BOOLEAN;
op('=/=', _)      -> ?BOOLEAN;

op('>=', integer) -> ?BOOLEAN;
op('>=', float)   -> ?BOOLEAN;

op('=<', integer) -> ?BOOLEAN;
op('=<', float)   -> ?BOOLEAN;

op('<', integer)  -> ?BOOLEAN;
op('<', float)    -> ?BOOLEAN;

op('>', integer)  -> ?BOOLEAN;
op('>', float)    -> ?BOOLEAN;

op(_, _)          -> ?INVALID.

%%-- unary ---------------------------------------

op('+')           -> ?FLOAT;
op('-')           -> ?FLOAT;

op(_)             -> ?INVALID.
