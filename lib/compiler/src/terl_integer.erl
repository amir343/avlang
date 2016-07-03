-module(terl_integer).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

op(_, undefined)     -> ?UNDEFINED;

op('/', integer)     -> ?INTEGER;
op('+', integer)     -> ?INTEGER;
op('-', integer)     -> ?INTEGER;
op('*', integer)     -> ?INTEGER;

op('band', integer)  -> ?INTEGER;
op('bor', integer)   -> ?INTEGER;
op('bxor', integer)  -> ?INTEGER;
op('bsl', integer)   -> ?INTEGER;
op('bsr', integer)   -> ?INTEGER;
op('bnot', integer)  -> ?INTEGER;

op('+', float)       -> ?FLOAT;
op('-', float)       -> ?FLOAT;
op('/', float)       -> ?FLOAT;
op('*', float)       -> ?FLOAT;

op('div', integer)   -> ?INTEGER;
op('div', float)     -> ?INVALID;

op('rem', integer)   -> ?INTEGER;
op('rem', float)     -> ?INVALID;

op('==', _)          -> ?BOOLEAN;
op('=:=', _)         -> ?BOOLEAN;

op('/=', _)          -> ?BOOLEAN;
op('=/=', _)         -> ?BOOLEAN;

op('>=', integer)    -> ?BOOLEAN;
op('>=', float)      -> ?BOOLEAN;

op('=<', integer)    -> ?BOOLEAN;
op('=<', float)      -> ?BOOLEAN;

op('<', integer)     -> ?BOOLEAN;
op('<', float)       -> ?BOOLEAN;

op('>', integer)     -> ?BOOLEAN;
op('>', float)       -> ?BOOLEAN;

op(_, _)             -> ?INVALID.

%%-- unary ---------------------------------------

op('+')              -> ?INTEGER;
op('-')              -> ?INTEGER;
op(_)                -> ?INVALID.
