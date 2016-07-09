-module(terl_integer).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

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

op('div', integer)   -> ?INTEGER;
op('div', float)     -> ?INVALID;

op('rem', integer)   -> ?INTEGER;
op('rem', float)     -> ?INVALID;

op('/', undefined)   -> ?INTEGER;
op('+', undefined)   -> ?INTEGER;
op('-', undefined)   -> ?INTEGER;
op('*', undefined)   -> ?INTEGER;

op('band', undefined) -> ?INTEGER;
op('bor', undefined)  -> ?INTEGER;
op('bxor', undefined)  -> ?INTEGER;
op('bsl', undefined)  -> ?INTEGER;
op('bsr', undefined)  -> ?INTEGER;
op('bnot', undefined) -> ?INTEGER;

op('div', undefined)  -> ?INTEGER;
op('rem', undefined)  -> ?INTEGER;

op(_, undefined)     -> ?UNDEFINED;


op('+', float)       -> ?FLOAT;
op('-', float)       -> ?FLOAT;
op('/', float)       -> ?FLOAT;
op('*', float)       -> ?FLOAT;

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
