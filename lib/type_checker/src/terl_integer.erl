-module(terl_integer).

-export([ op/1
        , op/2
        , lub/1
        ]).

-include("type_macros.hrl").

op('/', ?INTEGER)     -> ?INTEGER;
op('+', ?INTEGER)     -> ?INTEGER;
op('-', ?INTEGER)     -> ?INTEGER;
op('*', ?INTEGER)     -> ?INTEGER;

op('band', ?INTEGER)  -> ?INTEGER;
op('bor', ?INTEGER)   -> ?INTEGER;
op('bxor', ?INTEGER)  -> ?INTEGER;
op('bsl', ?INTEGER)   -> ?INTEGER;
op('bsr', ?INTEGER)   -> ?INTEGER;
op('bnot', ?INTEGER)  -> ?INTEGER;

op('div', ?INTEGER)   -> ?INTEGER;
op('div', ?FLOAT)     -> ?INVALID;

op('rem', ?INTEGER)   -> ?INTEGER;
op('rem', ?FLOAT)     -> ?INVALID;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

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


op('+', ?FLOAT)       -> ?FLOAT;
op('-', ?FLOAT)       -> ?FLOAT;
op('/', ?FLOAT)       -> ?FLOAT;
op('*', ?FLOAT)       -> ?FLOAT;

op('==', _)          -> ?BOOLEAN;
op('=:=', _)         -> ?BOOLEAN;

op('/=', _)          -> ?BOOLEAN;
op('=/=', _)         -> ?BOOLEAN;

op('>=', ?INTEGER)    -> ?BOOLEAN;
op('>=', ?FLOAT)      -> ?BOOLEAN;

op('=<', ?INTEGER)    -> ?BOOLEAN;
op('=<', ?FLOAT)      -> ?BOOLEAN;

op('<', ?INTEGER)     -> ?BOOLEAN;
op('<', ?FLOAT)       -> ?BOOLEAN;

op('>', ?INTEGER)     -> ?BOOLEAN;
op('>', ?FLOAT)       -> ?BOOLEAN;

op(_, _)             -> ?INVALID.

%%-- unary ---------------------------------------

op('+')              -> ?INTEGER;
op('-')              -> ?INTEGER;
op(_)                -> ?INVALID.

%%-- least common supertype ----------------------

lub(?INTEGER)        -> ?INTEGER;
lub(_)               -> ?ANY.
