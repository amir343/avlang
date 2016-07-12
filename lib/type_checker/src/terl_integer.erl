-module(terl_integer).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

op('/', 'Integer')     -> ?INTEGER;
op('+', 'Integer')     -> ?INTEGER;
op('-', 'Integer')     -> ?INTEGER;
op('*', 'Integer')     -> ?INTEGER;

op('band', 'Integer')  -> ?INTEGER;
op('bor', 'Integer')   -> ?INTEGER;
op('bxor', 'Integer')  -> ?INTEGER;
op('bsl', 'Integer')   -> ?INTEGER;
op('bsr', 'Integer')   -> ?INTEGER;
op('bnot', 'Integer')  -> ?INTEGER;

op('div', 'Integer')   -> ?INTEGER;
op('div', 'Float')     -> ?INVALID;

op('rem', 'Integer')   -> ?INTEGER;
op('rem', 'Float')     -> ?INVALID;

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


op('+', 'Float')       -> ?FLOAT;
op('-', 'Float')       -> ?FLOAT;
op('/', 'Float')       -> ?FLOAT;
op('*', 'Float')       -> ?FLOAT;

op('==', _)          -> ?BOOLEAN;
op('=:=', _)         -> ?BOOLEAN;

op('/=', _)          -> ?BOOLEAN;
op('=/=', _)         -> ?BOOLEAN;

op('>=', 'Integer')    -> ?BOOLEAN;
op('>=', 'Float')      -> ?BOOLEAN;

op('=<', 'Integer')    -> ?BOOLEAN;
op('=<', 'Float')      -> ?BOOLEAN;

op('<', 'Integer')     -> ?BOOLEAN;
op('<', 'Float')       -> ?BOOLEAN;

op('>', 'Integer')     -> ?BOOLEAN;
op('>', 'Float')       -> ?BOOLEAN;

op(_, _)             -> ?INVALID.

%%-- unary ---------------------------------------

op('+')              -> ?INTEGER;
op('-')              -> ?INTEGER;
op(_)                -> ?INVALID.
