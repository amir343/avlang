-module(terl_float).

-export([ op/1
        , op/2
        ]).

-include("type_macros.hrl").

op(_, undefined)  -> ?UNDEFINED;

op('/', 'Float')    -> ?FLOAT;
op('+', 'Float')    -> ?FLOAT;
op('-', 'Float')    -> ?FLOAT;
op('*', 'Float')    -> ?FLOAT;

op('+', 'Integer')  -> ?FLOAT;
op('*', 'Integer')  -> ?FLOAT;
op('/', 'Integer')  -> ?FLOAT;
op('-', 'Integer')  -> ?FLOAT;

op('div', _)      -> ?INVALID;
op('rem', _)      -> ?INVALID;

op('==', _)       -> ?BOOLEAN;
op('=:=', _)      -> ?BOOLEAN;

op('/=', _)       -> ?BOOLEAN;
op('=/=', _)      -> ?BOOLEAN;

op('>=', 'Integer') -> ?BOOLEAN;
op('>=', 'Float')   -> ?BOOLEAN;

op('=<', 'Integer') -> ?BOOLEAN;
op('=<', 'Float')   -> ?BOOLEAN;

op('<', 'Integer')  -> ?BOOLEAN;
op('<', 'Float')    -> ?BOOLEAN;

op('>', 'Integer')  -> ?BOOLEAN;
op('>', 'Float')    -> ?BOOLEAN;

op(_, _)          -> ?INVALID.

%%-- unary ---------------------------------------

op('+')           -> ?FLOAT;
op('-')           -> ?FLOAT;

op(_)             -> ?INVALID.
