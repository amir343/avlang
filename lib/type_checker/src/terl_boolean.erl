-module(terl_boolean).

-export([ op/1
        , op/2]).

-include("type_macros.hrl").

op('and', boolean)     -> ?BOOLEAN;
op('or', boolean)      -> ?BOOLEAN;
op('xor', boolean)     -> ?BOOLEAN;

op('andalso', boolean) -> ?BOOLEAN;
op('orelse', boolean)  -> ?BOOLEAN;

op('and', undefined)   -> ?BOOLEAN;
op('or', undefined)    -> ?BOOLEAN;
op('xor', undefined)   -> ?BOOLEAN;

op('andalso', undefined) -> ?BOOLEAN;
op('orelse', undefined)  -> ?BOOLEAN;

op(_, undefined)       -> ?UNDEFINED;

op('/', _)            -> ?INVALID;
op('+', _)            -> ?INVALID;
op('-', _)            -> ?INVALID;
op('*', _)            -> ?INVALID;

op('div', _)          -> ?INVALID;
op('rem', _)          -> ?INVALID;

op('==', _)           -> ?BOOLEAN;
op('=:=', _)          -> ?BOOLEAN;

op('/=', _)           -> ?BOOLEAN;
op('=/=', _)          -> ?BOOLEAN;

op('>=', _)           -> ?INVALID;
op('=<', _)           -> ?INVALID;
op('<', _)            -> ?INVALID;
op('>', _)            -> ?INVALID;

op(_, _)              -> ?INVALID.

%%-- unary ---------------------------------------

op('not')          -> ?BOOLEAN;
op(_)              -> ?INVALID.