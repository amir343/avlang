-module(terl_boolean).

-export([ op/1
        , op/2
        , lub/1
        ]).

-include("type_macros.hrl").

op('and', ?BOOLEAN)     -> ?BOOLEAN;
op('or', ?BOOLEAN)      -> ?BOOLEAN;
op('xor', ?BOOLEAN)     -> ?BOOLEAN;

op('andalso', ?BOOLEAN) -> ?BOOLEAN;
op('orelse', ?BOOLEAN)  -> ?BOOLEAN;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

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


%%-- least common supertype ----------------------

lub(?BOOLEAN)      -> ?BOOLEAN;
lub(_)             -> ?ANY.
