-module(terl_float).

-export([ op/1
        , op/2
        , lcs/1
        ]).

-include("type_macros.hrl").

op(_, undefined)  -> ?UNDEFINED;

op('/', ?FLOAT)    -> ?FLOAT;
op('+', ?FLOAT)    -> ?FLOAT;
op('-', ?FLOAT)    -> ?FLOAT;
op('*', ?FLOAT)    -> ?FLOAT;

op('+', ?INTEGER)  -> ?FLOAT;
op('*', ?INTEGER)  -> ?FLOAT;
op('/', ?INTEGER)  -> ?FLOAT;
op('-', ?INTEGER)  -> ?FLOAT;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

op('div', _)      -> ?INVALID;
op('rem', _)      -> ?INVALID;

op('==', _)       -> ?BOOLEAN;
op('=:=', _)      -> ?BOOLEAN;

op('/=', _)       -> ?BOOLEAN;
op('=/=', _)      -> ?BOOLEAN;

op('>=', ?INTEGER) -> ?BOOLEAN;
op('>=', ?FLOAT)   -> ?BOOLEAN;

op('=<', ?INTEGER) -> ?BOOLEAN;
op('=<', ?FLOAT)   -> ?BOOLEAN;

op('<', ?INTEGER)  -> ?BOOLEAN;
op('<', ?FLOAT)    -> ?BOOLEAN;

op('>', ?INTEGER)  -> ?BOOLEAN;
op('>', ?FLOAT)    -> ?BOOLEAN;

op(_, _)          -> ?INVALID.

%%-- unary ---------------------------------------

op('+')           -> ?FLOAT;
op('-')           -> ?FLOAT;

op(_)             -> ?INVALID.

%%-- least common supertype ----------------------

lcs(?FLOAT)       -> ?FLOAT;
lcs(_)            -> ?ANY.
