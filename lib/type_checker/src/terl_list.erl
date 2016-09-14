-module(terl_list).

-export([ op/1
        , op/3
        , lub/2
        ]).

-include("type_macros.hrl").

op('++', T, {list_type, nothing}) -> {list_type, T};
op('--', T, {list_type, nothing}) -> {list_type, T};
op('++', nothing, {list_type, T}) -> {list_type, T};
op('--', nothing, {list_type, T}) -> {list_type, T};

op('++', T, {list_type, T})       -> {list_type, T};
op('--', T, {list_type, T})       -> {list_type, T};

op('++', T, undefined)            -> {list_type, T};
op('--', T, undefined)            -> {list_type, T};

op('==', _, _)                    -> ?BOOLEAN;
op('=:=', _, _)                   -> ?BOOLEAN;

op('/=', _, _)                    -> ?BOOLEAN;
op('=/=', _, _)                   -> ?BOOLEAN;

op(_, _, undefined)               -> ?UNDEFINED;


op(_, _, _)                       -> ?INVALID.

%%-- unary ---------------------------------------

op(_)                             -> ?INVALID.

%%-- least common supertype ----------------------

lub(T, {list_type, T})            -> {list_type, T};
lub(T1, {list_type, T2})          -> {list_type, type_internal:lub(T1, T2)};
lub(_, _)                         -> ?ANY.
