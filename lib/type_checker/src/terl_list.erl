-module(terl_list).

-export([ op/1
        , op/3
        , lcs/2
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

op(_, _, undefined)               -> ?UNDEFINED;


op(_, _, _)                       -> ?INVALID.

%%-- unary ---------------------------------------

op(_)                             -> ?INVALID.

%%-- least common supertype ----------------------

lcs(T, {list_type, T})            -> {list_type, T};
lcs(T1, {list_type, T2})          -> {list_type, type_internal:lcs(T1, T2)};
lcs(_, _)                         -> ?ANY.
