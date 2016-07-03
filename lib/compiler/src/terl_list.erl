-module(terl_list).

-export([ op/1
        , op/3
        ]).

-include("type_macros.hrl").

op(_, _, undefined)               -> ?UNDEFINED;

op('++', T, {list_type, nothing}) -> {list_type, T};
op('--', T, {list_type, nothing}) -> {list_type, T};
op('++', nothing, {list_type, T}) -> {list_type, T};
op('--', nothing, {list_type, T}) -> {list_type, T};

op('++', T, {list_type, T})       -> {list_type, T};
op('--', T, {list_type, T})       -> {list_type, T};

op(_, _, _)                       -> ?INVALID.

%%-- unary ---------------------------------------

op(_)                             -> ?INVALID
