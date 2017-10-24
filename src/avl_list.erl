%% Copyright (c) 2016-2017 Amir Moulavi
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(avl_list).

-export([ op/1
        , op/3
        , lub/2
        ]).

-include("type_macros.hrl").

op(Op, ?CHAR, T)                  -> ?STRING_MOD:op(Op, T);

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
