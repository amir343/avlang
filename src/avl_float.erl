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

-module(avl_float).

-behaviour(type_interface).

-export([ op/1
        , op/2
        , lub/1
        ]).

-export([ abs_form/0
        , lub/0
        , name/0
        ]).

-include("type_macros.hrl").
-define(SELF, ?FLOAT).

abs_form() -> {avl_type, 'Float'}.
lub()      -> 'Number'.
name()     -> 'Float'.

op(_, undefined)  -> ?UNDEFINED;

op('/', ?SELF)    -> ?SELF;
op('+', ?SELF)    -> ?SELF;
op('-', ?SELF)    -> ?SELF;
op('*', ?SELF)    -> ?SELF;

op('+', ?INTEGER)  -> ?SELF;
op('*', ?INTEGER)  -> ?SELF;
op('/', ?INTEGER)  -> ?SELF;
op('-', ?INTEGER)  -> ?SELF;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

op('div', _)      -> ?INVALID;
op('rem', _)      -> ?INVALID;

op('==', _)       -> ?BOOLEAN;
op('=:=', _)      -> ?BOOLEAN;

op('/=', _)       -> ?BOOLEAN;
op('=/=', _)      -> ?BOOLEAN;

op('>=', ?INTEGER) -> ?BOOLEAN;
op('>=', ?SELF)   -> ?BOOLEAN;

op('=<', ?INTEGER) -> ?BOOLEAN;
op('=<', ?SELF)   -> ?BOOLEAN;

op('<', ?INTEGER)  -> ?BOOLEAN;
op('<', ?SELF)    -> ?BOOLEAN;

op('>', ?INTEGER)  -> ?BOOLEAN;
op('>', ?SELF)    -> ?BOOLEAN;

op(_, _)          -> ?INVALID.

%%-- unary ---------------------------------------

op('+')           -> ?SELF;
op('-')           -> ?SELF;

op(_)             -> ?INVALID.

%%-- least common supertype ----------------------

lub(?SELF)       -> ?SELF;
lub(_)            -> ?ANY.
