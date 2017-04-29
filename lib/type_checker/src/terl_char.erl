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

-module(terl_char).

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
-define(SELF, ?CHAR).

abs_form() -> {terl_type, 'Char'}.
lub()      -> 'Number'.
name()     -> 'Char'.

op('/', ?SELF)     -> ?SELF;
op('+', ?SELF)     -> ?SELF;
op('-', ?SELF)     -> ?SELF;
op('*', ?SELF)     -> ?SELF;

op('band', ?SELF)  -> ?SELF;
op('bor', ?SELF)   -> ?SELF;
op('bxor', ?SELF)  -> ?SELF;
op('bsl', ?SELF)   -> ?SELF;
op('bsr', ?SELF)   -> ?SELF;
op('bnot', ?SELF)  -> ?SELF;

op('div', ?SELF)   -> ?SELF;
op('div', ?FLOAT)  -> ?INVALID;

op('rem', ?SELF)   -> ?SELF;
op('rem', ?FLOAT)  -> ?INVALID;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

op('/', undefined)   -> ?SELF;
op('+', undefined)   -> ?SELF;
op('-', undefined)   -> ?SELF;
op('*', undefined)   -> ?SELF;

op('band', undefined) -> ?SELF;
op('bor', undefined)  -> ?SELF;
op('bxor', undefined) -> ?SELF;
op('bsl', undefined)  -> ?SELF;
op('bsr', undefined)  -> ?SELF;
op('bnot', undefined) -> ?SELF;

op('div', undefined)  -> ?SELF;
op('rem', undefined)  -> ?SELF;

op('+', ?FLOAT)       -> ?FLOAT;
op('-', ?FLOAT)       -> ?FLOAT;
op('/', ?FLOAT)       -> ?FLOAT;
op('*', ?FLOAT)       -> ?FLOAT;

op('==', _)           -> ?BOOLEAN;
op('=:=', _)          -> ?BOOLEAN;

op('/=', _)           -> ?BOOLEAN;
op('=/=', _)          -> ?BOOLEAN;

op('>=', ?SELF)       -> ?BOOLEAN;
op('>=', ?FLOAT)      -> ?BOOLEAN;

op('=<', ?SELF)       -> ?BOOLEAN;
op('=<', ?FLOAT)      -> ?BOOLEAN;

op('<', ?SELF)        -> ?BOOLEAN;
op('<', ?FLOAT)       -> ?BOOLEAN;

op('>', ?SELF)        -> ?BOOLEAN;
op('>', ?FLOAT)       -> ?BOOLEAN;

op(_, undefined)      -> ?UNDEFINED;

op(_, _)              -> ?INVALID.

%%-- unary ---------------------------------------

op('+')              -> ?SELF;
op('-')              -> ?SELF;
op(_)                -> ?INVALID.

%%-- least common supertype ----------------------

lub(?SELF)           -> ?SELF;
lub(_)               -> ?ANY.
