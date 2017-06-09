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

-module(terl_boolean).

-behaviour(type_interface).

-export([ op/1
        , op/2
        , lub/1
        ]).

-include("type_macros.hrl").

-export([ abs_form/0
        , lub/0
        , name/0
        ]).

abs_form() -> {terl_type, 'Boolean'}.
lub()      -> 'Atom'.
name()     -> 'Boolean'.

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
