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

-module(terl_binary).

-behaviour(type_interface).

-export([ op/1
        , op/2
        , lub/1
        , type_specifier_list/1
        ]).

-export([ abs_form/0
        , lub/0
        , name/0
        ]).

-include("type_macros.hrl").

abs_form() -> {terl_type, 'Binary'}.
lub()      -> 'Any'.
name()     -> 'Binary'.


op(_, _) -> ?INVALID.

op(_)    -> ?INVALID.

%%-- least common supertype ----------------------

lub(?BINARY)         -> ?BINARY;
lub(_)               -> ?ANY.


type_specifier_list(default) ->
  [?INTEGER];
type_specifier_list(TSLs) ->
  Types = [tsl(T) || T <- TSLs],
  [T || T <- Types, T =/= nil].

tsl(binary)    -> ?BINARY;
tsl(integer)   -> ?INTEGER;
tsl(float)     -> ?FLOAT;
tsl(bytes)     -> ?BINARY;
tsl(bitstring) -> ?BINARY;
tsl(bits)      -> ?BINARY;
tsl(utf8)      -> ?INTEGER;
tsl(utf16)     -> ?INTEGER;
tsl(utf32)     -> ?INTEGER;
tsl(_)         -> nil.
