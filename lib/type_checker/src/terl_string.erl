%% Copyright (c) 2016 Amir Moulavi
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

-module(terl_string).

-export([ op/1
        , op/2
        , lub/1
        ]).

-include("type_macros.hrl").

op('++', ?STRING) -> ?STRING;
op('--', ?STRING) -> ?STRING;

op(Op, {union_type, Ts}) ->
  [op(Op, T) || T <- Ts];

op('++', undefined) -> ?STRING;
op('--', undefined) -> ?STRING;

op(_, undefined) -> ?UNDEFINED;

op(_, _)         -> ?INVALID.

%%--- Unary ---------------------------------

op(_)            -> ?INVALID.

%%-- least common supertype ----------------------

lub(?STRING)     -> ?STRING;
lub(_)           -> ?ANY.
