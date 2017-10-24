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

-module(avl_pid).

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

abs_form() -> {avl_type, 'Pid'}.
lub()      -> 'Any'.
name()     -> 'Pid'.

op('!', T)  -> T;
op(_, _)    -> ?INVALID.


op(_)       -> ?INVALID.

lub(?PID)   -> ?PID;
lub(_)      -> ?ANY.
