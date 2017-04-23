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

-module(type_api).

-export([ built_in_types/0
        , built_in_types_info/0
        , stdlib_types/0
        ]).


built_in_types() ->
  [M:name() || M <- built_in_modules()].

built_in_types_info() ->
  [{M:name(), M:lub(), M:abs_form()} || M <- built_in_modules()].

built_in_modules() ->
  [ terl_any
  , terl_atom
  , terl_binary
  , terl_boolean
  , terl_float
  , terl_integer
  , terl_none
  , terl_number
  , terl_reference
  , terl_string
  , terl_pid
  , terl_port
  ].

stdlib_types() ->
  lists:foreach(
    fun({K, Vs}) ->
        lists:foreach(
          fun(V) ->
              io:format("~p:: ~s~n", [K, type_err_msg:p_type(V)])
          end, Vs)
    end, dict:to_list(type_check:bootstrap_erlang_types())).
