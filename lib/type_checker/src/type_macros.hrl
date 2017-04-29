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

-define(ANY,       {terl_type, 'Any'}).
-define(ATOM,      {terl_type, 'Atom'}).
-define(BOOLEAN,   {terl_type, 'Boolean'}).
-define(BINARY,    {terl_type, 'Binary'}).
-define(CHAR,      {terl_type, 'Char'}).
-define(FLOAT,     {terl_type, 'Float'}).
-define(INTEGER,   {terl_type, 'Integer'}).
-define(PID,       {terl_type, 'Pid'}).
-define(TERM,      {terl_type, 'Term'}).
-define(STRING,    {terl_type, 'String'}).
-define(INVALID,   type_internal:invalid_operator()).
-define(UNDEFINED, undefined).


-define(ANY_MOD,     terl_any).
-define(BOOLEAN_MOD, terl_boolean).
-define(BINARY_MOD,  terl_binary).
-define(CHAR_MOD,    terl_char).
-define(FLOAT_MOD,   terl_float).
-define(INTEGER_MOD, terl_integer).
-define(LIST_MOD,    terl_list).
-define(PID_MOD,     terl_pid).
-define(STRING_MOD,  terl_string).
-define(UNION_MOD,   terl_union).
