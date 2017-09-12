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

-define(ANY,       {avl_type, 'Any'}).
-define(ATOM,      {avl_type, 'Atom'}).
-define(BOOLEAN,   {avl_type, 'Boolean'}).
-define(BINARY,    {avl_type, 'Binary'}).
-define(CHAR,      {avl_type, 'Char'}).
-define(FLOAT,     {avl_type, 'Float'}).
-define(INTEGER,   {avl_type, 'Integer'}).
-define(PID,       {avl_type, 'Pid'}).
-define(TERM,      {avl_type, 'Term'}).
-define(STRING,    {avl_type, 'String'}).
-define(INVALID,   type_internal:invalid_operator()).
-define(UNDEFINED, undefined).


-define(ANY_MOD,     avl_any).
-define(BOOLEAN_MOD, avl_boolean).
-define(BINARY_MOD,  avl_binary).
-define(CHAR_MOD,    avl_char).
-define(FLOAT_MOD,   avl_float).
-define(INTEGER_MOD, avl_integer).
-define(LIST_MOD,    avl_list).
-define(PID_MOD,     avl_pid).
-define(STRING_MOD,  avl_string).
-define(UNION_MOD,   avl_union).
