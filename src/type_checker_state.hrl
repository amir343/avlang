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


-ifndef(_TYPE_CHECK_STATE_HRL_).
-define(_TYPE_CHECK_STATE_HRL_, true).

-define(TYPE_MSG, type_err_msg).

-record(state,
        {
          compiler_opts       = []
        , export_whitelist    = gb_sets:new()
        , erlang_types        = dict:new()
        , first_pass          = true
        , guard_types         = dict:new()
        , module_scopes       = dict:new()
          %% {Key, #module_scope{}}
        , current_module      = nil
        , last_nr_undefined   = infinity
        }).

-record(module_scope,
        {
          module_name         = nil
        , compiler_options    = gb_sets:new()
        , declared_fun        = dict:new()
          %% {Key, [{function, ...}]}
        , errors              = []
        , exports             = gb_sets:new()
        , filename            = nil
        , forms               = []
        , fun_lookup          = []
        , fun_sigs            = dict:new()
          %% {key, [{fun_sig, L, N, [T]}]}
        , global              = dict:new()
        , local               = nil
        , locals              = dict:new()
          %% global :: Dict(N, [[{fun_type}]])
        , records             = dict:new()
        , record_types        = dict:new()
          %% {Key, {record_type_def, L, N, T}}
        , remote_fun_sigs     = dict:new()
          %% {key, [{fun_remote_sig, L, M, Fun, [T]}]}
        , type_aliases        = dict:new()
          %% {Key, {type_alias, L, N, T}}
        , type_cons           = dict:new()
          %% {Key, {type_cons, L, N, P, T}}
        , type_used           = gb_sets:new()
          %% Set(atom)
        , type_used_loc       = dict:new()
          %% {Key, [Line]}
        , warnings            = []
        , compile_record      = nil
        }).

-record(meta_var,
        {
          type                = undefined
        , line                = -1
        }).

-record(local_scope,
        {
          name                = nil
          %% vars :: Dict(Var, #meta_var)
        , vars                = dict:new()
        , type                = undefined
          %% Pointer to outer local scope name
        , outer_scope         = nil
        , inner_scopes        = gb_sets:new()
          %% last fun type signature for fun expression
        , last_ftype          = undefined
        , last_nr_undefined   = infinity
        }).


-endif.
