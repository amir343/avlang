-ifndef(_TYPE_CHECK_STATE_HRL_).
-define(_TYPE_CHECK_STATE_HRL_, true).

-define(TYPE_MSG, type_err_msg).

-record(state,
        {
          compiler_opts       = []
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
        , declared_fun        = dict:new()
          %% {Key, [{function, ...}]}
        , errors              = []
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
