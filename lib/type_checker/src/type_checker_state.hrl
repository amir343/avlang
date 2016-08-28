-ifndef(_TYPE_CHECK_STATE_HRL_).
-define(_TYPE_CHECK_STATE_HRL_, true).

-record(state,
        { fun_sigs         = dict:new()
          %% {key, [{fun_sig, L, N, [T]}]}
        , remote_fun_sigs  = dict:new()
          %% {key, [{fun_remote_sig, L, M, Fun, [T]}]}
        , type_aliases     = dict:new()
          %% {Key, {type_alias, L, N, T}}
        , type_cons        = dict:new()
          %% {Key, {type_cons, L, N, P, T}}
        , record_types     = dict:new()
          %% {Key, {record_type_def, L, N, T}}
               , records          = dict:new()
        , declared_fun     = dict:new()
          %% {Key, [{function, ...}]}
        , type_used        = gb_sets:new()
          %% Set(atom)
        , type_used_loc    = dict:new()
          %% {Key, [Line]}
        , errors           = []
        , compiler_opts    = []
        , erlang_types     = dict:new()
        , guard_types      = dict:new()}).


-record(meta_var,
        { type                = undefined
        , line                = -1}).

-record(local_scope,
        { name                = nil
          %% vars :: Dict(Var, #meta_var)
        , vars                = dict:new()
        , type                = undefined
          %% Pointer to outer local scope name
        , outer_scope         = nil
        , inner_scopes        = gb_sets:new()
          %% last fun type signature for fun expression
        , last_ftype          = undefined
        , last_nr_undefined   = infinty}).

-record(scopes,
        { local               = nil
        , locals              = dict:new()
          %% global :: Dict(N, [[{fun_type}]])
        , global              = dict:new()
        , state               = #state{}
        , errors              = []
        , fun_lookup          = []
        , first_pass          = true}).



-endif.
