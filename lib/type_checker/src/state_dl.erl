-module(state_dl).

-include("type_checker_state.hrl").


-compile([export_all]).

new_state() ->
  #state{current_module = #module_scope{}}.

erlang_types(#state{erlang_types = ET}) ->
  ET.
erlang_types(State=#state{}, ET) ->
  State#state{erlang_types = ET}.

guard_types(#state{guard_types = GT}) ->
  GT.
guard_types(State=#state{}, GT) ->
  State#state{guard_types = GT}.

compiler_opts(#state{compiler_opts = CO}) ->
  CO.
compiler_opts(State=#state{}, CO) ->
  State#state{compiler_opts = CO}.

first_pass(#state{first_pass = FP}) ->
  FP.
first_pass(State=#state{}, FP) ->
  State#state{first_pass = FP}.

module_scopes(#state{module_scopes = MS}) ->
  MS.

%%*.---------------------------------------------------------------------------

current_module(#state{current_module = CM}) ->
  CM.
current_module(State=#state{}, CM) ->
  State#state{current_module = CM}.

declared_fun(#state{current_module = #module_scope{declared_fun = DF}}) ->
  DF.
declared_fun(State=#state{current_module = CM}, DF) ->
  State#state{current_module = CM#module_scope{declared_fun = DF}}.

errors(#state{current_module = #module_scope{errors = Errs}}) ->
  Errs;
errors(#module_scope{errors = Errs}) ->
  Errs.

errors(State=#state{current_module = CM}, Errs) ->
  State#state{current_module = CM#module_scope{errors = Errs}};
errors(CM=#module_scope{}, Errs) ->
  CM#module_scope{errors = Errs}.

fun_sigs(#state{current_module = #module_scope{fun_sigs = FS}}) ->
  FS.
fun_sigs(State=#state{current_module = CM}, FS) ->
  State#state{current_module = CM#module_scope{fun_sigs = FS}}.

filename(#state{current_module = #module_scope{filename = F}}) ->
  F;
filename(#module_scope{filename = F}) ->
  F.

forms(#state{current_module = #module_scope{forms = Forms}}) ->
  Forms.

fun_lookup(#state{current_module = #module_scope{fun_lookup = FL}}) ->
  FL.
fun_lookup(State=#state{current_module = CM}, FL) ->
  State#state{current_module = CM#module_scope{fun_lookup = FL}}.

global(#module_scope{global = G}) ->
  G;
global(#state{current_module = #module_scope{global = G}}) ->
  G.
global(State=#state{current_module = CM}, G) ->
  State#state{current_module = CM#module_scope{global = G}}.

inner_scopes(#local_scope{inner_scopes = ISs}) ->
  ISs.
inner_scopes(LS=#local_scope{}, ISs) ->
  LS#local_scope{inner_scopes = ISs}.

last_ftype(#local_scope{last_ftype = LF}) ->
  LF.
last_ftype(LS=#local_scope{}, LF) ->
  LS#local_scope{last_ftype = LF}.

local(#state{current_module = #module_scope{local = L}}) ->
  L;
local(#module_scope{local = L}) ->
  L.

local(State=#state{current_module = CM}, L) ->
  State#state{current_module = CM#module_scope{local = L}}.

locals(#state{current_module = #module_scope{locals = LS}}) ->
  LS.
locals(State=#state{current_module = CM}, LS) ->
  State#state{current_module = CM#module_scope{locals = LS}}.

local_scope_name(#local_scope{name = Name}) ->
  Name.
local_scope_name(LS=#local_scope{}, Name) ->
  LS#local_scope{name = Name}.

module_scope(State=#state{module_scopes = MS}, ModuleName, ModuleScope) ->
  NMS = dict:store(ModuleName, ModuleScope, MS),
  State#state{module_scopes = NMS}.

module_scope(ModuleName, #state{module_scopes = MS}) ->
  case dict:find(ModuleName, MS) of
    {ok, M} -> M;
    error   -> nil
  end.

module_name(#state{current_module = #module_scope{module_name = M}}) ->
  M.
module_name(State=#state{current_module = CM}, M) ->
  State#state{current_module = CM#module_scope{module_name = M}}.

meta_var(Type, L) ->
  #meta_var{type = Type, line = L}.

new_local_scope(Name) ->
  #local_scope{name = Name}.

new_module_scope(Filename, Forms, Compile) ->
  #module_scope{filename = Filename, forms = Forms, compile_record = Compile}.

outer_scope(#local_scope{outer_scope = OS}) ->
  OS.
outer_scope(LS=#local_scope{}, OS) ->
  LS#local_scope{outer_scope = OS}.

remote_fun_sigs(#state{current_module = #module_scope{remote_fun_sigs = RF}}) ->
  RF.
remote_fun_sigs(State=#state{current_module = CM}, RF) ->
  State#state{current_module = CM#module_scope{remote_fun_sigs = RF}}.

record_types(#state{current_module = #module_scope{record_types = RT}}) ->
  RT.
record_types(State=#state{current_module = CM}, RT) ->
  State#state{current_module = CM#module_scope{record_types = RT}}.

records(#state{current_module = #module_scope{records = R}}) ->
  R.
records(State=#state{current_module = CM}, R) ->
  State#state{current_module = CM#module_scope{records = R}}.

type_used(#state{current_module = #module_scope{type_used = TU}}) ->
  TU.
type_used(State=#state{current_module = CM}, TU) ->
  State#state{current_module = CM#module_scope{type_used = TU}}.

type_used_loc(#state{current_module = #module_scope{type_used_loc = TUL}}) ->
  TUL.
type_used_loc(State=#state{current_module = CM}, TUL) ->
  State#state{current_module = CM#module_scope{type_used_loc = TUL}}.

type_aliases(#state{current_module = #module_scope{type_aliases = TA}}) ->
  TA.
type_aliases(State=#state{current_module = CM}, TA) ->
  State#state{current_module = CM#module_scope{type_aliases = TA}}.

type_cons(#state{current_module = #module_scope{type_cons = TC}}) ->
  TC.
type_cons(State=#state{current_module = CM}, TC) ->
  State#state{current_module = CM#module_scope{type_cons = TC}}.

type(#local_scope{type = Type}) ->
  Type.
type(LS=#local_scope{}, Type) ->
  LS#local_scope{type = Type}.

update_errors(State=#state{current_module = CM}, Errors)
  when is_list(Errors) ->
  Errs = errors(State),
  State#state{current_module = CM#module_scope{errors = Errs ++ Errors}}.

update_errors(State=#state{current_module = CM}, L, Msg) ->
  Errs = errors(State),
  State#state{
    current_module = CM#module_scope{errors = Errs ++ [{L, ?TYPE_MSG, Msg}]}}.

vars(#local_scope{vars = Vars}) ->
  Vars.
vars(LS=#local_scope{}, Vars) ->
  LS#local_scope{vars = Vars}.

warnings(#state{current_module = #module_scope{warnings = Ws}}) ->
  Ws;
warnings(#module_scope{warnings = Ws}) ->
  Ws.

warnings(State=#state{current_module = CM}, Ws) ->
  State#state{current_module = CM#module_scope{warnings = Ws}}.

compile_record(#module_scope{compile_record = CR}) ->
  CR.

%%*.---------------------------------------------------------------------------

last_nr_undefined(#local_scope{last_nr_undefined = LNU}) ->
  LNU;
last_nr_undefined(#state{last_nr_undefined = LNU}) ->
  LNU.

last_nr_undefined(LS=#local_scope{}, LNU) ->
  LS#local_scope{last_nr_undefined = LNU};
last_nr_undefined(State=#state{}, LNU) ->
  State#state{last_nr_undefined = LNU}.

%%*.---------------------------------------------------------------------------

%% Either find an already existing local scope or
%% initializes a new one
start_ls(Name, S=#state{}) ->
  L = find_ls(Name, S),
  local(S, L).

%% Same as `start_ls` except it nests the local scope
nest_ls(Name, S=#state{}) ->
  OuterScope = local(S),
  LS = locals(S),
  OuterScopeName = local_scope_name(OuterScope),
  OuterScope1 =
    inner_scopes(OuterScope,
                 gb_sets:add(Name, inner_scopes(OuterScope))),
  LS1 = dict:store(OuterScopeName, OuterScope1, LS),
  case dict:find(Name, LS) of
    {ok, L} ->
      locals(local(S, L), LS1);
    _ ->
      NewLS = outer_scope(
                local_scope_name(#local_scope{}, Name)
                                  , OuterScopeName),
      locals(local(S, NewLS), LS1)
  end.

%% Save current local scope for later uses
sync_ls(Name, S=#state{}) ->
  L = local(S),
  LS = locals(S),
  case L#local_scope.outer_scope of
    nil ->
      locals(S, dict:store(Name, L, LS));
    OS ->
      S1 = locals(S, dict:store(Name, L, LS)),
      local(S1, dict:fetch(OS, LS))
  end.

%% Cache the number of undefined type in current local scope
%% as long as it is zero.
update_undefined_types_in_local(UnDefs, S=#state{}) ->
  L = local(S),
  L1 = last_nr_undefined(L, UnDefs),
  local(S, L1).

%% Find local scope by its given name
find_ls(Name, S=#state{}) ->
  LS = locals(S),
  find_ls(Name, LS);

find_ls(Name, LocalsDict) ->
  case dict:find(Name, LocalsDict) of
    {ok, L} ->
      L;
    _ ->
      new_local_scope(Name)
  end.

update_type_in_local_scope(Type, State0) ->
  LS = type(local(State0), Type),
  local(State0, LS).

save_current_module_scope(State=#state{ current_module = CM
                                      , module_scopes = MS}) ->
  ModuleName = CM#module_scope.module_name,
  NewMS = dict:store(ModuleName, CM, MS),
  State#state{current_module = nil, module_scopes = NewMS}.


