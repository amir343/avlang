-module(state_dl).

-include("type_checker_state.hrl").


-compile([export_all]).


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

type_used(#state{type_used = TU}) ->
  TU.
type_used(State=#state{}, TU) ->
  State#state{type_used = TU}.

type_used_loc(#state{type_used_loc = TUL}) ->
  TUL.
type_used_loc(State=#state{}, TUL) ->
  State#state{type_used_loc = TUL}.

type_aliases(#state{type_aliases = TA}) ->
  TA.
type_aliases(State=#state{}, TA) ->
  State#state{type_aliases = TA}.

type_cons(#state{type_cons = TC}) ->
  TC.
type_cons(State=#state{}, TC) ->
  State#state{type_cons = TC}.

declared_fun(#state{declared_fun = DF}) ->
  DF.
declared_fun(State=#state{}, DF) ->
  State#state{declared_fun = DF}.

fun_sigs(#state{fun_sigs = FS}) ->
  FS.
fun_sigs(State=#state{}, FS) ->
  State#state{fun_sigs = FS}.

remote_fun_sigs(#state{remote_fun_sigs = RFS}) ->
  RFS.
remote_fun_sigs(State=#state{}, RFS) ->
  State#state{remote_fun_sigs = RFS}.

record_types(#state{record_types = RT}) ->
  RT.
record_types(State=#state{}, RT) ->
  State#state{record_types = RT}.

records(#state{records = R}) ->
  R.
records(State=#state{}, R) ->
  State#state{records = R}.

scopes(State=#state{}) ->
  #scopes{state = State}.

errors(#scopes{errors = Errs}) ->
  Errs.
errors(Scopes=#scopes{}, Errs) ->
  Scopes#scopes{errors = Errs}.

state(#scopes{state = St}) ->
  St.
state(Scopes=#scopes{}, St) ->
  Scopes#scopes{state = St}.

local(#scopes{local = L}) ->
  L.
local(Scopes=#scopes{}, L) ->
  Scopes#scopes{local = L}.

locals(#scopes{locals = LS}) ->
  LS.
locals(Scopes=#scopes{}, LS) ->
  Scopes#scopes{locals = LS}.

first_pass(#scopes{first_pass = FP}) ->
  FP.
first_pass(Scopes=#scopes{}, FP) ->
  Scopes#scopes{first_pass = FP}.

vars(#local_scope{vars = Vars}) ->
  Vars.
vars(LS=#local_scope{}, Vars) ->
  LS#local_scope{vars = Vars}.

type(#local_scope{type = Type}) ->
  Type.
type(LS=#local_scope{}, Type) ->
  LS#local_scope{type = Type}.

last_ftype(#local_scope{last_ftype = LF}) ->
  LF.
last_ftype(LS=#local_scope{}, LF) ->
  LS#local_scope{last_ftype = LF}.

global(#scopes{global = G}) ->
  G.
global(Scopes=#scopes{}, G) ->
  Scopes#scopes{global = G}.

last_nr_undefined(#local_scope{last_nr_undefined = LNU}) ->
  LNU.
last_nr_undefined(LS=#local_scope{}, LNU) ->
  LS#local_scope{last_nr_undefined = LNU}.

inner_scopes(#local_scope{inner_scopes = ISs}) ->
  ISs.
inner_scopes(LS=#local_scope{}, ISs) ->
  LS#local_scope{inner_scopes = ISs}.

fun_lookup(#scopes{fun_lookup = FL}) ->
  FL.
fun_lookup(Scopes=#scopes{}, FL) ->
  Scopes#scopes{fun_lookup = FL}.

meta_var(Type, L) ->
  #meta_var{type = Type, line = L}.

outer_scope(#local_scope{outer_scope = OS}) ->
  OS.
outer_scope(LS=#local_scope{}, OS) ->
  LS#local_scope{outer_scope = OS}.

local_scope_name(#local_scope{name = Name}) ->
  Name.
local_scope_name(LS=#local_scope{}, Name) ->
  LS#local_scope{name = Name}.

new_local_scope(Name) ->
  #local_scope{name = Name}.

update_errors(Scopes=#scopes{errors = Errs}, Errors) when is_list(Errors) ->
  Scopes#scopes{errors = Errs ++ Errors};
update_errors(State=#state{errors = Errs}, Errors) when is_list(Errors) ->
  State#state{errors = Errs ++ Errors}.

update_errors(Scopes=#scopes{errors = Errs}, L, Msg) ->
  Scopes#scopes{errors = Errs ++ [{L, ?TYPE_MSG, Msg}]};

update_errors(State=#state{errors = Errs}, L, Msg) ->
  State#state{errors = Errs ++ [{L, ?TYPE_MSG, Msg}]}.









