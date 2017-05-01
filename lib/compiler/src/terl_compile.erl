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

-module(terl_compile).

%% High-level interface.
-export([ file/1
        , file/2
        , files/1
        , files/2
        , format_error/1
        , iofile/1
        ]).

-export([ forms/1
        , forms/2
        ]).

-include("erl_compile.hrl").
-include("include/terl_compiler.hrl").

-import(lists, [member/2,reverse/1,reverse/2,keyfind/3,last/1,
                map/2,flatmap/2,foreach/2,foldr/3]).

%%----------------------------------------------------------------------

-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.

-type err_info() :: {terl_anno:line() | 'none',
                     module(), term()}. %% ErrorDescriptor
-type errors()   :: [{file:filename(), [err_info()]}].
-type warnings() :: [{file:filename(), [err_info()]}].
-type mod_ret()  :: {'ok', module()}
                  | {'ok', module(), cerl:c_module()} %% with option 'to_core'
                  | {'ok', module(), warnings()}.
-type bin_ret()  :: {'ok', module(), binary()}
                  | {'ok', module(), binary(), warnings()}.
-type err_ret()  :: 'error' | {'error', errors(), warnings()}.
-type comp_ret() :: mod_ret() | bin_ret() | err_ret().

%%----------------------------------------------------------------------


-define(DEFAULT_OPTIONS, [verbose,report_errors,report_warnings]).

-spec file(module() | file:filename()) -> comp_ret().
file(File) -> files([File], ?DEFAULT_OPTIONS).

-spec files([module() | [file:filename()]]) -> comp_ret().
files(Files) when is_list(Files) ->
  files(Files, ?DEFAULT_OPTIONS).

-spec file(module() | file:filename(), [option()] | option()) -> comp_ret().
file(File, Opts) when is_list(Opts) ->
  do_compile({files, [File]}, Opts++env_default_opts());
file(File, Opt) ->
  file(File, [Opt|?DEFAULT_OPTIONS]).

files(Files, Opts) when is_list(Files), is_list(Opts) ->
  do_compile({files, Files}, Opts ++ env_default_opts());
files(Files, Opt) when is_list(Files) ->
  files(Files, [Opt|?DEFAULT_OPTIONS]).

forms(File) -> forms(File, ?DEFAULT_OPTIONS).

forms(Forms, Opts) when is_list(Opts) ->
  do_compile({forms,Forms}, [binary|Opts++env_default_opts()]);
forms(Forms, Opt) when is_atom(Opt) ->
  forms(Forms, [Opt|?DEFAULT_OPTIONS]).

%%
%%  Local functions
%%

env_default_opts() ->
  Key = "ERL_COMPILER_OPTIONS",
  case os:getenv(Key) of
    false -> [];
    Str when is_list(Str) ->
      case terl_scan:string(Str) of
        {ok,Tokens,_} ->
          Dot = {dot, terl_anno:new(1)},
          case terl_parse:parse_term(Tokens ++ [Dot]) of
            {ok,List} when is_list(List) -> List;
            {ok,Term} -> [Term];
            {error,_Reason} ->
              io:format("Ignoring bad term in ~s\n", [Key]),
              []
          end;
        {error, {_,_,_Reason}, _} ->
          io:format("Ignoring bad term in ~s\n", [Key]),
          []
      end
  end.

do_compile(Input, Opts0) ->
  Opts = expand_opts(Opts0),
  {Pid,Ref} =
    spawn_monitor(
      fun() ->
          exit(try
                 internal(Input, Opts)
               catch
                 error:Reason ->
                   St = erlang:get_stacktrace(),
                   {error,{Reason, St}}
               end)
      end),
  receive
    {'DOWN',Ref,process,Pid,Rep} -> Rep
  end.

expand_opts(Opts0) ->
  %% {debug_info_key,Key} implies debug_info.
  Opts = case {proplists:get_value(debug_info_key, Opts0),
               proplists:get_value(encrypt_debug_info, Opts0),
               proplists:get_bool(debug_info, Opts0)} of
           {undefined,undefined,_} -> Opts0;
           {_,_,false} -> [debug_info|Opts0];
           {_,_,_} -> Opts0
         end,
  foldr(fun expand_opt/2, [], Opts).

expand_opt(basic_validation, Os) ->
  [no_code_generation,to_pp,binary|Os];
expand_opt(strong_validation, Os) ->
  [no_code_generation,to_kernel,binary|Os];
expand_opt(report, Os) ->
  [report_errors,report_warnings|Os];
expand_opt(return, Os) ->
  [return_errors,return_warnings|Os];
expand_opt(r12, Os) ->
  [no_recv_opt,no_line_info|Os];
expand_opt(r13, Os) ->
  [no_recv_opt,no_line_info|Os];
expand_opt(r14, Os) ->
  [no_line_info|Os];
expand_opt({debug_info_key,_}=O, Os) ->
  [encrypt_debug_info,O|Os];
expand_opt(no_float_opt, Os) ->
  %%Turn off the entire type optimization pass.
  [no_topt|Os];
expand_opt(O, Os) -> [O|Os].

%% format_error(ErrorDescriptor) -> string()

format_error(no_native_support) ->
  "this system is not configured for native-code compilation.";
format_error(no_crypto) ->
  "this system is not configured with crypto support.";
format_error(bad_crypto_key) ->
  "invalid crypto key.";
format_error(no_crypto_key) ->
  "no crypto key supplied.";
format_error({native, E}) ->
  io_lib:fwrite("native-code compilation failed with reason: ~tP.",
                [E, 25]);
format_error({native_crash,E,Stk}) ->
  io_lib:fwrite("native-code compilation crashed with reason: ~tP.\n~tP\n",
                [E,25,Stk,25]);
format_error({open,E}) ->
  io_lib:format("open error '~ts'", [file:format_error(E)]);
format_error({epp,E}) ->
  terl_epp:format_error(E);
format_error(write_error) ->
  "error writing file";
format_error({rename,From,To,Error}) ->
  io_lib:format("failed to rename ~ts to ~ts: ~ts",
                [From,To,file:format_error(Error)]);
format_error({delete,File,Error}) ->
  io_lib:format("failed to delete file ~ts: ~ts",
                [File,file:format_error(Error)]);
format_error({delete_temp,File,Error}) ->
  io_lib:format("failed to delete temporary file ~ts: ~ts",
                [File,file:format_error(Error)]);
format_error({parse_transform,M,R}) ->
  io_lib:format("error in parse transform '~s': ~tp", [M, R]);
format_error({undef_parse_transform,M}) ->
  io_lib:format("undefined parse transform '~s'", [M]);
format_error({core_transform,M,R}) ->
  io_lib:format("error in core transform '~s': ~tp", [M, R]);
format_error({crash,Pass,Reason}) ->
  io_lib:format("internal error in ~p;\ncrash reason: ~ts",
                [Pass,format_error_reason(Reason)]);
format_error({bad_return,Pass,Reason}) ->
  io_lib:format("internal error in ~p;\nbad return value: ~ts",
                [Pass,format_error_reason(Reason)]);
format_error({module_name,Mod,Filename}) ->
  io_lib:format("Module name '~s' does not match file name '~ts'",
                [Mod,Filename]);
format_error(reparsing_invalid_unicode) ->
  "Non-UTF-8 character(s) detected, but no encoding declared. Encode the "
    "file in UTF-8 or add \"%% coding: latin-1\" at the beginning of the file."
    " Retrying with latin-1 encoding.".

format_error_reason({Reason, Stack}) when is_list(Stack) ->
  StackFun = fun
               (escript, run,      2) -> true;
               (escript, start,    1) -> true;
               (init,    start_it, 1) -> true;
               (init,    start_em, 1) -> true;
               (_Mod, _Fun, _Arity)   -> false
             end,
  FormatFun = fun (Term, _) -> io_lib:format("~tp", [Term]) end,
  [io_lib:format("~tp", [Reason]),"\n\n",
   lib:format_stacktrace(1, Stack, StackFun, FormatFun)];
format_error_reason(Reason) ->
  io_lib:format("~tp", [Reason]).


internal({forms,Forms}, Opts0) ->
  {_,Ps} = passes(forms, Opts0),
  Source = proplists:get_value(source, Opts0, ""),
  Opts1 = proplists:delete(source, Opts0),
  Compile = #compile{code=Forms,options=Opts1,mod_options=Opts1},
  internal_comp(Ps, Source, "", Compile);
internal({files, Files}, Opts) ->
  {Ext,Ps} = passes(file, Opts),
  Run = build_run(Opts),
  Compiles =
    lists:foldl(fun(File, Acc) ->
                    St0 = #compile{options = Opts, mod_options = Opts},
                    [build_state(File, Ext, St0) | Acc]
                end, [], Files),
  case fold_comps(Ps, Run, Compiles, Opts) of
    {ok, St2} -> comp_ret_ok(St2);
    {error, St2} ->
      comp_ret_err(St2)
  end.

internal_comp(Passes, File, Suffix, St0=#compile{options = Opts}) ->
  St1 = build_state(File, Suffix, St0),
  Run = build_run(Opts),
  case fold_comps(Passes, Run, [St1], Opts) of
    {ok,St2}    -> comp_ret_ok(St2);
    {error,St2} -> comp_ret_err(St2)
  end.

build_state(File, Suffix, St0) ->
  Dir = filename:dirname(File),
  Base = filename:basename(File, Suffix),
  St0#compile{filename=File, dir=Dir, base=Base,
              ifile=erlfile(Dir, Base, Suffix),
              ofile=objfile(Base, St0)}.

build_run(Opts) ->
  Run0 = case member(time, Opts) of
           true  ->
             fun run_tc/2;
           false -> fun({_Name, Fun}, St) ->
                        catch Fun(St)
                    end
         end,
  case keyfind(eprof, 1, Opts) of
    {eprof,EprofPass} ->
      fun(P, St) ->
          run_eprof(P, EprofPass, St)
      end;
    false ->
      Run0
  end.

fold_comps([{delay,Ps0}|Passes], Run, Compiles, Opts) ->
  Ps = select_passes(Ps0, Opts) ++ Passes,
  fold_comps(Ps, Run, Compiles, Opts);
fold_comps([{Name,Test,Pass}|Ps], Run, [C|_]=Compiles, Opts) ->
  case Test(C) of
    false ->        %Pass is not needed.
      fold_comps(Ps, Run, Compiles, Opts);
    true ->         %Run pass in the usual way.
      fold_comps([{Name,Pass}|Ps], Run, Compiles, Opts)
  end;
fold_comps([{type_check, Pass}|Ps], Run, Compiles, Opts) ->
  case Run({type_check, Pass}, Compiles) of
    {ok,St1} -> fold_comps(Ps, Run, St1, Opts);
    {error,_St1} = Error -> Error;
    {'EXIT',Reason} ->
      generate_errors(Compiles, crash, type_check, Reason);
    Other ->
      generate_errors(Compiles, bad_return, type_check, Other)
  end;
fold_comps([{Name,Pass}|Ps], Run, Compiles, Opts) ->
  {NErrs, Compiles1} =
    lists:foldl(fun(Compile, {Errs, Acc}) ->
                    case Run({Name,Pass}, Compile) of
                      {ok, C1} -> {Errs, Acc ++ [C1]};
                      {error, C1} -> {Errs + 1, Acc ++ [C1]};
                      {'EXIT',Reason} ->
                        {error, C1} =
                          generate_error(Compile, crash, Name, Reason),
                        {Errs + 1, Acc ++ [C1]};
                      Other ->
                        {error, C1} =
                          generate_error(Compile, bad_return, Name, Other),
                        {Errs + 1, Acc ++ [C1]}
                    end
                end, {0, []}, Compiles),
  case NErrs of
    0 -> fold_comps(Ps, Run, Compiles1, Opts);
    _ -> {error, Compiles1}
  end;
fold_comps([], _Run, Compiles, _Opts) -> {ok, Compiles}.

generate_errors(Compiles, Crash, Name, Reason) ->
  {error,
   lists:foldl(fun(St0, Acc) ->
                   {error, St1} = generate_error(St0, Crash, Name, Reason),
                   Acc ++ [St1]
               end, [], Compiles)}.

generate_error(St0, Crash, Name, Reason) ->
  Es = [{St0#compile.ifile, [{none, ?MODULE, {Crash, Name, Reason}}]}],
  {error,St0#compile{errors=St0#compile.errors ++ Es}}.

run_tc({Name,Fun}, St) ->
  T1 = erlang:monotonic_time(),
  Val = (catch Fun(St)),
  T2 = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(T2 - T1, native, milli_seconds),
  Mem0 = erts_debug:flat_size(Val)*erlang:system_info(wordsize),
  Mem = lists:flatten(io_lib:format("~.1f kB", [Mem0/1024])),
  io:format(" ~-30s: ~10.3f s ~12s\n",
            [Name,Elapsed/1000,Mem]),
  Val.

run_eprof({Name,Fun}, Name, St) ->
  io:format("~p: Running eprof\n", [Name]),
  c:appcall(tools, eprof, start_profiling, [[self()]]),
  Val = (catch Fun(St)),
  c:appcall(tools, eprof, stop_profiling, []),
  c:appcall(tools, eprof, analyze, []),
  Val;
run_eprof({_,Fun}, _, St) ->
  catch Fun(St).

comp_ret_ok(Compiles) when is_list(Compiles) ->
  Res =
    lists:map(fun(Compile) ->
                  comp_ret_ok(Compile)
              end, Compiles),
  {Oks, Errors} =
    lists:foldl(fun(C, {Oks, Errs}) ->
                    case element(1, C) of
                      error -> {Oks, [remove_tuple_head(C) | Errs]};
                      ok    -> {[remove_tuple_head(C) | Oks], Errs}
                    end
                end, {[], []}, Res),
  case length(Errors) of
    0 -> case Oks of
           [Ok] -> {ok, Ok};
           Oks  -> {ok, Oks}
         end;
    _ -> case Errors of
           [Error] -> {error, Error};
           Errors -> {error, Errors}
         end
  end;
comp_ret_ok(#compile{code=Code,warnings=Warn0,module=Mod,options=Opts}=St) ->
  case werror(St) of
    true ->
      case member(report_warnings, Opts) of
        true ->
          io:format("~p: warnings being treated as errors\n",
                    [?MODULE]);
        false ->
          ok
      end,
      comp_ret_err(St);
    false ->
      Warn = messages_per_file(Warn0),
      report_warnings(St#compile{warnings = Warn}),
      Ret1 = case member(binary, Opts) andalso
               not member(no_code_generation, Opts) of
               true -> [Code];
               false -> []
             end,
      Ret2 = case member(return_warnings, Opts) of
               true -> Ret1 ++ [Warn];
               false -> Ret1
             end,
      list_to_tuple([ok,Mod|Ret2])
  end.

remove_tuple_head(Tuple) ->
  List = tl(tuple_to_list(Tuple)),
  case List of
    [E] -> E;
    Es  -> Es
  end.

comp_ret_err(Compiles) when is_list(Compiles)  ->
  Errors =
    lists:map(fun(Compile) ->
                  remove_tuple_head(comp_ret_err(Compile))
              end, Compiles),
  case Errors of
    [Error] -> {error, Error};
    _       -> {error, Errors}
  end;
comp_ret_err(#compile{warnings=Warn0,errors=Err0,module=Mod,options=Opts}=St) ->
  Warn = messages_per_file(Warn0),
  Err = messages_per_file(Err0),
  report_errors(St#compile{errors=Err}),
  report_warnings(St#compile{warnings=Warn}),
  case member(return_errors, Opts) of
    true  -> {error,Mod,Err,Warn};
    false -> {error, Mod}
  end.

werror(#compile{options=Opts,warnings=Ws}) ->
  Ws =/= [] andalso member(warnings_as_errors, Opts).

%% messages_per_file([{File,[Message]}]) -> [{File,[Message]}]
messages_per_file(Ms) ->
  T = lists:sort([{File,M} || {File,Messages} <- Ms, M <- Messages]),
  PrioMs = [terl_scan, epp, terl_parse],
  {Prio0, Rest} =
    lists:mapfoldl(fun(M, A) ->
                       lists:partition(fun({_,{_,Mod,_}}) -> Mod =:= M;
                                          (_) -> false
                                       end, A)
                   end, T, PrioMs),
  Prio = lists:sort(fun({_,{L1,_,_}}, {_,{L2,_,_}}) -> L1 =< L2 end,
                    lists:append(Prio0)),
  flatmap(fun mpf/1, [Prio, Rest]).

mpf(Ms) ->
  [{File,[M || {F,M} <- Ms, F =:= File]} ||
    File <- lists:usort([F || {F,_} <- Ms])].

%% passes(forms|file, [Option]) -> {Extension,[{Name,PassFun}]}
%%  Figure out the extension of the input file and which passes
%%  that need to be run.

passes(Type, Opts) ->
  Passes = case Type of
             file  -> [{parse_module, fun parse_module/1} | standard_passes()];
             forms -> standard_passes()
           end,
  SelectedPasses = select_passes(Passes, Opts),
  {".erl", SelectedPasses}.

%% select_passes([Command], Opts) -> [{Name,Function}]
%%  Interpret the lists of commands to return a pure list of passes.
%%
%%  Command can be one of:
%%
%%    {pass,Mod}  Will be expanded to a call to the external
%%      function Mod:module(Code, Options).  This
%%      function must transform the code and return
%%      {ok,NewCode} or {error,Term}.
%%      Example: {pass,beam_codegen}
%%
%%    {Name,Fun}  Name is an atom giving the name of the pass.
%%          Fun is an 'fun' taking one argument: a compile record.
%%      The fun should return {ok,NewCompileRecord} or
%%      {error,NewCompileRecord}.
%%      Note: ?pass(Name) is equvivalent to {Name,fun Name/1}.
%%      Example: ?pass(parse_module)
%%
%%    {Name,Test,Fun} Like {Name,Fun} above, but the pass will be run
%%      (and listed by the `time' option) only if Test(St)
%%      returns true.
%%
%%    {src_listing,Ext} Produces an Erlang source listing with the
%%      the file extension Ext.  (Ext should not contain
%%      a period.)  No more passes will be run.
%%
%%    {listing,Ext} Produce an listing of the terms in the internal
%%      representation.  The extension of the listing
%%      file will be Ext.  (Ext should not contain
%%      a period.)   No more passes will be run.
%%
%%    done              End compilation at this point.
%%
%%    {done,Ext}        End compilation at this point. Produce a listing
%%                      as with {listing,Ext}, unless 'binary' is
%%                      specified, in which case the current
%%                      representation of the code is returned without
%%                      creating an output file.
%%
%%    {iff,Flag,Cmd}  If the given Flag is given in the option list,
%%      Cmd will be interpreted as a command.
%%      Otherwise, Cmd will be ignored.
%%      Example: {iff,dcg,{listing,"codegen}}
%%
%%    {unless,Flag,Cmd} If the given Flag is NOT given in the option list,
%%      Cmd will be interpreted as a command.
%%      Otherwise, Cmd will be ignored.
%%      Example: {unless,no_kernopt,{pass,sys_kernopt}}
%%

select_passes([{pass,Mod}|Ps], Opts) ->
  F = fun(St) ->
          case catch Mod:module(St#compile.code, St#compile.options) of
            {ok,Code} ->
              {ok,St#compile{code=Code}};
            {ok,Code,Ws} ->
              {ok,St#compile{code=Code,warnings=St#compile.warnings++Ws}};
            {error,Es} ->
              {error,St#compile{errors=St#compile.errors ++ Es}}
          end
      end,
  [{Mod,F}|select_passes(Ps, Opts)];
select_passes([{src_listing,Ext}|_], _Opts) ->
  [{listing,fun (St) -> src_listing(Ext, St) end}];
select_passes([{listing,Ext}|_], _Opts) ->
  [{listing,fun (St) -> listing(Ext, St) end}];
select_passes([done|_], _Opts) ->
  [];
select_passes([{done,Ext}|_], Opts) ->
  select_passes([{unless,binary,{listing,Ext}}], Opts);
select_passes([{iff,Flag,Pass}|Ps], Opts) ->
  select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless,Flag,Pass}|Ps], Opts) ->
  select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{_,Fun}=P|Ps], Opts) when is_function(Fun) ->
  [P|select_passes(Ps, Opts)];
select_passes([{delay,Passes0}|Ps], Opts) when is_list(Passes0) ->
  %% Delay evaluation of compiler options and which compiler passes to run.
  %% Since we must know beforehand whether a listing will be produced, we
  %% will go through the list of passes and evaluate all conditions that
  %% select a list pass.
  case select_list_passes(Passes0, Opts) of
    {done,Passes} ->
      [{delay,Passes}];
    {not_done,Passes} ->
      [{delay,Passes}|select_passes(Ps, Opts)]
  end;
select_passes([{_,Test,Fun}=P|Ps], Opts) when is_function(Test),
                                              is_function(Fun) ->
  [P|select_passes(Ps, Opts)];
select_passes([], _Opts) ->
  [];
select_passes([List|Ps], Opts) when is_list(List) ->
  case select_passes(List, Opts) of
    [] -> select_passes(Ps, Opts);
    Nested ->
      case last(Nested) of
        {listing,_Fun} -> Nested;
        _Other         -> Nested ++ select_passes(Ps, Opts)
      end
  end.

select_cond(Flag, ShouldBe, Pass, Ps, Opts) ->
  ShouldNotBe = not ShouldBe,
  case member(Flag, Opts) of
    ShouldBe    -> select_passes([Pass|Ps], Opts);
    ShouldNotBe -> select_passes(Ps, Opts)
  end.

%% select_list_passes([Pass], Opts) -> {done,[Pass]} | {not_done,[Pass]}
%%  Evaluate all conditions having to do with listings in the list of
%%  passes.

select_list_passes(Ps, Opts) ->
  select_list_passes_1(Ps, Opts, []).

select_list_passes_1([{iff,Flag,{listing,_}=Listing}|Ps], Opts, Acc) ->
  case member(Flag, Opts) of
    true -> {done,reverse(Acc, [Listing])};
    false -> select_list_passes_1(Ps, Opts, Acc)
  end;
select_list_passes_1([{iff,Flag,{done,Ext}}|Ps], Opts, Acc) ->
  case member(Flag, Opts) of
    false ->
      select_list_passes_1(Ps, Opts, Acc);
    true ->
      {done,case member(binary, Opts) of
              false -> reverse(Acc, [{listing,Ext}]);
              true -> reverse(Acc)
            end}
  end;
select_list_passes_1([{iff=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
  case select_list_passes(List0, Opts) of
    {done,List} -> {done,reverse(Acc) ++ List};
    {not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
  end;
select_list_passes_1([{unless=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
  case select_list_passes(List0, Opts) of
    {done,List} -> {done,reverse(Acc) ++ List};
    {not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
  end;
select_list_passes_1([P|Ps], Opts, Acc) ->
  select_list_passes_1(Ps, Opts, [P|Acc]);
select_list_passes_1([], _, Acc) ->
  {not_done,reverse(Acc)}.

%% The standard passes (almost) always run.

standard_passes() ->
  [
   {transform_module, fun transform_module/1}

  , {iff, makedep,[
                   {makedep, fun makedep/1},
                   {unless,binary, {makedep_output, fun makedep_output/1}}
                  ]}
  , {iff, makedep,done}

  , {iff, 'dpp', {listing,"pp"}}
  , {lint_module, fun lint_module/1}
  , {iff, 'P', {src_listing,"P"}}
  , {iff, 'to_pp', {done,"P"}}

  , {iff, 'dabstr', {listing,"abstr"}}
  , {iff, debug_info, {save_abstract_code, fun save_abstract_code/1}}

   %% Type checking pass
  , {type_check, fun type_check/1}

  , {expand_module, fun expand_module/1}
  , {iff, 'dexp', {listing,"expand"}}
  , {iff, 'E', {src_listing,"E"}}
  , {iff, 'to_exp', {done,"E"}}

   %% Conversion to Core Erlang.
  , {pass, terl_core}
  , {iff,'dcore',{listing,"core"}}
  , {iff,'to_core0',{done,"core"}}
  , {dump_core, fun dump_core/1}

   %% Conversion from Core Erlang that is done by Erlang compiler
  , {from_core, fun from_core/1}
  ].

%%%
%%% Compiler passes.
%%%

parse_module(St0) ->
  case do_parse_module(utf8, St0) of
    {ok,_}=Ret ->
      Ret;
    {error,_}=Ret ->
      Ret;
    {invalid_unicode,File,Line} ->
      case do_parse_module(latin1, St0) of
        {ok,St} ->
          Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
          {ok,St#compile{warnings=Es++St#compile.warnings}};
        {error,St} ->
          Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
          {error,St#compile{errors=Es++St#compile.errors}}
      end
  end.

do_parse_module(DefEncoding, #compile{ifile=File,options=Opts,dir=Dir}=St) ->
  R = terl_epp:parse_file(File,
                          [{includes,[".",Dir|inc_paths(Opts)]},
                           {macros,pre_defs(Opts)},
                           {default_encoding,DefEncoding},
                           extra]),
  case R of
    {ok,Forms,Extra} ->
      Encoding = proplists:get_value(encoding, Extra),
      case find_invalid_unicode(Forms, File) of
        none ->
          {ok,St#compile{code=Forms,encoding=Encoding}};
        {invalid_unicode,_,_}=Ret ->
          case Encoding of
            none ->
              Ret;
            _ ->
              {ok,St#compile{code=Forms,encoding=Encoding}}
          end
      end;
    {error,E} ->
      Es = [{St#compile.ifile,[{none,?MODULE,{epp,E}}]}],
      {error,St#compile{errors=St#compile.errors ++ Es}}
  end.

find_invalid_unicode([H|T], File0) ->
  case H of
    {attribute,_,file,{File,_}} ->
      find_invalid_unicode(T, File);
    {error,{Line,file_io_server,invalid_unicode}} ->
      {invalid_unicode,File0,Line};
    _Other ->
      find_invalid_unicode(T, File0)
  end;
find_invalid_unicode([], _) -> none.

compile_options([{attribute,_L,compile,C}|Fs]) when is_list(C) ->
  C ++ compile_options(Fs);
compile_options([{attribute,_L,compile,C}|Fs]) ->
  [C|compile_options(Fs)];
compile_options([_F|Fs]) -> compile_options(Fs);
compile_options([]) -> [].

clean_parse_transforms(Fs) ->
  clean_parse_transforms_1(Fs, []).

clean_parse_transforms_1([{attribute,L,compile,C0}|Fs], Acc) when is_list(C0) ->
  C = lists:filter(fun({parse_transform,_}) -> false;
                      (_) -> true
                   end, C0),
  clean_parse_transforms_1(Fs, [{attribute,L,compile,C}|Acc]);
clean_parse_transforms_1([{attribute,_,compile,{parse_transform,_}}|Fs], Acc) ->
  clean_parse_transforms_1(Fs, Acc);
clean_parse_transforms_1([F|Fs], Acc) ->
  clean_parse_transforms_1(Fs, [F|Acc]);
clean_parse_transforms_1([], Acc) -> reverse(Acc).

transforms(Os) -> [ M || {parse_transform,M} <- Os ].

transform_module(#compile{options=Opt,code=Code0}=St0) ->
  %% Extract compile options from code into options field.
  case transforms(Opt ++ compile_options(Code0)) of
    [] -> {ok,St0};       %No parse transforms.
    Ts ->
      %% Remove parse_transform attributes from the abstract code to
      %% prevent parse transforms to be run more than once.
      Code = clean_parse_transforms(Code0),
      St = St0#compile{code=Code},
      foldl_transform(St, Ts)
  end.

foldl_transform(St, [T|Ts]) ->
  Name = "transform " ++ atom_to_list(T),
  case code:ensure_loaded(T) =:= {module,T} andalso
    erlang:function_exported(T, parse_transform, 2) of
    true ->
      Fun = fun(S) ->
                T:parse_transform(S#compile.code, S#compile.options)
            end,
      Run = case member(time, St#compile.options) of
              true  -> fun run_tc/2;
              false -> fun({_Name,F}, S) -> catch F(S) end
            end,
      case Run({Name, Fun}, St) of
        {error,Es,Ws} ->
          {error,St#compile{warnings=St#compile.warnings ++ Ws,
                            errors=St#compile.errors ++ Es}};
        {'EXIT',R} ->
          Es = [{St#compile.ifile,[{none,compile,
                                    {parse_transform,T,R}}]}],
          {error,St#compile{errors=St#compile.errors ++ Es}};
        {warning, Forms, Ws} ->
          foldl_transform(
            St#compile{code=Forms,
                       warnings=St#compile.warnings ++ Ws}, Ts);
        Forms ->
          foldl_transform(St#compile{code=Forms}, Ts)
      end;
    false ->
      Es = [{St#compile.ifile,[{none,compile,
                                {undef_parse_transform,T}}]}],
      {error,St#compile{errors=St#compile.errors ++ Es}}
  end;
foldl_transform(St, []) -> {ok,St}.

%%% Fetches the module name from a list of forms. The module attribute must
%%% be present.
get_module([{attribute,_,module,M} | _]) -> M;
get_module([_ | Rest]) ->
  get_module(Rest).

%%% A #compile state is returned, where St.base has been filled in
%%% with the module name from Forms, as a string, in case it wasn't
%%% set in St (i.e., it was "").
add_default_base(St, Forms) ->
  F = St#compile.filename,
  case F of
    "" ->
      M = get_module(Forms),
      St#compile{base=atom_to_list(M)};
    _ ->
      St
  end.

type_check(Compiles) ->
  {Input, Opts} =
    case Compiles of
      [H|_] -> {Compiles, H#compile.options};
      _     -> {[Compiles], Compiles#compile.options}
    end,
  case type_check:modules(Input, Opts) of
    {ok, Rs} ->
      {ok, type_check_result(Rs)};
    {error, Rs} ->
      {error, type_check_result(Rs)}
  end.

type_check_result({Ws, Es, C=#compile{warnings = Ws1, errors = Es1}}) ->
  C#compile{warnings = Ws ++ Ws1, errors = Es ++ Es1};
type_check_result([_|_] = Rs) ->
  [type_check_result(R) || R <- Rs].

dump_core(St) ->
  listing("core", St).

from_core(St = #compile{ base = Base
                       , dir = Dir
                       , options = Opts
                       , errors = Errs0
                       , warnings = Ws0}) ->
  CoreFile = outfile(Base, "core", Opts),
  BeamFile = objfile(Base, St),
  Options = #options{ outdir = Dir
                    , output_type = beam
                    , outfile = BeamFile
                    , cwd = Dir},
  case compile:compile_core(CoreFile, CoreFile, Options) of
    ok                -> {ok, St};
    error             -> {error, St};
    {error, Errs, Ws} -> {error, St#compile{errors = Errs0 ++ Errs
                                           , warnings = Ws0 ++ Ws}}
  end.

lint_module(St) ->
  case terl_lint:module(St#compile.code,
                        St#compile.ifile, St#compile.options) of
    {ok,Ws} ->
      %% Insert name of module as base name, if needed. This is
      %% for compile:forms to work with listing files.
      St1 = add_default_base(St, St#compile.code),
      {ok,St1#compile{warnings=St1#compile.warnings ++ Ws}};
    {error,Es,Ws} ->
      {error,St#compile{warnings=St#compile.warnings ++ Ws,
                        errors=St#compile.errors ++ Es}}
  end.

makedep(#compile{code=Code,options=Opts}=St) ->
  Ifile = St#compile.ifile,
  Ofile = St#compile.ofile,

  %% Get the target of the Makefile rule.
  Target0 =
    case proplists:get_value(makedep_target, Opts) of
      undefined ->
        %% The target is derived from the output filename: possibly
        %% remove the current working directory to obtain a relative
        %% path.
        shorten_filename(Ofile);
      T ->
        %% The caller specified one.
        T
    end,

  %% Quote the target is the called asked for this.
  Target1 = case proplists:get_value(makedep_quote_target, Opts) of
              true ->
                %% For now, only "$" is replaced by "$$".
                Fun = fun
                        ($$) -> "$$";
                  (C)  -> C
                   end,
  map(Fun, Target0);
_ ->
  Target0
end,
Target = Target1 ++ ":",

%% List the dependencies (includes) for this target.
{MainRule,PhonyRules} = makedep_add_headers(
                          Ifile,          % The input file name.
                          Code,           % The parsed source.
                          [],             % The list of dependencies already added.
                          length(Target), % The current line length.
                          Target,         % The target.
                          "",             % Phony targets.
                          Opts),

%% Prepare the content of the Makefile. For instance:
%%   hello.erl: hello.hrl common.hrl
%%
%% Or if phony targets are enabled:
%%   hello.erl: hello.hrl common.hrl
%%
%%   hello.hrl:
%%
%%   common.hrl:
Makefile = case proplists:get_value(makedep_phony, Opts) of
             true -> MainRule ++ PhonyRules;
             _ -> MainRule
           end,
{ok,St#compile{code=iolist_to_binary([Makefile,"\n"])}}.

makedep_add_headers(Ifile, [{attribute,_,file,{File,_}}|Rest],
                    Included, LineLen, MainTarget, Phony, Opts) ->
  %% The header "File" exists, add it to the dependencies.
  {Included1,LineLen1,MainTarget1,Phony1} =
    makedep_add_header(Ifile, Included, LineLen, MainTarget, Phony, File),
  makedep_add_headers(Ifile, Rest, Included1, LineLen1,
                      MainTarget1, Phony1, Opts);
makedep_add_headers(Ifile, [{error,{_,epp,{include,file,File}}}|Rest],
                    Included, LineLen, MainTarget, Phony, Opts) ->
  %% The header "File" doesn't exist, do we add it to the dependencies?
  case proplists:get_value(makedep_add_missing, Opts) of
    true ->
      {Included1,LineLen1,MainTarget1,Phony1} =
        makedep_add_header(Ifile, Included, LineLen, MainTarget,
                           Phony, File),
      makedep_add_headers(Ifile, Rest, Included1, LineLen1,
                          MainTarget1, Phony1, Opts);
    _ ->
      makedep_add_headers(Ifile, Rest, Included, LineLen,
                          MainTarget, Phony, Opts)
  end;
makedep_add_headers(Ifile, [_|Rest], Included, LineLen,
                    MainTarget, Phony, Opts) ->
  makedep_add_headers(Ifile, Rest, Included,
                      LineLen, MainTarget, Phony, Opts);
makedep_add_headers(_Ifile, [], _Included, _LineLen,
                    MainTarget, Phony, _Opts) ->
  {MainTarget,Phony}.

makedep_add_header(Ifile, Included, LineLen, MainTarget, Phony, File) ->
  case member(File, Included) of
    true ->
      %% This file was already listed in the dependencies, skip it.
      {Included,LineLen,MainTarget,Phony};
    false ->
      Included1 = [File|Included],

      %% Remove "./" in front of the dependency filename.
      File1 = case File of
                "./" ++ File0 -> File0;
                _ -> File
              end,

      %% Prepare the phony target name.
      Phony1 = case File of
                 Ifile -> Phony;
                 _     -> Phony ++ "\n\n" ++ File1 ++ ":"
               end,

      %% Add the file to the dependencies. Lines longer than 76 columns
      %% are splitted.
      if
        LineLen + 1 + length(File1) > 76 ->
          LineLen1 = 2 + length(File1),
          MainTarget1 = MainTarget ++ " \\\n  " ++ File1,
          {Included1,LineLen1,MainTarget1,Phony1};
        true ->
          LineLen1 = LineLen + 1 + length(File1),
          MainTarget1 = MainTarget ++ " " ++ File1,
          {Included1,LineLen1,MainTarget1,Phony1}
      end
  end.

makedep_output(#compile{code=Code,options=Opts,ofile=Ofile}=St) ->
  %% Write this Makefile (Code) to the selected output.
  %% If no output is specified, the default is to write to a file named after
  %% the output file.
  Output0 = case proplists:get_value(makedep_output, Opts) of
              undefined ->
                %% Prepare the default filename.
                outfile(filename:basename(Ofile, ".beam"), "Pbeam", Opts);
              O ->
                O
            end,

  %% If the caller specified an io_device(), there's nothing to do. If he
  %% specified a filename, we must create it. Furthermore, this created file
  %% must be closed before returning.
  Ret = case Output0 of
          _ when is_list(Output0) ->
            case file:delete(Output0) of
              Ret2 when Ret2 =:= ok; Ret2 =:= {error,enoent} ->
                case file:open(Output0, [write]) of
                  {ok,IODev} ->
                    {ok,IODev,true};
                  {error,Reason2} ->
                    {error,open,Reason2}
                end;
              {error,Reason1} ->
                {error,delete,Reason1}
            end;
          _ ->
            {ok,Output0,false}
        end,

  case Ret of
    {ok,Output1,CloseOutput} ->
      try
        %% Write the Makefile.
        io:fwrite(Output1, "~ts", [Code]),
        %% Close the file if relevant.
        if
          CloseOutput -> ok = file:close(Output1);
          true -> ok
        end,
        {ok,St}
      catch
        exit:_ ->
          %% Couldn't write to output Makefile.
          Err = {St#compile.ifile,[{none,?MODULE,write_error}]},
          {error,St#compile{errors=St#compile.errors++[Err]}}
      end;
    {error,open,Reason} ->
      %% Couldn't open output Makefile.
      Err = {St#compile.ifile,[{none,?MODULE,{open,Reason}}]},
      {error,St#compile{errors=St#compile.errors++[Err]}};
    {error,delete,Reason} ->
      %% Couldn't open output Makefile.
      Err = {St#compile.ifile,[{none,?MODULE,{delete,Output0,Reason}}]},
      {error,St#compile{errors=St#compile.errors++[Err]}}
  end.

%% expand_module(State) -> State'
%%  Do the common preprocessing of the input forms.

expand_module(#compile{code=Code,options=Opts0}=St0) ->
  {Mod,Exp,Forms,Opts1} = terl_pre_expand:module(Code, Opts0),
  Opts = expand_opts(Opts1),
  {ok,St0#compile{module=Mod,options=Opts,code={Mod,Exp,Forms}}}.

save_abstract_code(#compile{ifile=File}=St) ->
  case abstract_code(St) of
    {ok,Code} ->
      {ok,St#compile{abstract_code=Code}};
    {error,Es} ->
      {error,St#compile{errors=St#compile.errors ++ [{File,Es}]}}
  end.

abstract_code(#compile{code=Code0,options=Opts,ofile=OFile}) ->
  Code = terl_parse:anno_to_term(Code0),
  Abstr = erlang:term_to_binary({raw_abstract_v1,Code}, [compressed]),
  case member(encrypt_debug_info, Opts) of
    true ->
      case keyfind(debug_info_key, 1, Opts) of
        {_,Key} ->
          encrypt_abs_code(Abstr, Key);
        false ->
          %% Note: #compile.module has not been set yet.
          %% Here is an approximation that should work for
          %% all valid cases.
          Module = list_to_atom(filename:rootname(filename:basename(OFile))),
          Mode = proplists:get_value(crypto_mode, Opts, des3_cbc),
          case beam_lib:get_crypto_key({debug_info, Mode, Module, OFile}) of
            error ->
              {error, [{none,?MODULE,no_crypto_key}]};
            Key ->
              encrypt_abs_code(Abstr, {Mode, Key})
          end
      end;
    false ->
      {ok, Abstr}
  end.

encrypt_abs_code(Abstr, Key0) ->
  try
    RealKey = generate_key(Key0),
    case start_crypto() of
      ok -> {ok,encrypt(RealKey, Abstr)};
      {error,_}=E -> E
    end
  catch
    error:_ ->
      {error,[{none,?MODULE,bad_crypto_key}]}
  end.

start_crypto() ->
  try crypto:start() of
      {error,{already_started,crypto}} -> ok;
      ok -> ok
  catch
    error:_ ->
      {error,[{none,?MODULE,no_crypto}]}
  end.

generate_key({Type,String}) when is_atom(Type), is_list(String) ->
  beam_lib:make_crypto_key(Type, String);
generate_key(String) when is_list(String) ->
  generate_key({des3_cbc,String}).

encrypt({des3_cbc=Type,Key,IVec,BlockSize}, Bin0) ->
  Bin1 = case byte_size(Bin0) rem BlockSize of
           0 -> Bin0;
           N -> list_to_binary([Bin0,random_bytes(BlockSize-N)])
         end,
  Bin = crypto:block_encrypt(Type, Key, IVec, Bin1),
  TypeString = atom_to_list(Type),
  list_to_binary([0,length(TypeString),TypeString,Bin]).

random_bytes(N) ->
  _ = random:seed(erlang:time_offset(),
                  erlang:monotonic_time(),
                  erlang:unique_integer()),
  random_bytes_1(N, []).

random_bytes_1(0, Acc) -> Acc;
random_bytes_1(N, Acc) -> random_bytes_1(N-1, [random:uniform(255)|Acc]).

%% report_errors(State) -> ok
%% report_warnings(State) -> ok

report_errors(#compile{options=Opts,errors=Errors}) ->
  case member(report_errors, Opts) of
    true ->
      foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
                  ({F,Eds}) -> list_errors(F, Eds) end,
              Errors);
    false -> ok
  end.

report_warnings(#compile{options=Opts,warnings=Ws0}) ->
  Werror = member(warnings_as_errors, Opts),
  P = case Werror of
        true -> "";
        false -> "Warning: "
      end,
  ReportWerror = Werror andalso member(report_errors, Opts),
  case member(report_warnings, Opts) orelse ReportWerror of
    true ->
      Ws1 = flatmap(fun({{F,_L},Eds}) -> format_message(F, P, Eds);
                       ({F,Eds}) -> format_message(F, P, Eds) end,
                    Ws0),
      Ws = lists:sort(Ws1),
      foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws);
    false -> ok
  end.

format_message(F, P, [{none,Mod,E}|Es]) ->
  M = {none,io_lib:format("~ts: ~s~ts\n", [F,P,Mod:format_error(E)])},
  [M|format_message(F, P, Es)];
format_message(F, P, [{{Line,Column}=Loc,Mod,E}|Es]) ->
  M = {{F,Loc},io_lib:format("~ts:~w:~w ~s~ts\n",
                             [F,Line,Column,P,Mod:format_error(E)])},
  [M|format_message(F, P, Es)];
format_message(F, P, [{Line,Mod,E}|Es]) ->
  M = {{F,{Line,0}},io_lib:format("~ts:~w: ~s~ts\n",
                                  [F,Line,P,Mod:format_error(E)])},
  [M|format_message(F, P, Es)];
format_message(F, P, [{Mod,E}|Es]) ->
  %% Not documented and not expected to be used any more, but
  %% keep a while just in case.
  M = {none,io_lib:format("~ts: ~s~ts\n", [F,P,Mod:format_error(E)])},
  [M|format_message(F, P, Es)];
format_message(_, _, []) -> [].

%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{none,Mod,E}|Es]) ->
  io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
  list_errors(F, Es);
list_errors(F, [{{Line,Column},Mod,E}|Es]) ->
  io:fwrite("~ts:~w:~w: ~ts\n", [F,Line,Column,Mod:format_error(E)]),
  list_errors(F, Es);
list_errors(F, [{Line,Mod,E}|Es]) ->
  io:fwrite("~ts:~w: ~ts\n", [F,Line,Mod:format_error(E)]),
  list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
  %% Not documented and not expected to be used any more, but
  %% keep a while just in case.
  io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
  list_errors(F, Es);
list_errors(_F, []) -> ok.

%% erlfile(Dir, Base) -> ErlFile
%% outfile(Base, Extension, Options) -> OutputFile
%% objfile(Base, Target, Options) -> ObjFile
%% tmpfile(ObjFile) -> TmpFile
%%  Work out the correct input and output file names.

iofile(File) when is_atom(File) ->
  iofile(atom_to_list(File));
iofile(File) ->
  {filename:dirname(File), filename:basename(File, ".erl")}.

erlfile(".", Base, Suffix) ->
  Base ++ Suffix;
erlfile(Dir, Base, Suffix) ->
  filename:join(Dir, Base ++ Suffix).

outfile(Base, Ext, Opts) when is_atom(Ext) ->
  outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
  Obase = case keyfind(outdir, 1, Opts) of
            {outdir, Odir} -> filename:join(Odir, Base);
            _Other -> Base      % Not found or bad format
          end,
  Obase ++ "." ++ Ext.

objfile(Base, St) ->
  outfile(Base, "beam", St#compile.options).

%% pre_defs(Options)
%% inc_paths(Options)
%%  Extract the predefined macros and include paths from the option list.

pre_defs([{d,M,V}|Opts]) ->
  [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
  [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
  pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
  [ P || {i,P} <- Opts, is_list(P) ].

src_listing(Ext, St) ->
  listing(fun (Lf, {_Mod,_Exp,Fs}) -> do_src_listing(Lf, Fs);
              (Lf, Fs) -> do_src_listing(Lf, Fs) end,
          Ext, St).

do_src_listing(Lf, Fs) ->
  Opts = [lists:keyfind(encoding, 1, io:getopts(Lf))],
  foreach(fun (F) -> io:put_chars(Lf, [terl_pp:form(F, Opts),"\n"]) end,
          Fs).

listing(Ext, St0) ->
  St = St0#compile{encoding = none},
  listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, St).

listing(LFun, Ext, St) ->
  Lfile = outfile(St#compile.base, Ext, St#compile.options),
  case file:open(Lfile, [write,delayed_write]) of
    {ok,Lf} ->
      Code = restore_expanded_types(Ext, St#compile.code),
      output_encoding(Lf, St),
      LFun(Lf, Code),
      ok = file:close(Lf),
      {ok,St};
    {error,_Error} ->
      Es = [{Lfile,[{none,compile,write_error}]}],
      {error,St#compile{errors=St#compile.errors ++ Es}}
  end.

output_encoding(F, #compile{encoding = none}) ->
  ok = io:setopts(F, [{encoding, terl_epp:default_encoding()}]);
output_encoding(F, #compile{encoding = Encoding}) ->
  ok = io:setopts(F, [{encoding, Encoding}]),
  ok = io:fwrite(F, <<"%% ~s\n">>, [terl_epp:encoding_to_string(Encoding)]).

restore_expanded_types("P", Fs) ->
  terl_epp:restore_typed_record_fields(Fs);
restore_expanded_types("E", {M,I,Fs0}) ->
  Fs1 = restore_expand_module(Fs0),
  Fs = terl_epp:restore_typed_record_fields(Fs1),
  {M,I,Fs};
restore_expanded_types(_Ext, Code) -> Code.

restore_expand_module([{attribute,Line,type,[Type]}|Fs]) ->
  [{attribute,Line,type,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,opaque,[Type]}|Fs]) ->
  [{attribute,Line,opaque,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,spec,[Arg]}|Fs]) ->
  [{attribute,Line,spec,Arg}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,callback,[Arg]}|Fs]) ->
  [{attribute,Line,callback,Arg}|restore_expand_module(Fs)];
restore_expand_module([F|Fs]) ->
  [F|restore_expand_module(Fs)];
restore_expand_module([]) -> [].


shorten_filename(Name0) ->
  {ok,Cwd} = file:get_cwd(),
  case lists:prefix(Cwd, Name0) of
    false -> Name0;
    true ->
      case lists:nthtail(length(Cwd), Name0) of
        "/"++N -> N;
        N -> N
      end
  end.

%%%_*---------------------------------------------------------------------------
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
