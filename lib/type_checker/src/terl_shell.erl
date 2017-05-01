-module(terl_shell).

-export([ start/0 ]).

%% Shell commands
-export([ c/1
        , c/2
        , clear/0
        , exit/0
        , h/0
        , help/0
        , q/0
        ]).

-record(state, { eval
               , bs    = []
               , cnt   = 0
               }).


%% Colours for the banner
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").

start() ->
  spawn(fun() -> server() end).

server() ->
  process_flag(trap_exit, true),
  io:put_chars(generate_banner()),
  State = init_state(),
  server_loop(State).

init_state() ->
  EvalPid = spawn(fun() -> evaluator_loop() end),
  #state{ eval = EvalPid
        , bs = terl_eval:new_bindings()}.

server_loop(State) ->
  Prompt = prompt(),
  Ret = read_expression(Prompt),
  State1 =
    case Ret of
      {error, Error} ->
        report_error(Error),
        State;
      {ok, Exprs} ->
        evaluate(Exprs, State)
    end,
  server_loop(State1).

prompt() ->
  case is_alive() of
    true  -> io_lib:format("~s", [node_prompt()]);
    false -> io_lib:format("~s", [user_prompt()])
  end.

node_prompt() ->
  Prompt = user_prompt(),
  Node = atom_to_list(node()),
  ["(", Node, ") ", Prompt].

user_prompt() ->
  case init:get_argument(prompt) of
    {ok, [[]]} -> [""];
    {ok, [P]}  -> P;
    _          -> ["terl> "]
  end.

read_expression(Prompt) ->
  Read = fun() ->
             exit(read_one_expression(Prompt))
         end,
  Reader = spawn_link(Read),
  receive
    {'EXIT', Reader, Ret} ->
      Ret
  end.

generate_banner() ->
  [io_lib:format(
     ?GRN(" ___________          .__                         \n") ++
     ?GRN(" \\__    ___/__________|  | _____    ____    ____ \n") ++
     ?GRN("   |    |_/ __ \\_  __ \\  | \\__  \\  /    \\  / ___\\") ++ "      |  Type-safe Erlang! \n" ++
     ?GRN("   |    |\\  ___/|  | \\/  |__/ __ \\|   |  \\/ /_/  >") ++ "     |  Source: " ++ ?BLU("https://github.com/amir343/terlang") ++ "\n" ++
     ?GRN("   |____| \\___  >__|  |____(____  /___|  /\\___  / \n") ++
     ?GRN("              \\/                \\/     \\//_____/  ") ++ "\n\n"
    ,[])].

report_error({error, C}) ->
  io:format("~p~n~n", [C]);
report_error(Error) ->
  io:format("Error: ~p~n~n", [Error]).

read_one_expression(Prompt) ->
  case read_one_expression(Prompt, "") of
    {ok, Line} ->
      {ok, Tokens, _} = terl_scan:string(Line),
      terl_parse:parse_exprs(Tokens);
    {error, _} = Error ->
      Error
  end.

read_one_expression(Prompt, Result) ->
  Line = read_line(Prompt),
  case Line of
    {error, _} ->
      Line;
    _ ->
      case lists:reverse(Line) of
        [$\n, $. | _] ->
          {ok, Result ++ Line};
        _ ->
          read_one_expression(Prompt, Result ++ Line)
      end
  end.

read_line(Prompt) ->
  case io:get_line(standard_io, Prompt) of
    {error, Error} ->
      {error, {io, Error}};
    Line -> Line
  end.

evaluate(Exprs, State=#state{bs = Bs, eval = Pid, cnt = Cnt}) ->
  NPid =
    case process_info(Pid) =/= undefined of
      true  -> Pid;
      false -> spawn(fun() -> evaluator_loop() end)
    end,
  NPid ! {eval, Exprs, Bs, self()},
  receive
    {value, {type, V, T}, NewBs} ->
      io:format("res~p: ~s :: ~s~n~n", [Cnt, type_err_msg:p_expr(V), type_err_msg:p_type(T)]),
      State#state{bs = NewBs, eval = NPid, cnt = Cnt + 1};
    {value, V, NewBs} ->
      io:format("res~p: ~p~n~n", [Cnt, V]),
      State#state{bs = NewBs, eval = NPid, cnt = Cnt + 1};
    {error, Error} ->
      report_error(Error),
      State#state{eval = NPid, cnt = Cnt + 1}
  end.

evaluator_loop() ->
  receive
    {eval, Exprs, Bs, From} ->
      From ! evaluate_exprs(Exprs, Bs),
      evaluator_loop();
    _ ->
      evaluator_loop()
    end.

evaluate_exprs(Exprs0, Bs) ->
  try
    case is_forget_binding(Exprs0) of
      {true, Binding} ->
        {value, ok, terl_eval:del_binding(Binding, Bs)};
      false ->
        Exprs = lists:map(fun map_shell_commands/1, Exprs0),
        terl_eval:exprs(Exprs, Bs, none, none)
    end
  catch
    C:E ->
      {error, {C, E}}
  end.

is_forget_binding([{call, _, {atom, _, f}, [{var, _, Binding}]}]) ->
  {true, Binding};
is_forget_binding(_) ->
  false.

shell_commands() ->
  [c, clear, exit, help, h, q].

map_shell_commands({call, L1, {atom, L2, Cmd}, Args} = Abs) ->
  case lists:member(Cmd, shell_commands()) of
    true ->
      {call, L1, {remote, L2, {atom, L2, ?MODULE}, {atom, L2, Cmd}}, Args};
    false ->
      Abs
  end;
map_shell_commands(Abs) ->
  Abs.

%%_-----------------------------------------------------------------------------
%% Shell commads:
%%_-----------------------------------------------------------------------------

c(File) ->
  c(File, []).

c(Files, Opts) when is_list(Files) ->
  compile_result(terl_compile:files(Files, Opts), Opts);
c(File, Opts) ->
  compile_result(terl_compile:file(File, Opts), Opts).

clear() ->
  io:format("\e[H\e[J").

h() -> help().

help() ->
  io:put_chars(<<"\nTerl shell built-in functions\n\n"
                 "c(file)        -- compile and load code in <file>\n"
                 "clear()        -- clear REPL output\n"
                 "f(Binding)     -- forget a binding\n"
                 "h()            -- an alias for help command\n"
                 "help()         -- print this help info\n"
                 "q()            -- quit the shell\n"
                 "exit()         -- an alias for q()\n"
                 "\n\n"
               >>).

q() ->
  c:q().

exit() ->
  c:q().

%%_-----------------------------------------------------------------------------
%% Helper functions
%%_-----------------------------------------------------------------------------

compile_result(Res, Opts) ->
  case Res of
    {ok, Mods} ->
      OutDir = out_dir(Opts),
      load_modules(Mods, OutDir);
    _ ->
      ok
  end,
  Res.

load_modules(Mods, OutDir) when is_list(Mods) ->
  lists:foreach(fun(M) ->
                    lm(M, OutDir)
                end, Mods);
load_modules(Mod, OutDir) ->
  lm(Mod, OutDir).

lm(Mod, OutDir) ->
  ModFile = filename:join(OutDir, atom_to_list(Mod)),
  code:purge(Mod),
  code:load_abs(ModFile, Mod).

out_dir(Opts) ->
  proplists:get_value(outdir, Opts, ".").
