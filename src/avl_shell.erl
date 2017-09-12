-module(avl_shell).

-export([ start/0 ]).

%% Shell commands
-export([ c/1
        , c/2
        , clear/0
        , exit/0
        , h/0
        , help/0
        , i/0
        , l/1
        , q/0
        ]).

%% Colours for the shell
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").
-define(PUR(Str), "\e[0;95m" ++ Str ++ "\e[0m").

-record(state, { eval
               , bs      = []
               , type_bs = []
               , cnt     = 0
               }).

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
        , bs = avl_eval:new_bindings()}.

server_loop(State) ->
  Prompt = prompt(),
  Ret = read_expression(Prompt),
  State1 =
    case Ret of
      {error, _} = E->
        report_error(E),
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
    _          -> ["erm> "]
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

report_error({error, C}) ->
  io:format("~p~n~n", [C]);
report_error(Error) ->
  io:format("Error: ~p~n~n", [Error]).

read_one_expression(Prompt) ->
  case read_one_expression(Prompt, "") of
    {ok, Line} ->
      case string_to_abs_form(Line) of
        {ok, _} = E             -> E;
        {error, {_, _, Reason}} -> {error, lists:flatten(Reason)}
      end;
    {error, _} = Error ->
      Error
  end.

string_to_abs_form(Str) ->
  {ok, Tokens, _} = avl_scan:string(Str),
  avl_parse:parse_exprs(Tokens).

read_one_expression(Prompt, Result) ->
  Line = read_line(Prompt),
  case Line of
    {error, _} -> Line;
    _ ->
      case lists:reverse(Line) of
        [$\n, $. | _] -> {ok, Result ++ Line};
        _             -> read_one_expression(Prompt, Result ++ Line)
      end
  end.

read_line(Prompt) ->
  case io:get_line(standard_io, Prompt) of
    {error, Error} -> {error, {io, Error}};
    Line           -> Line
  end.

evaluate(Exprs, State=#state{bs = Bs, type_bs = TBs, eval = Pid, cnt = Cnt}) ->
  NPid =
    case process_info(Pid) =/= undefined of
      true  -> Pid;
      false -> spawn(fun() -> evaluator_loop() end)
    end,
  NPid ! {eval, Exprs, Bs, TBs, self()},
  receive
    {value, {type, V, T}, _, NewBs, NewTBs, _} ->
      io:format("res~p: ~s :: ~s~n~n",
                [Cnt, type_err_msg:p_expr(V), type_err_msg:pp_type(T)]),
      State#state{bs = NewBs, type_bs = NewTBs, eval = NPid, cnt = Cnt + 1};
    {value, V, T, NewBs, NewTBs, Errors} ->
      io:format("res~p :: ~s = ~p~n~n", [Cnt, type_err_msg:pp_type(T), V]),
      [type_err_msg:format_error(E) || E <- Errors],
      State#state{bs = NewBs, type_bs = NewTBs, eval = NPid, cnt = Cnt + 1};
    {error, Error} ->
      report_error(Error),
      State#state{eval = NPid, cnt = Cnt + 1}
  end.

evaluator_loop() ->
  receive
    {eval, Exprs, Bs, TBs, From} ->
      From ! evaluate_exprs(Exprs, Bs, TBs),
      evaluator_loop();
    _ ->
      evaluator_loop()
    end.

evaluate_exprs(Exprs0, Bs, TBs) ->
  try
    case is_forget_binding(Exprs0) of
      {true, Binding} ->
        {value, ok, {avl_atom_type, ok}, avl_eval:del_binding(Binding, Bs),
         lists:keydelete(Binding, 1, TBs), []};
      false ->
        Exprs = lists:map(fun map_shell_commands/1, Exprs0),
        {value, V, NBs} = avl_eval:exprs(Exprs, Bs, none, none),
        VStr = term_to_string(V),
        {Type, NTBs, Error} = type_check:exprs(Exprs, TBs),
        %% Skip type intersection of the returned value is a function.
        FinalType =
          case re:run(VStr, "#Fun<.*>$") of
            {match, _} -> Type;
            _ ->
              {ok, ValueAbs} = string_to_abs_form(VStr ++ "."),
              {ValueType, _, _} = type_check:exprs(ValueAbs),
              type_internal:type_intersection(ValueType, Type)
          end,
        {value, V, FinalType, NBs, NTBs, Error}
    end
  catch
    C:E -> {error, {C, E}}
  end.

term_to_string(Term) ->
  lists:flatten(io_lib:format("~p", [Term])).

is_forget_binding([{call, _, {atom, _, f}, [{var, _, Binding}]}]) ->
  {true, Binding};
is_forget_binding(_) ->
  false.

shell_commands() ->
  [c, clear, exit, help, h, i, l, q].

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
  compile_result(avl_compile:files(Files, Opts), Opts);
c(File, Opts) ->
  compile_result(avl_compile:file(File, Opts), Opts).

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
                 "i()            -- print process list\n"
                 "l(Mod)         -- load or reload a module\n"
                 "q()            -- quit the shell\n"
                 "exit()         -- an alias for q()\n"
                 "\n\n"
               >>).

i() ->
  shell_default:i().

l(Mod) ->
  c:l(Mod).

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

%%_-----------------------------------------------------------------------------

generate_banner() ->
  [io_lib:format(
     ?GRN("   _____          .__                            \n") ++
     ?GRN("  /  _  \\  ___  __|  |  _____     ____     ____  \n") ++
     ?GRN(" /  /_\\  \\ \\  \\/ /|  |  \\__  \\   /    \\   / ___\\ ") ++ "      |  Type-safe Erlang! \n" ++
     ?GRN("/    |    \\ \\   / |  |__ / __ \\_|   |  \\ / /_/  >") ++ "      |  Source: " ++ ?BLU("https://github.com/amir343/eramlang") ++ "\n" ++
     ?GRN("\\____|__  /  \\_/  |____/(____  /|___|  / \\___  / ") ++  "      |\n" ++
     ?GRN("        \\/                   \\/      \\/ /_____/  ") ++ "      |  v0.1" ++ "\n\n"
                , [])].
