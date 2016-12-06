-module(type_check_SUITE).


-export([ all/0
        , test_binary/0
        , test_function_pointer/0
        , test_generic_types/0
        , test_misc/0
        , test_multiple_input/0
        , test_record/0
        , test_remote_call/0
        , test_type_syntax/0
       ]).

-include("../../compiler/include/terl_compiler.hrl").

all() ->
  [ test_binary
  , test_function_pointer
  , test_generic_types
  , test_misc
  , test_multiple_input
  , test_record
  , test_remote_call
  , test_type_syntax
  ].


test_record() ->
  FileName = "record_test.erl",
  CompRec = build_compile_rec(FileName),
  {ok, [{[], [], _}]} = type_check:module(CompRec).

test_binary() ->
  FileName = "binary_test.erl",
  CompRec = build_compile_rec(FileName),
  {ok, [{[], [], _}]} = type_check:module(CompRec).

test_misc() ->
  FileName = "misc_test.erl",
  CompRec = build_compile_rec(FileName),
  {ok, [{[], [], _}]} = type_check:module(CompRec).

test_type_syntax() ->
  FileName = "type_syntax_test.erl",
  CompRec = build_compile_rec(FileName),
  {ok, [{[], [], _}]} = type_check:module(CompRec).

test_function_pointer() ->
  FileName = "function_pointer_test.erl",
  CompRec = build_compile_rec(FileName),
  {ok, [{[], [], _}]} = type_check:module(CompRec).

test_generic_types() ->
  FileName = "generic_types.erl",
  CompRec = build_compile_rec(FileName),
  {ok, [{[], [], _}]} =
    print_result(type_check:module(CompRec)).

test_multiple_input() ->
  FileName1 = "function_pointer_test.erl",
  FileName2 = "misc_test.erl",
  CompRec1 = build_compile_rec(FileName1),
  CompRec2 = build_compile_rec(FileName2),
  {ok, [{[], [], _}, {[], [], _}]} =
    type_check:modules([CompRec1, CompRec2]).

test_remote_call() ->
  FileName1 = "remote_call_1.erl",
  FileName2 = "remote_call_2.erl",
  CompRec1 = build_compile_rec(FileName1),
  CompRec2 = build_compile_rec(FileName2),
  {ok, [{[], [], _}, {[], [], _}]} =
    type_check:modules([CompRec1, CompRec2]).

build_compile_rec(FileName) ->
  #compile{ifile = FileName, code = abstract_forms_for_module(FileName)}.

abstract_forms_for_module(FileName) ->
  TestDir = code:lib_dir(type_checker, test),
  DataDir = filename:join(TestDir, "type_check_SUITE_data"),
  Mod = filename:join(DataDir, FileName),
  {ok, AbsForm} = terl_epp:parse_file(Mod, [], []),
  AbsForm.

print_result({ok, [{[], [], _}]} = Res) ->
  Res;
print_result({error, ProblematicFiles} = Res) ->
  io:format("~n", []),
  lists:foreach(
    fun({_Ws, Errs, _}) ->
        lists:foreach(
          fun({FN, Msgs}) ->
              [io:format("\t~s",
                         [type_err_msg:internal_format_error(FN, L, Msg)])
               || {L, _, Msg} <- Msgs]
          end, Errs)
    end, ProblematicFiles),
  Res.
