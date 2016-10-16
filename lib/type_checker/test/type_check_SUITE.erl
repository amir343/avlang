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
  AbsForms = abstract_forms_for_module(FileName),
  {ok, [{[], [], nil}]} = type_check:module([{FileName, AbsForms, nil}], []).

test_binary() ->
  FileName = "binary_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, [{[], [], nil}]} = type_check:module([{FileName, AbsForms, nil}], []).

test_misc() ->
  FileName = "misc_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, [{[], [], nil}]} = type_check:module([{FileName, AbsForms, nil}], []).

test_type_syntax() ->
  FileName = "type_syntax_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, [{[], [], nil}]} = type_check:module([{FileName, AbsForms, nil}], []).

test_function_pointer() ->
  FileName = "function_pointer_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, [{[], [], nil}]} = type_check:module([{FileName, AbsForms, nil}], []).

test_generic_types() ->
  FileName = "generic_types.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, [{[], [], nil}]} = type_check:module([{FileName, AbsForms, nil}], []).

test_multiple_input() ->
  FileName1 = "function_pointer_test.erl",
  FileName2 = "misc_test.erl",
  AbsForms1 = abstract_forms_for_module(FileName1),
  AbsForms2 = abstract_forms_for_module(FileName2),
  {ok, [{[], [], nil}, {[], [], nil}]} =
    type_check:module([ {FileName1, AbsForms1, nil}
                      , {FileName2, AbsForms2, nil}], []).

test_remote_call() ->
  FileName1 = "remote_call_1.erl",
  FileName2 = "remote_call_2.erl",
  AbsForms1 = abstract_forms_for_module(FileName1),
  AbsForms2 = abstract_forms_for_module(FileName2),
  {ok, [{[], [], nil}, {[], [], nil}]} =
    type_check:module([ {FileName1, AbsForms1, nil}
                      , {FileName2, AbsForms2, nil}], []).

abstract_forms_for_module(FileName) ->
  TestDir = code:lib_dir(type_checker, test),
  DataDir = filename:join(TestDir, "type_check_SUITE_data"),
  Mod = filename:join(DataDir, FileName),
  {ok, AbsForm} = epp:parse_file(Mod, [], []),
  AbsForm.

