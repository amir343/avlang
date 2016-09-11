-module(type_check_SUITE).


-export([ all/0
        , test_binary/0
        , test_misc/0
        , test_record/0
        , test_type_syntax/0
       ]).

all() ->
  [ test_binary
  , test_misc
  , test_record
  , test_type_syntax
  ].


test_record() ->
  FileName = "record_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, []} = type_check:module(AbsForms, FileName, []).

test_binary() ->
  FileName = "binary_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, []} = type_check:module(AbsForms, FileName, []).

test_misc() ->
  FileName = "misc_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, []} = type_check:module(AbsForms, FileName, []).

test_type_syntax() ->
  FileName = "type_syntax_test.erl",
  AbsForms = abstract_forms_for_module(FileName),
  {ok, []} = type_check:module(AbsForms, FileName, []).

abstract_forms_for_module(FileName) ->
  TestDir = code:lib_dir(type_checker, test),
  DataDir = filename:join(TestDir, "type_check_SUITE_data"),
  Mod = filename:join(DataDir, FileName),
  {ok, AbsForm} = epp:parse_file(Mod, [], []),
  AbsForm.

