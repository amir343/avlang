-module(type_check_compiler_opts).

-export([ options_of_interest/1
        , dump_local_scopes/1
        ]).


%% Causes the type_checker module to output debug info
%% about scopes and types
-define(TYPE_DEBUG, type_debug).

%% Causes the type checker to dump info about all local
%% scopes in a module
-define(DUMP_LOCAL_SCOPES, dump_local_scopes).

-define(OPTIONS, [ ?TYPE_DEBUG
                 , ?DUMP_LOCAL_SCOPES]).


options_of_interest(Opts0) ->
  [Opt || Opt <- Opts0, lists:member(Opt, ?OPTIONS)].


dump_local_scopes(Opts0) ->
  lists:member(?DUMP_LOCAL_SCOPES, Opts0).
