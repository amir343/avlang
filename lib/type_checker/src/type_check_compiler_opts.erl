-module(type_check_compiler_opts).

-export([options_of_interest/1]).


%% Causes the type_checker module to output debug info
%% about scopes and types
-define(TYPE_DEBUG, type_debug).


-define(OPTIONS, [?TYPE_DEBUG]).


options_of_interest(Opts0) ->
  [Opt || Opt <- Opts0, lists:member(Opt, ?OPTIONS)].
