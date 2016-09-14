-module(terl_binary).

-export([ op/1
        , op/2
        , lub/1
        , type_specifier_list/1
        ]).

-include("type_macros.hrl").

op(_, _) -> ?INVALID.

op(_)    -> ?INVALID.

%%-- least common supertype ----------------------

lub(?BINARY)         -> ?BINARY;
lub(_)               -> ?ANY.


type_specifier_list(default) ->
  [?INTEGER];
type_specifier_list(TSLs) ->
  Types = [tsl(T) || T <- TSLs],
  [T || T <- Types, T =/= nil].

tsl(binary)    -> ?BINARY;
tsl(integer)   -> ?INTEGER;
tsl(float)     -> ?FLOAT;
tsl(bytes)     -> ?BINARY;
tsl(bitstring) -> ?BINARY;
tsl(bits)      -> ?BINARY;
tsl(utf8)      -> ?INTEGER;
tsl(utf16)     -> ?INTEGER;
tsl(utf32)     -> ?INTEGER;
tsl(_)         -> nil.
