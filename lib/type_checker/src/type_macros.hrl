-define(INTEGER, {terl_type, 'Integer'}).
-define(FLOAT, {terl_type, 'Float'}).
-define(BOOLEAN, {terl_type, 'Boolean'}).
-define(STRING, {terl_type, 'String'}).
-define(ANY, {terl_type, 'Any'}).
-define(INVALID, type_internal:invalid_operator()).
-define(UNDEFINED, undefined).



-define(ANY_MOD,     terl_any).
-define(INTEGER_MOD, terl_integer).
-define(FLOAT_MOD,   terl_float).
-define(BOOLEAN_MOD, terl_boolean).
-define(STRING_MOD,  terl_string).
-define(LIST_MOD,    terl_list).
-define(UNION_MOD,   terl_union).
