-define(ANY,       {terl_type, 'Any'}).
-define(BOOLEAN,   {terl_type, 'Boolean'}).
-define(BINARY,    {terl_type, 'Binary'}).
-define(FLOAT,     {terl_type, 'Float'}).
-define(INTEGER,   {terl_type, 'Integer'}).
-define(PID,       {terl_type, 'Pid'}).
-define(STRING,    {terl_type, 'String'}).
-define(INVALID,   type_internal:invalid_operator()).
-define(UNDEFINED, undefined).


-define(ANY_MOD,     terl_any).
-define(BOOLEAN_MOD, terl_boolean).
-define(BINARY_MOD,  terl_binary).
-define(FLOAT_MOD,   terl_float).
-define(INTEGER_MOD, terl_integer).
-define(LIST_MOD,    terl_list).
-define(PID_MOD,     terl_pid).
-define(STRING_MOD,  terl_string).
-define(UNION_MOD,   terl_union).
