-module(avl_init).

-export([ start/0 ]).

start() ->
  user_drv:start(['tty_sl -c -e', {avl_shell, start, []}]).
