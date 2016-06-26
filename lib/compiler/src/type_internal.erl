-module(type_internal).

-export([ type_tag/1
        , built_in/1
        ]).

type_tag(Type) ->
  case built_in(Type) of
    true -> {terl_type, Type};
    false -> {terl_user_defined, Type}
  end.

built_in(any) -> true;
built_in(none) -> true;
built_in(atom) -> true;
built_in(integer) -> true;
built_in(pid) -> true;
built_in(reference) -> true;
built_in(binary) -> true;
built_in(string) -> true;
built_in(boolean) -> true;
built_in(float) -> true;

built_in('Any') -> true;
built_in('None') -> true;
built_in('Atom') -> true;
built_in('Integer') -> true;
built_in('Pid') -> true;
built_in('Reference') -> true;
built_in('Binary') -> true;
built_in('String') -> true;
built_in('Boolean') -> true;
built_in('Float') -> true;


built_in(_) -> false.