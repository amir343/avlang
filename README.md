# Avlang

Avlang is a statically-typed language based on Erlang. It does compile-time type-checking and rejects programs that break the type safety. 

If you know Erlang already it requires you almost zero time learning Avlang as the syntax is exactly the same except minor additions for type declaration. 

### Show me an example:

```erlang
module_to_action :: (String) -> Char.
module_to_action(Str) ->
  F :: (Char -> Boolean) = fun (C) -> C =/= $_ end,
  [A | _] = lists:dropwhile(F, Str),
  A.
```

## How to build

First clone the repo:

```
git clone git@github.com:amir343/avlang.git
```

and build:

```
cd avlang
make all
```

This requires that you have Erlang OTP 17 installed in your machine. Alternatively you can build with vanila OTP Docker image:

```
./docker-make.sh all
```

You can pass any `make` arguments to this script.

## Getting started

Avlang comes with a REPL:

```
./avl shell
```

You can play around and test different stuffs in the shell. 

Source code goes into `.avl` files. For instance assume that you want to define your own `map` function. We will create a file called `my_list.avl` with this content:

```erlang
-module(my_list).

-export([my_map/2]).

my_map :: ([], Any) -> [];
          ([A], (A -> B)) -> [B].
my_map([], _F) ->
  [];
my_map([H|T], F) ->
  [F(H) | my_map(T, F)].
```

In the shell you can compile the code like this:

```
avl> avl_compile:file(my_list).
res0 :: {ok, my_list} = {ok,my_list}
```

and then for example:

```
avl> my_list:my_map([1,2,3,4], fun(I) -> integer_to_list(I) end).
res1 :: [[Char]] = ["1","2","3","4"]
```

You can alternatively use `bin/avlc` to compile your source code.

## Contributions

If you want to help me out on this project you can start by finding/fixing bugs and/or adding missing parts. You are more than welcome!

## Rationale

Erlang is a great simple language that you can do powerful things with it. One of the most important thing that has been missing in Erlang to this date is lack of proper type checking. There were some attempts to introduce this into the language but Erlang never got its type-checking for many reasons.

Avlang born with the intention to test how easy it is to extend Erlang with type checking. The idea was that developers should define the bare minimum amount of types and the compiler should do the rest and infer missing type declarations. 

#### Why Dialyzer is not sufficient?

Actually Dialyzer is sufficient enough to provide a good basis for your Erlang programs safety. I see two issues with Dialyzer though:

1. I don't like optional typing. It's either that the compiler enforces the type checking and stops you from proceeding or it doesn't do it at all.
2. Dialyzer lacks expression type declaration which is in many cases really helpful for the compiler/developer to express the intended type for an expression.

#### Is Avlang type system sound and decidable?

Not really! There is no formal proof and there are lots of things left to complete the type system implementation.

#### What's the plan for this work?

This project has been my little research thingy that's getting bigger now. I didn't plan to open-source it initially but I decided to do so despite the fact the implementation is not complete and clean. 

#### What's left to do?

A lot actually. To name just a few:

* Recursive types
* Session typing for Actors
* The implementation of type check/inference can be more efficient


#### Any reasoning behind the name Avlang?

Avlang stands for [Avicenna](https://en.wikipedia.org/wiki/Avicenna) Lang, a persian mathematician and you can read about him by following the provided link.
