# Avlang

## Short history

Coming from a staticly-typed world I recognised and sensed the need for types in Erlang language quiet a lot. At first I was mostly interested in augmenting the Erlang itself with typing which was not possible without changing the syntax. 

So I forked Erlang compiler to start supporting types and in order to do that I started adding rules to Erlang grammars. The plan has been to not touch the Erlang base syntax and only add as much as needed to support type declaration. I think this goal has already been reached. A lot of ideas have been taken from Dialyzer syntax and I tried to keep it simple and clean as possible.

Please see the below FAQ section for more detailed discussions around these decisions.

### Show me the codez

```erlang
module_to_action :: (String) -> Char.
module_to_action(Str) ->
  F :: (Char -> Boolean) = fun (C) -> C =/= $_ end,
  [A | _] = lists:dropwhile(F, Str),
  A.
```

## FAQ

#### Why Dialyzer is not sufficient?

Actually Dialyzer is sufficient enough to provide a good basis for your Erlang programs safety. I see two issues with Dialyzer though: 

1. I don't like optional typing. It's either that the compiler enforces the type checking and stops you from proceeding or it doesn't do it at all.
2. Dialyzer lacks expression type declaration which is in many cases really helpful for the compiler/developer to express the intended type for an expression

#### Is ERM type system sound and decidable?

Not really! There is no formal proof and there are lots of things left to complete the type system implementation.

#### What's the plan for this work?

This project has been my little research thingy that's getting bigger now. I didn't plan to open-source it initially but I decided to do so despite the fact the implementation is not complete.

#### What's left to do?

A lot actually. To name a few:

* Session typing for Actors
* Recursive types
* The implementation of type check/inference can be more efficient


#### Any reasoning behind the name Avlang?

Avlang stands for [Avicenna](https://en.wikipedia.org/wiki/Avicenna) Lang, a persian mathematician and you can read about him by following the provided link.