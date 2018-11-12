Previous entries..

- [Intro](/entries/functional-erlang---purescript-on-the-beam---intro.html)
- [Getting Started](/entries/purescript-on-the-beam:-getting-started.html)
- [Writing some basic code](/entries/purescript-on-the-beam---writing-some-basic-code.html)
- [Basic interop with Erlang](/entries/purescript-on-the-beam---basic-interop-with-erlang.html)

Why would we want to call Purescript *from* Erlang? Given that moving *from* Erlang *to* Purescript as our day to day language is one of our stated goals? Well the answer is in the whole "Module Function Arguments" thing, a lot of APIs in Erlang make you provide an MFA for callback with your state/the library state for performing operations. This is true for all of the web frameworks certainly, and most of OTP (Again, more on that later..)

# Calling Purescript from Erlang

Consider the following contrived example first.. 

*my_erlang.erl*

```erlang

    -module(my_erlang).

    -export([ init/0, callback/1 ]).

    init() ->
      ok = some_library:configure(my_erlang, callback).

    callback(SomeLibraryState) ->
      NewLibraryState = some_library:do_something(SomeLibraryState),
      { ok, NewLibraryState }.

```

This is a very familiar pattern for anybody used to writing Erlang, and it isn't immediately obvious how we're going to give that library anything from Purescript - most APIs in a functional language if they want a callback, will just ask for a function to callback. This isn't typically the case in Erlang because this would get in the way of the (somewhat important) code hotloading which is used during development for a rapid experience, and in production to upgrade live systems without incurring downtime.

Ignoring further steps that we might take to then prettify the interaction with Erlang/Purescript here, it turns out that Purescript (currently) compiles modules/functions to fairly predictable names which we can use in our FFI.

So first up, we'd need to write the code to talk from Purescript to Erlang as covered in the previous post (Assuming that both configure/do_something are impure and require effects).

*SomeLibrary.erl*

```erlang

    -module(someLibrary@foreign).

    -export([configure/2, doSomething/1]).

    configure(Module, Function) ->
      fun() -> some_library(Module, Function) end.

    doSomething(OpaqueState) ->
      fun() -> some_library:do_something(OpaqueState) end.

```

*SomeLibrary.purs*

```haskell

    module SomeLibrary where

    foreign import data LibraryState :: Type

    foreign import configure :: Atom -> Atom -> Effect Atom
    foreign import doSomething :: OpaqueState -> Effect OpaqueState

```

Using this FFI we can now re-write our original Erlang in Purescript, or at least have a go at it

```haskell

    module MyCode where

    init :: Effect Atom
    init = SomeLibrary.configure (atom "myCode@ps") (atom "callback")

    callback :: OpaqueState -> Tuple2 Atom OpaqueState
    callback state = tuple2 $ (atom "ok") state -- don't actually do anything yet
      
```

A problem immediately presents itself, we can't use the doSomething function from our callback because it's an Effectful function and our callback function is not, this means callback is pure and can't actually have any side effects.

We can try to re-write callback so it is Effectful

```haskell

    module MyCode where

    init :: Effect Atom
    init = SomeLibrary.configure (atom "myCode@ps") (atom "callback")

    callback :: OpaqueState -> Effect (Tuple2 Atom OpaqueState)
    callback state = do
      newState <- SomeLibrary.state
      pure $ tuple2 $ (atom "ok") newState 
      
```

But now we'll just get a runtime error because the native Erlang code expects a function of *(LibraryState -> { ok, Library State })*, and we've now giving it a function that is *LibraryState -> (() -> { ok, LibraryState})* - essentially passing a function reference back to Erlang instead of the result of that function (ew).

We can dance around this in a number of ways, the simplest being to lie about the effect and use unsafePerformEffect in our "pure" function (No thanks).

We can also use the namespace Effect.Uncurried to return an effectively negative arity effect back to Erlang (IE, the direct result) without writing any code that lies about what it actually is.

```haskell

    module MyCode where

    init :: Effect Atom
    init = SomeLibrary.configure (atom "myCode@ps") (atom "callback")

    callback :: EffectFn1 OpaqueState (Tuple2 Atom OpaqueState)
    callback = mkEffectFn1 \state -> do
        newState <- SomeLibrary.state
        pure $ tuple2 $ (atom "ok") newState 
      
``` 

(IE, the direct result) without writing any code that lies about what it actually is.

This is typically how we will interact directly with Erlang libraries and will get us pretty far when creating low level bindings with those libraries, but we will see as we progress into creating our wrappers around OTP we'll uncover some better patterns for making this less brittle (Passing atoms all over the show is pretty dull). For one-off cases the above is perfectly fine, but in reality we'll only be using this mechanism when writing the wrappers themselves and presenting higher level APIs to the application itself.


