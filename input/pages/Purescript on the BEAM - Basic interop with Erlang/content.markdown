Previous entries..

- [Intro](/entries/functional-erlang---purescript-on-the-beam---intro.html)
- [Getting Started](/entries/purescript-on-the-beam:-getting-started.html)
- [Writing some basic code](/entries/purescript-on-the-beam---writing-some-basic-code.html)

Super, now it's all very well and good being able to write code Purescript that runs on the BEAM but unless we want to do absolutely everything from scratch and ignore everything that OTP has to give us (more on that later) we'll need to be able to invoke Purescript from Erlang and we'll need to be able to invoke Erlang from Purescript.

We'll cover both of these in this short post as it's not complicated but there are some nuances that we need to be aware of.

# Calling Erlang from Purescript

This is probably the easiest direction to go in, we know the shape of Erlang and it's relatively simple as everything is an MFA (Module, Function, Arguments). 

*cool_native_module.erl*

```erlang

    -module(cool_native_module).

    -export([ call_me_an_ambulance/1 ]).

    call_me_an_ambulance(Name) ->
      << "Hey ", Name/binary, " - you're an ambulance" >>.

```

Assuming we have a native module in a library somewhere we want to call (like the above), we'll need to take some steps to get this callable from anywhere in our Purescript code.

*CoolNativeModule.purs*

```haskell

    module CoolNativeModule where

    foreign import callMeAnAmbulance :: String -> String

```
    
*CoolNativeModule.erl*

```erlang

    -module(coolNativeModule@foreign).

    -export([ callMeAnAmbulance/1 ]).

    callMeAnAmbulance(Name) -> cool_native_module:call_me_an_ambulance(Name).

```

Essentially, you end up creating wrappers around existing modules in this manner with both a Purescript module for use from Purescript and a backing "FFI" which then talks to the original Erlang module and in theory can ship them as Purescript modules for download in parallel with their Erlang modules.

It's not that simple though, as very often our external functions are going to have side effects and in Purescript, these are supposed to be modelled *as* an Effect, consider the following.

*cool_native_module.erl*

```erlang
    
    -module(cool_native_module).

    -export([ call_me_an_ambulance/1 ]).

    call_me_an_ambulance(Name) ->
      io:format("You're definitely an ambulance"),
      << "Hey ", Name/binary, " - you're an ambulance" >>.


```

In this case, we're just echoing something to stdout, but it could equally be something over the network/disk etc. Modelling this a *String -> String* is dishonest because in reality it's now a *String -> Effect String*

*CoolNativeModule.purs*

```haskell

    module CoolNativeModule where

    foreign import callMeAnAmbulance :: String -> Effect String

```

How is this modelled in the FFI? 

```erlang

    -module(coolNativeModule@foreign).

    -export([ callMeAnAmbulance/1 ]).

    callMeAnAmbulance(Name) -> 
       fun() -> 
         cool_native_module:call_me_an_ambulance(Name)
       end.

```

It's just monads innit (DW, an Effect is just a function that has a side effect, and until that happens you're effectively just passing around functions.

Now - there are no guarantees when you call (most) Erlang that there are no side effects though, you can in (pretty much any) function open a file and write whatever you want to it and in theory every single function call to Erlang would probably be of type *Effect a* - this would be cumbersome though and so far in the modules I've read there seems to be a pragmatic approach to this. Is there a side effect? It's an Effect, is there not? No Effect - there is nothing stopping you ignoring the types entirely and doing all sorts of nastiness in your FFI but on your head be it.

You can also export types from these modules, and they'll be opaque as far as the Purescript is concerned but at least type safe while you're still *in* Purescript, consider

```erlang

    -module(coolNativeModule@foreign).

    -export([ callMeAnAmbulance/1 ]).

    -record(some_record, {
        foo :: string()
      }).

    callMeAnAmbulance(Name) -> 
      #some_record { foo = Name }.

```

We *could* model that record as a *Tuple2 Atom String*

*CoolNativeModule.purs*

```haskell

    module CoolNativeModule where

    foreign import callMeAnAmbulance :: String -> Tuple2 Atom String

```

But this is going to be brittle, as changes to the record's structure aren't going to show up at compilation.

We could equally pass in a constructor function to the Erlang that given all the arguments creates a record that's usable in Purescript but we could equally just accept that it's an opaque object that's only usable from the Erlang that owns it (this is 90% of most Erlang anyway). Functions can then be provided via the FFI to operate over that opaque structure.

*CoolNativeModule.purs*

```haskell

    module CoolNativeModule where

    foreign import data SomeRecord :: Type

    foreign import callMeAnAmbulance :: String -> SomeRecord

```

That pretty much covers much of what we need when importing libraries that are already written in Erlang, so next we'll look at how we can call Purescript *from* Erlang (Which is something that we need more than you'd think at first glance).
