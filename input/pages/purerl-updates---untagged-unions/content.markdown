Yes, another blog post about Purerl, following on from

- [Purerl Updates - Processes and Self](/entries/purerl-updates---processes-and-self.html)
- [Purerl Updates - Subscriptions](/entries/purerl-updates---subscriptions.html)

We are now here to talk about untagged unions and how they can be used when dealing with legacy messages being sent to our process and why we might want to do something differently at all.

Consider a dependency to a legacy Erlang library that has a subscribe call that sends us messages


```erlang
legacy_api:subscribe(),

receive 
  { data, Bin } -> ..
  { err, Reason } -> ..
  eof -> ..

```

It isn't possible to subcribe to this in Purescript without doing *something* to unpack those Erlang types. We could have

```haskell
workerLoop :: ProcessM Foreign Unit
workerLoop = do
  msg <- receive
  case (LegacyApi.unpack msg) of 
    Err err -> ...
    Data bin -> ...
    Fin -> ...

startWorker :: ProcessM Foreign Unit
startWorker = do
  LegacyApi.subscribe
  workerLoop

```

Simply leaving it to the caller to know that the Foreign needs running through the API again in order to decipher it.

We could also use the MessageRouter in Pinto to spin up another process that knows how to do this for us so this translation is already done once we're in user code

```haskell
import Pinto.MessageRouter as MR

workerLoop :: ProcessM LegacyApi.Msg Unit
workerLoop = do
  msg <- receive
  case msg of 
    Err err -> ...
    Data bin -> ...
    Fin -> ...

startWorker :: ProcessM LegacyApi.Msg Unit
startWorker = do
  me <- self
  MR.startRouter LegacyApi.subscribe LegacyApi.unsubscribe (self self <<< LegacyApi.unpack)
  workerLoop
```

This is a viable solution for *most* scenarios because it is easy to understand, reasonably lightweight, allows us to accept more than one kind of message, etc. It incur the "cost" of spinning up another process, and it does incur the cost of doubling the number of messages being sent for a single interaction.

Usually this isn't a problem (or more - if doubling the messages or processes is going to cause you issues, then it's possible that the codebase already has issues anyway!). 

Having said all of that, for FFI around existing Erlang where there are there are numerous variants, writing mapping code in Erlang could be rather error prone (and passing in constructors from Purescript gets tiresome after half a dozen) - it starts to make sense to instead try to describe the data types "in place" in Purescript using something formal.

For this, there is the package [purescript-untagged-union](https://github.com/id3as/purescript-erl-untagged-union).

Now, two of these values are directly representable in Purescript because of the way in which types are represented in Erlang under the hood.

```haskell
data LegacyMsg 
  = Err Binary
  | Data Binary
```

We can let the untagged unions library know that underlying data maps onto this with an instance of RuntimeType that describes it


```haskell
instance legacyMsg_runtimeType ::
  RuntimeType
    LegacyMsg
    (RTOption (RTTuple2 (RTLiteralAtom "err") RTBinary)
               RTTuple2 (RTLiteralAtom "data" RTBinary))
```

Come to think of it, that 'err' is horrible, we're in Purescript and there is no reason to have such an ugly constructor - how about renaming it during mapping?


```haskell
data LegacyMsg 
  = Error Binary
  | Data Binary

instance legacyMsg_runtimeType ::
  RuntimeType
    LegacyMsg
    (RTOption (RTTuple2 (RTLiteralAtomConvert "err" "error") RTBinary)
               RTTuple2 (RTLiteralAtom "data" RTBinary))
```

We still have that atom all by itself however, the atom 'fin' isn't representable as a Purescript type, it is only an Atom but we'd like still like to be able to receive it! 

Well, we can build a type that represents our incoming messages, including that atom.

```haskell
type Msg = Union |$| AtomSymbol.Atom "fin" |+| LegacyMsg |+| Nil
```

And then all that is left is to use this in a process that can receive these messages


```haskell
workerLoop :: ProcessM LegacyApi.Msg Unit
workerLoop = do
  msg <- receive
  ( case_ 
      # on (\(m' :: LegacyMsg) ->
          case m' of 
            Err err -> ...
            Data bin -> ...
        )
      # on (\(_ :: AtomSymbol.Atom "fin") ->
            ...
        )
  ) msg

startWorker :: ProcessM LegacyApi.Msg Unit
startWorker = do
  LegacyApi.subscribe 
  workerLoop
```

This needs to be exhaustive or we will get a compile error which is super cool. 

All of this works without an additional process or a fumbly mapping layer. It's not perfect and relies on actually getting the type description correct but this at least presents a way of doing it without writing error-prone Erlang, which can be useful when dealing with APIs that have a whole array of random structures.

