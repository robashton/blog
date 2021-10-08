It has been about three years since I first sat in a hotel in Lithuania throwing together the first versions of [purescript-erl-pinto](https://github.com/id3as/purescript-erl-pinto) and [purescript-erl-stetson](https://github.com/id3as/purescript-erl-stetson) so that we could get started on a project for one of our clients.

Quite a lot of code has been written against those projects internally by colleagues and myself and over time:

- Various improvements/patterns have been discovered within core libraries such as [purescript-erl-process](https://github.com/purerl/purescript-erl-process). 
- A heap of Erlang specific packages have been written and released
- Another mountain of packages have been ported across from Purescript 
- Pinto and Stetson have been upgraded repeatedly by various colleagues as our understanding of what we need them to do has evolved.

The [purerl cookbook](https://purerl-cookbook.readthedocs.io/) has been updated for these latest releases, as has the [demo-ps](https://github.com/id3as/demo-ps) but felt it worthwhile highlighting some of the changes in a few (of my increasingly rare) blog posts.

In this post we'll be looking at Processes and the concept of 'self'

Processes
=========

A common practise in Erlang codebases is to spawn a new process and use its Pid for communication.

```erlang
example() ->
  Pid = spawn_link(fun receive_message/0)
  Pid ! hi.

receive_message() ->
  receive
    SomeMessage -> 
      ?PRINT("Got a message"),
      ok
   end.
```

In Purerl, the type *Pid* lives in *Erl.Process.Raw* and is just an imported foreign type

```haskell
foreign import data Pid :: Type
```

This typically isn't used to any great amount except in some specific FFI cases, it being far better generally to use the types found in *Erl.Process* which have the phantom type *msg* floating around meaning all the sends and receives are limited to the types of message that that process has declared it will receive. *Process msg* itself is just a newtype around Raw.Pid of course.

```haskell
newtype Process (a :: Type)
  = Process Raw.Pid
```

To create a typed process, one could call *spawnLink* and provide a callback which will be executed in the process created in that underlying call to *spawn_link* in Erlang. A change made fairly early on in development was to change this callback from something that took some context, to something that operated inside *ProcessM msg r*, with the context being provided by that monad - this will become important later on in this blog entry so I'll demonstrate this here.

The old way
-----------

"Given a callback that accepts a *SpawnLinkContext* typed around *msg*, run that callback inside a new process and return that new process, also typed around *msg*"

```haskell
spawnLink :: forall msg. (SpawnLinkContext msg -> Effect Unit) -> Effect (Process msg)
```

That context then provided the means of receiving messages, being defined as something like

```haskell
type SpawnLinkContext msg = 
  { receive :: Effect msg 
  , receiveWithTimeout :: Timeout -> Effect (Maybe msg)
  }
```

Thus, the Erlang example, re-written in Purerl would have looked something like this

```haskell
example :: Effect Unit
example = do
  pid <- spawnLink receiveMessage
  pid ! "hi"

receiveMessage :: SpawnLinkContext String -> Effect Unit
receiveMessage c = do 
  msg <- c.receive
  log "Got a message"
```

The new way
-----------

"Evaluate the given code in the context of a *ProcessM* typed around *msg*"

```haskell
spawnLink :: forall msg. ProcessM msg Unit -> Effect (Process msg)
```

And quite simply, any calls to receive/etc are defined as functions that operate inside ProcessM, again all typed around *msg*

```haskell
receive :: forall msg. ProcessM msg msg
receive = ProcessM Raw.receive
```

This cuts down on the cruft somewhat, as instead of having to pass a context everywhere, one can simply write functions in that context, re-writing that initial example now looks like this (Note that the log call needs lifting into *ProcessM* because it is written as an *Effect*)

```haskell
example :: Effect Unit
example = do
  pid <- spawnLink receiveMessage
  pid ! "hi"

receiveMessage :: ProcessM String Unit
receiveMessage = do 
  msg <- receive
  liftEffect $ log "Got a message"
```

Extending this model
--------------------

It turns out that this is quite a nice pattern for representing the different _types_ of process available in Erlang (OTP, Cowboy and indeed our own application code), consider:

- Cowboy Loop handlers
- Cowboy Websocket handlers
- OTP gen_server
- OTP supervisor
- OTP gen_statem

All of these could be ran as *ProcessM*, except they have more types associated with them _and_ various functions available designed for use in those specific contexts, for example in the simple case of cowboy..

```haskell
type WebSocketInfoHandler msg state
  = msg -> state -> WebSocketResult msg (WebSocketCallResult state)
```

We have the types *msg* and *state* in our type because the callbacks involved tend to take *state* and there is a callback (*info*) for messages received by the loop handler typed around *msg*

In the more complicated case of an OTP GenServer, this looks like this

```
type CallFn reply cont stop msg state
  = From reply -> state -> ResultT cont stop msg state (CallResult reply cont stop state)
```

Most operations take place inside that *ResultT* which encodes the *cont*, *stop*, *msg*, and *state* types for use with our operations. (*cont* being the message that can be received by *handle_continue*, *stop* being a custom stop reason, *msg* being messages received by *handle_info* and *state* being the state of the gen server).

We also end up with our own contexts in our own codebases for specific process types around common units of business logic. 

Common Functionality
-------------------

In most of these cases, just like with *Process.spawnLink*, something gets returned that represents the started process - for example the GenServer.


```haskell
startLink :: forall cont stop msg state. 
  (ServerSpec cont stop msg state) 
  -> Effect (StartLinkResult (ServerPid cont stop msg state))
```

Here we have a *ServerPid cont stop msg state* returned to the caller - again keeping a lot of useful information around to help us make calls into a GenServer but the type we're interested in here is *msg*. Most generic APIs will be written around the concept of a *Process msg* and what we have here is a *ServerPid cont stop msg state*

The logical step here is to expose

```haskell
toProcess :: ServerPid cont stop msg state -> Process msg
```

It turns out that this is a very common operation and so a typeclass is born and added to Erl.Process for everybody to implement when writing this kind of code.

```haskell
class HasProcess b a where
  getProcess :: a -> Process b
```

In fact, two typeclasses are born because some APIs only need a Pid after all.

```haskell
class HasPid a where
  getPid :: a -> Pid
```

Most functions that take a *Process msg* therefore don't actually care about it *being* a *Process msg*, but only that a *Process msg* can be gotten from the type

```haskell
callMe :: forall p msg. 
  HasProcess msg p => 
  p -> Effect Unit
```

And now all of those custom types can be used with a whole suite of APIs without having to unpack a convoluted structure of newtypes.

Self
----

There is one more common operation that has been ignored so far, and that is the concept of self. An incredibly common thing in Erlang is to invoke *self* to get the Pid of the current process.

```erlang
Self = self(),
some_api:call_me(Self)
```

For a while, we started having *self* methods on every module that exported some monad in which process logic could be evalulated, and this would return the full type of the process (complete with *cont*, *stop*, *state*, etc). There were a lot of *self* functions being exported and imported and in 99.99% of all cases they were immediately followed by a call to *getProcess* using the *HasProcess* typeclass implementation for that system.

```haskell
(me :: Process MyMsg) <- getProcess <$> GenServer.self
```

What does *self* mean then? The correct answer is as written above, but the correct answer isn't always the nicest answer - it was very rare that we would need anything from *self* other than the current *Process msg* and we were running into issues in modules that had code for more than one of these contexts in them, whose self are we using anyway?

The answer was to be pragmatic and create a typeclass for '*any m*' that allowed that '*m*' to export a *Process msg*

```haskell
class HasSelf (m :: Type -> Type) msg | m -> msg where
  self :: m (Process msg)
```

For the case of anything running inside a *ProcessM*, this isn't any more complicated than calling Raw.self to get the current pid and wrapping it up with the relevant newtypes

```haskell
instance selfProcessM :: HasSelf (ProcessM a) a where
  self :: forall a. ProcessM a (Process a)
  self = ProcessM $ Process <$> Raw.self
```

Similar implementations then exist for Pinto/Stetson contexts, allowing code to simply call *Process.self* from practically anywhere to get a typed *Process msg* valid for the current context.

```haskell
SomeApi.callMe <<< self
```

All typed, all safe, nobody sent messages they can't handle - living the dream.


---------------------------------------

Subscriptions
=============

Two blog posts in a week? Unheard of - following on from last week's blockbuster smash hit epic "[Purerl Updates - Processes and Self](/)", today we have an entry describing our current thoughts on subscriptions in a Purerl world.

A standard procedure in Erlang, is to invoke an API that then proceeds to send messages back to the invoking process.

```erlang
some_api:subscribe()
```

More often or not, an overload will be provided allowing a Pid to be passed in, in case you didn't want it sending to *self*.

```erlang
some_api:subscribe(Pid)
```

We could write a function that looks like this in Purescript (using the typeclasses explored in the previous blog entry).

```haskell
module SomeApi where

data Msg = HelloWorld | Goodbye

subscribe :: forall process. 
  HasProcess Msg process => 
  process -> Effect Unit
subscribe = ...

```

In this, we are saying "Given a process that accepts *Msg*, then the subscription will send *msg* to you.

```haskell
workerLoop :: ProcessM SomeApi.Msg Unit
workerLoop = do
  msg <- receive
  case msg of 
    HelloWorld -> ...
    Goodbye -> ...

main :: Effect Unit
main = do 
  process <- spawnLink workerLoop
  SomeApi.subscribe process
```

This isn't great, typically we don't write processes this way - processes usually not only want to subscribe to messages from external sources, but also send themselves messages, typically they'll achieve that by lifting messages into a process specific data type.

```haskell
data ProcessMsg 
  = ApiMsg SomeApi.Msg
  | Tick

workerLoop :: ProcessM ProcessMsg Unit
workerLoop = ...

```

Calling *SomeApi.subscribe* with this process will result in a compiler error because SomeApi.Msg is the wrong type.

Emitters
--------

The pattern we've been using up until now to side-step this is for subscription calls to take in a blank cheque in the form of *msg -> Effect Unit*, allowing the caller to decide what to do with messages on that subscription.


```haskell
module SomeApi where

data Msg = HelloWorld | Goodbye

subscribe :: (Msg -> Effect Unit) -> Effect Unit
subscribe = ...

```

Subscription could then look like this

```haskell
data ProcessMsg 
  = ApiMsg SomeApi.Msg
  | Tick

main :: Effect Unit
main = do 
  process <- spawnLink workerLoop
  SomeApi.subscribe $ send process <<< ApiMsg
```

More commonly the use of some form of *self* would be used inside the process itself, as it makes little sense for one process to subscribe on behalf of another.

```haskell
workerLoop :: ProcessM SomeApi.Msg Unit
workerLoop = do
  msg <- receive
  case msg of 
    HelloWorld -> ...
    Goodbye -> ...

startWorker :: ProcessM SomeApi.Msg Unit
startWorker = do
  me <- self
  liftEffect $ SomeApi.subscribe $ send me <<< ApiMsg
  workerLoop

main :: Effect Unit
main = do 
  _ <- spawnLink workerLoop

```

This *works* and is elegant - and indeed for about two years this has been The Way. Our codebase is/was littered with *send me <<< Msg*, but in hindsight this isn't great.


Why not? The problem with *Effect Unit* is that it can do anything. *ANYTHING*. The problem with passing an *Effect Unit* is that it can do that "anything" within the context of a completely different process. This can cause problems.


*Errors*: What happens if the Effect Unit causes an exception? Who crashes? The remote process? Who does that affect? Anybody else who is currently subscribed? How do they find out? Are they in the right supervision tree for this not to be a problem? Can we just swallow the exception? How does the caller find out that it crashed? Oh dear.

*Interference*: What if this is in a message loop doing other things? What if the *Effect Unit* takes up precious time? What happens to the backlog? What happens to anything awaiting a message with a timeout? 

While the reality is that while nearly all our callbacks are implemented as *send me <<< Msg*, as our codebases grow and we do more and more of these things, somebody is eventually going to cause problems with the above and we'll be looking at hilarious (and needless) debug sessions as a result. If not us - then somebody else using Purerl (and our numbers *are* growing...).

HasSelf to the rescue
---------------------

HasSelf was added fairly recently and makes it possible to write code that operates inside any monad provided it has an implementation of 'self'.

```haskell
saferSubscribe ::
  forall m msg.
  MonadEffect m =>
  HasSelf m msg =>
  (Msg -> msg) -> m Unit
saferSubscribe f = do
  me :: (Process msg) <- self
  liftEffect $ subscribe $ send me <<< f
  pure unit
```

Adding onto that, we need to assert that we can evaluate effects in this monad (MonadEffect m =>) in order to actually issue the subscription and suddenly our call to this becomes

```haskell
workerLoop :: ProcessM SomeApi.Msg Unit
workerLoop = do
  msg <- receive
  case msg of 
    HelloWorld -> ...
    Goodbye -> ...

startWorker :: ProcessM SomeApi.Msg Unit
startWorker = do
  SomeApi.saferSubscribe ApiMsg
  workerLoop

main :: Effect Unit
main = do 
  _ <- spawnLink workerLoop

```

Note: The liftEffect is gone, as is the call to 'self', the API looks a *lot* more like the original Erlang and is safer while we're at it. 

Because it's no longer a blank cheque, we should probably provide a second method (just like in Erlang) that takes a process to send messages to instead of simply relying on 'self'.

```haskell
saferSubscribeTo ::
  forall msg.
  Process msg -> (Msg -> msg) -> Effect Unit
saferSubscribeTo p f = do subscribe $ send p <<< f
```

This is how most of our subscriptions now look across Pinto/our own code/etc. It's safer for everybody concerned and fits well with the ethos of 'trying to look like the Erlang APIs most of this stuff is built on top of'.


Untagged Unions + Erlang Messages
=================================

Yes, another blog post about Purerl, following on from

- [Purerl Updates - Processes and Self](/)
- [Purerl Updates - Subscriptions](/)

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







