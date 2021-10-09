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

This cuts down on the cruft somewhat, as instead of having to pass a context everywhere, one can simply write functions in the context of ProcessM, re-writing that initial example now looks like this (Note that the log call needs lifting into *ProcessM* because it is written as an *Effect*)

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
