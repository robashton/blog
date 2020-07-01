We've covered the essential upgrades to Pinto and Stetson, so now we'll cover a quick bonus topic which is one of the concepts thrown into Pinto to help with all of this work.

# Previous Purerl posts

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)
- [Shared code twixt Purescript server and client](/entries/shared-code-twixt-purescript-server-and-client.html)
- [Purescript interop with native Erlang, interaction with Redis](/entries/purescript-interop-with-native-erlang---interacting-with-redis.html)

# Updates

- [Nix overlays for Purerl/etc](/entries/updates-to-pinto+stetson---purerl-in-progress.html)
- [Typed routing for Stetson](/entries/purerl-updates---typed-routes-in-stetson.html)
- [Arbitrary messages and handle_info in gen_servers](/entries/purerl-updates---arbitrary-messages-and-handle_info-in-gen-servers.html)
- [Arbitrary messages and Stetson handlers](/entries/purerl-updates---arbitrary-messages-and-stetson-handlers.html)
- [Monitors for arbitrary pids from Gen servers + Stetson handlers](/entries/purerl-updates---monitors-in-stetson-and-pinto.html)
- MessageRouting in Pinto to easily bind to legacy code that sends us messages


# The problem


A *lot* of legacy Erlang code (ours included) will have something along the lines of 

```erlang

cool_api:do_something().

```

That behind the scenes will almost immediately do a call to *self()* to get the caller pid and then probably spin up some more processes and start sending messages back to us.


```erlang
  
  do_something() ->
    Self = self(),
    spawn_link(fun Fun() ->
      receive
        _ ->  ok
      after 1000 ->
         Self ! hi
         Fun()
      end
    end).

```

In this case, we've got a native function called do_something() that captures the current pid, spins up a process which will stop if it receives anything and otherwise every second send a message back to the parent (hi).

If we were to write FFI for this, it'd look a lot like this:


```haskell

foreign import doSomething :: Effect Pid
foreign import stop :: Effect Unit


```

```erlang

doSomething() ->
  fun() ->
    cool_api:do_something()
  end.

stop(Pid) ->
  fun() ->
    Pid ! this_will_stop_you_cos_you_received_something
  end.

```

We would immediately start receiving atoms of 'hi' to the calling process, which unless we happen to be very specific and careful, won't know how to receive them, for example in a gen server.

```haskell

type State = {}

serverName :: ServerName State Atom
serverName = Local $ atom "listener"

startLink :: Effect StartLinkResult
startLink =
  Gen.buildStartLink serverName init $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: Gen.Init State Atom
init args = do
  Gen.lift CoolApi.doSomething
  pure $ {}

handleInfo :: Atom -> State -> Gen.HandleInfo State Atom
handleInfo msg state = do
  -- got an atom, woo
  -- not much to do with it
  pure CastNoReply state

```

This will work, it's a gen server that knows how to receive atoms - but it's unlikely we'll want to write a gen server that only receives atoms and nothing else - as soon as we add a timer, monitor or subscribe to anything else we'll want to change our message type into an ADT so that we can dispatch over the various message types.

```haskell

type Msg = CoolApiMsg Atom
         | Tick

```

So we're going to need some way to map this. 

*The old way*: We'd register a mapping function with the gen server that would recognise the cool_api messages and convert them into the right type for us, this was janky AF and has been deleted in the latest Pinto
*the new way*: Proxy process that receives the message, translates it and then sends it on to the main process

That proxy process is a burden to create because if we start spawning processes in Erlang, we need to make sure we monitor the parent so we terminate when it does, yada yada yada what a mess. Thankfully this is what *MessageRouter* in Pinto is for, it neatly wraps up this common pattern safely so we don't have to.

# Pinto.MessageRouter


The message router exports three functions of interest, one of which invokes a router on top of a process that will always start, and one on top of a process that might fail, the third takes a RouterRef (returned on success) and terminates the router.

```haskell

    startRouter :: forall handle msg. Effect handle -> (handle -> Effect Unit) -> (msg -> Effect Unit) ->  Effect (RouterRef handle)

    maybeStartRouter = maybeStartRouterImpl RouterRef

    stopRouter  :: forall handle. RouterRef handle -> Effect Unit

```

We'll focus on the simple case. 

- Given an *Effect handle* - ie something that returns some reference to whatever is created (in our case a pid)
- Given a function that given that handle, terminates the process
- Given a callback that takes 'whatever is received' and 'does something to it' (*Effect Unit*)
- We'll get an *Effect* of *(RouterRef handle)* back (which we can hold onto in order to terminate the whole show by calling stopRouter)


Wrapping our legacy API is "simple" now that we've already written the FFI for it

```haskell

import Pinto.MessageRouting as MR

wrappedDoSomething :: forall. (Atom -> Effect Unit) -> Effect (MR.RouterRef Pid)
wrappedDoSomething recv = MR.startRouter CoolApi.doSomething CoolApi.stop recv


```

With this, we can re-write our gen server with the message lifted into the appropriate type

```haskell

type State = {}
data Msg = Tick | DoSomething Atom

serverName :: ServerName State Msg
serverName = Local $ atom "listener"

startLink :: Effect StartLinkResult
startLink =
  Gen.buildStartLink serverName init $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: Gen.Init State Msg
init args = do
  self <- Gen.self
  Gen.lift Wrapper.wrappedDoSomething $ send self <<< DoSomething
  pure $ {}

handleInfo :: Msg -> State -> Gen.HandleInfo State Atom
handleInfo msg state = do
  case msg of
    Tick -> ...
    DoSomething msg -> ...

```

It means an extra process per router, so isn't something we want to be using if we're going to be spinning up 1000s of short lived versions of it, but for that sort of thing we're in specialist territory where we'd be using a look up table or dropping to plain ol' Erlang. (See also *Pinto.Timer* which just uses the underlying mechanisms to send messages of the right type directly without an intermediary process.

The point is that wrapping up legacy code that sends us arbitrary messages has been turned into a relatively small amount of work as a result of these changes, so long as we supply a start function and a stop function and a callback that knows what to do with the messages we can transform and then send accordingly. This has been used across our codebases with great success (as well as in Pinto itself) and has enabled our gen servers and web handlers to remain clean and receive the right typed messages.









