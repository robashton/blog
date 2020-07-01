A continuation of progress updates on Pinto/Stetson then..  
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
- Arbitrary messages and handle_info in gen_servers
- Monitors for arbitrary pids from Gen servers + Stetson handlers
- WebSocket handlers in Stetson
- Streaming handlers in Stetson
- MessageRouting in Pinto to easily bind to legacy code that sends us messages

# The story so far

The code for dealing with handle_info was very hand-wavey and involved the creation and registration of mappers and receivers within the gen server itself. This also ended up abusing gen_server:cast in order to function correctly and it wasn't really obvious where messages were coming from. It was a ticking time bomb as far as supporting the increasing amounts of code we are writing in Purescript goes.

There was *some* good in this approach, in that the type of the Gen Server specified both the State of the Gen Server and the type of the Msg it would receive, and the handleInfo function could  be supplied  in Gen.init, forcibly typed with this server name.

```haskell

  data Msg = Tick | SomethingHappened String

  type State = { 
      -- some stuff 
  }

  serverName :: ServerName State Msg
  serverName = Local $ atom "my_server"

  startLink :: Effect StartLinkResult
  startLink = Gen.startLink init handleInfo

  init :: Effect State 
  init = do
    SomethingElse.registerForEvents serverName SomethingHappened
    pure {}


  handleInfo :: Msg -> Effect (CastResult State)
  handleInfo msg = do
    case msg of
      Tick -> doTIck
      SomethingHappened id -> handleSomething id


```

Having to provide serverName as part of the registration function is clunky AF, under the hood this places the responsibility of mapping messages to the external module and  there is a disconnect between *that* and the handleInfo we supplied  as part of startLink. 


# A first pass, emitter functions 

The code was changed so that an emitter function would be extractable from within a gen server, this would be typed around ServerName automatically and only the right type of messages would be capable of being passed into it.


```haskell

init :: Effect State = do
init = do
    emitter <- Gen.emitter serverName
    SomethingElse.registerForEvents $ emitter <<< SomethingHappened
    pure {}

```

This is somewhat an improvement, as it could at this point be assumed that anything passed into that function would automatically be the right type for handle_info and the mapping code from inside the gen server could be removed entirely. It requires the use of proxy processes to intercept messages, and I spent a day or two upgrading nearly all of our company Purescript over to this new model because it felt good.

It didn't feel *great* after doing that though, once again we're relying on convention to create that emitter with the right 'serverName' and it's not very 'Erlang', in theory it also means that code could be written to send messages to arbitrary gen servers providing you have access to the serverName and thats a bit naff.


# Second pass, making it more Erlang

The type of 'emitter' was changed to *Process Msg* (A welcome suggestion from [http://twitter.com/louispilfold](@louispilfold) when I was putting code samples out for feedback). This maps under the hood to a new type of a plain ol' pid and is therefore compatible automatically with classic Erlang APIs. (Specifically erlang monitors and such being a useful end-goal here).


```haskell

init :: Effect State = do
init = do
    self <- Gen.self serverName
    self ! DoSomeStuffAfterStartup
    SomethingElse.registerForEvents $ send self <<< SomethingHappened
    pure {}

```

This was still not ideal however, the call to Gen.self included a runtime check (below) to ensure that the caller was indeed the "self" we were looking at to prevent external clients from abusing the API (if you provide an API, it *will* be abused and I'd already seen some "interesting" code already written around these APIs while I was upgrading just our own code!)


```erlang

selfImpl(Name) ->
  fun() ->
    Pid  = where_is_name(Name),
    Self = erlang:self(),
    if
      Self =:= Pid -> Self;
      true ->
        exit(Self, {error, <<"Gen.self was called from an external process, this is not allowed">>})
    end
  end.


```

#  Third pass, making it more Purescript

Sod it, StateT it is. We'd been discussing moving the Gen callbacks into a state monad since I first wrote Pinto, the only obstacle being that I didn't understand state monads, which sounds stupid on retrospect but it's the truth *shrug*. I read a few tutorials, had a mild "aha" moment and things became a bit clearer.

What we really want is that all the callbacks to automatically

- Be typed around ServerName, so that all calls to Pinto APIs automatically assume this type
- have access to the 'internal' state in the Pinto gen_server implementation, so no casts ever have to be made again

We had quite a bit of code in Gen.purs (our gen server wrapper) that relied on making casts to modify its state, monitors and such - removing all of this was just a sensible idea -  the idea being that if the callbacks to client coded operated within the context of that state, it could be retrieved and modified (optionally) as part of those callbacks.


```haskell

startLink :: Effect StartLinkResult
startLink = Gen.startLink init handleInfo

init :: Gen.Init State Msg
init = 
  self <- Gen.self
  Gen.lift $ SomethingElse.registerForEvents $ send self  <<< SomethingHappened
  pure {}

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state =
  case msg of
    Tick -> CastNoReply <$> handleTick state
    SomethingHappened ev -> CastNoReply <$> handleSomethingHappened ev state


```

On the surface of this it isn't that different, but we've done away with the need to constantly refer to 'serverName' because we're operating in the context of a state monad (Gen.Init and Gen.HandleInfo are type aliass to help refer to the fairly wordy type used behind the scenes in Pinto).

Gen.self doesn't need to do anything other than pull state out of that state monad and return it to the client code (implementation below), this means that unless your code is being executed in the context of the state monad (IE: the gen server) you can't call it and the runtime checks and side effects can go away.


```haskell

  self :: forall state msg. StateT (GenContext state msg) Effect (Process msg)
  self = do
    GenContext { pid } <- State.get
    pure pid

```

Similarly, Gen.Cast and Gen.Call are provided for *those* callbacks too, and all code executed  within the context of a Pinto Genserver has access to the internal state via the API so in theory things like trapExit/handleInfo/config can be modified safely from within that context without doing weird things around async casts back to that gen server.


That's a lot of words to say that Gen Servers and arbitrary messages are now very pretty indeed in Purerl. Example below of a gen server subscribing to a message bus from the [demo_ps](https://github.com/id3as/demo-ps/blob/master/server/src/HandleInfoExample.purs) web project. You'll note that the actual API used in startLink has evolved to include a builder for setting the initial handlers/etc - there are a number of optional things to tweak about a gen server and it made sense to do this rather than accept an endlessly growing list of arguments.

```haskell

type BookWatchingStartArgs = {}
type State = {}

data Msg = BookMsg BookEvent 

serverName :: ServerName State Msg
serverName = Local $ atom "handle_info_example"

startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: BookWatchingStartArgs -> Gen.Init State Msg
init args = do
  self <- Gen.self
  _ <- Gen.lift $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self
  pure $ {}
 
handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state = do
  case msg of
    BookMsg bookEvent -> 
      Gen.lift $ handleBookEvent bookEvent state

handleBookEvent :: BookEvent -> State -> Effect (CastResult State)
handleBookEvent ev state =
  case ev of
    BookCreated isbn -> do
      _ <- Logger.info1 "Book created ~p" isbn
      pure $ CastNoReply state
    BookDeleted isbn -> do
      _ <- Logger.info1 "Book deleted ~p" isbn
      pure $ CastNoReply state
    BookUpdated isbn -> do
      _ <- Logger.info1 "Book updated ~p" isbn
      pure $ CastNoReply state

```

I'll insert another item to the list of 'new things' to the bullet points currently being traversed as my next post will be about the corresponding message handling implementation in [Stetson](https://github.com/id3as/purescript-erl-stetson), which unsurprisingly uses the State monad to improve our lives there as well. Once you learn how to use a hammer, everything looks like a nail I guess.
