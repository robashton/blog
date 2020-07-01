An extra post was required on this topic..

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
- Arbitrary messages and Stetson handlers
- Monitors for arbitrary pids from Gen servers + Stetson handlers
- WebSocket handlers in Stetson
- Streaming handlers in Stetson
- MessageRouting in Pinto to easily bind to legacy code that sends us messages

# The story so far

Stetson was thrown together at the same time as Pinto to enable me to start building real products in Purerl and I didn't have a lot of use for websockets at that time. When the need occurred in a client project, I added a new handler (WebSocketHandler) to Stetson to deal with this with an 'emitter' function as part of the 'init' callback and got on with my life.


```haskell

busEvents :: ReceivingStetsonHandler ExternalMessage Unit
busEvents =

  WebSocket.handler (\req -> WebSocket.initResult req unit)
  # WebSocket.init (\emitter s ->  do
                             Bus.callback msg ExternalMessages.bus $ emitter <<< ExternalMsg
                             pure $ NoReply s
                             )
  # WebSocket.handle (\msg state -> pure $ NoReply state)
  # WebSocket.info (\msg state -> pure $ Reply ((TextFrame $ writeJSON msg) : nil) state)
  # WebSocket.yeeha

```

While I was "getting on with my life", people were writing code on top of this, and a pull request came into Stetson to add a mapper for arbitrary messages being received into the process that I accepted without a second thought. 

```haskell


nativeMapper :: forall msg. msg -> ExternalMessage

_ <- (Stetson.registerMapper $ nativeMapper SomeConstructor SomeOtherConstructor)
_ <- subscribeToNativeEvents

```

At some point a month ago, I was asked about the Loop handler and streaming in Stetson by a colleague, I gazed apon the abomination that was external mapping and realised that it was time to do a proper job of unifying these handlers (a single type for all of them, meaning the death of 'yeeha' sadly), deleting the ability to register external mappers and providing the ability to switch from a Rest handler into a Loop handler as part of content negotiation.  The actual means of doing this isn't worth covering in this post, but the end result is that we now had three namespaces for building handlers over the top of a single record and a standard pattern of being supplied an 'emitter' function in the init callback for Loop and WebSocket for sending typed messages into had handler.

# Straight Rest

```haskell

rest :: StetsonHandler {}
Rest.handler (\req -> Rest.initResult req {})
  # Rest.serviceAvailable (\req s -> Rest.result true req s)
    # Rest.allowedMethods (\req url -> Rest.result (Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req s)
    # Rest.contentTypesProvided (\s url -> Rest.result (jsonWriter : nil) req s)

```

# Rest into Loop

```haskell

eventsFirehoseRest :: StetsonHandler EventsWsMsg Unit
eventsFirehoseRest =
  Rest.handler (\req -> Rest.initResult req unit)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (streamEvents : nil) req state)
    # Loop.init (\emitter req state -> do
                              _ <- SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> emitter
                              pure state)
    # Loop.info (\(BookMsg msg) req state ->  do
          _ <- streamBody (stringToBinary $ writeJSON msg) req
          pure $ LoopOk req state)
    where
          streamEvents = tuple2 "application/json" (\req state -> do
                         req2 <- streamReply (StatusCode 200) Map.empty req
                         Rest.switchHandler LoopHandler req2 state)

```

# Straight Loop

```haskell

eventsFirehoseLoop :: StetsonHandler EventsWsMsg {}
eventsFirehoseLoop =
   Loop.handler (\req -> Loop.initResult req {})
    # Loop.init (\emitter req state -> do
                              _ <- SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> emitter
                              pure s{})
    # Loop.info (\(BookMsg msg) req s -> do
          _ <- streamBody (stringToBinary $ writeJSON msg) req
          pure $ LoopOk req s)

```          

# WebSocket


```haskell

eventsWs :: StetsonHandler EventsWsMsg Unit
eventsWs =
  WebSocket.handler (\req -> WebSocket.initResult req unit)
  # WebSocket.init (\emitter req s ->  do
                              _ <- SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> emitter
                              pure $ Stetson.NoReply s
                             )
  # WebSocket.handle (\frame state -> pure $ Stetson.NoReply state)
  # WebSocket.info (\(BookMsg msg) state -> pure $ Stetson.Reply ((TextFrame $ writeJSON msg) : nil) state)

```

Similarly to the [last post](/entries/purerl-updates---arbitrary-messages-and-handle_info-in-gen-servers.html), doing this as an emitter function made sense on first pass, but this was swiftly replaced with a plain ol' pid because it played nicer with monitors, existing APIs, etc.

# Passing in a Pid instead

```haskell

eventsWs :: StetsonHandler EventsWsMsg Unit
eventsWs =
  WebSocket.handler (\req -> WebSocket.initResult req unit)
  # WebSocket.init (\self s ->  do
                              _ <- SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self
                              pure $ Stetson.NoReply s
                             )
  # WebSocket.handle (\frame state -> pure $ Stetson.NoReply state)
  # WebSocket.info (\(BookMsg msg) state -> pure $ Stetson.Reply ((TextFrame $ writeJSON msg) : nil) state)

```

This all said, requiring this pid to be passed in as an argument is quite awkward, having to stash it in state if we want to access it from outside of our init function etc, once again StateT was employed so that the API for Stetson and Pinto would be aligned.


```haskell

eventsWs :: StetsonHandler EventsWsMsg Unit
eventsWs =
  WebSocket.handler (\req -> WebSocket.initResult req unit)
  # WebSocket.init (\req s ->  do
                              self <- WebSocket.self
                              _ <- WebSocket.lift $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send emitter
                              pure $ Stetson.NoReply s
                             )
  # WebSocket.handle (\frame state -> pure $ Stetson.NoReply state)
  # WebSocket.info (\(BookMsg msg) state -> pure $ Stetson.Reply ((TextFrame $ writeJSON msg) : nil) state)

```

This then allows Stetson to stash internal state in the underlying implementation and surface an API over this; indeed there is no way of accidentally calling the wrong 'self' and sending messages to the wrong process much like in Pinto. Every callback takes place within a typed  context that enforces what 'state' and 'msg' are - pretty neat.
