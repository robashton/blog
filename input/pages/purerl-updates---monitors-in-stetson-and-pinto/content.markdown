We've managed to get nicely typed arbitrary messages into our web handlers and gen servers, now it's time to look at Monitors.

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
- Monitors for arbitrary pids from Gen servers + Stetson handlers
- MessageRouting in Pinto to easily bind to legacy code that sends us messages

# Monitors

A reasonably common pattern for monitors in some of our code is

- Internal server manages a collection of pids that are recipients of data streams
- web handlers open and register with this server to receive those data streams

In this case *sometimes*

- It's useful for the internal server to monitor the subscribers and remove the pids when they become invalid
- It's useful for the web handler to monitor the server, so it can close the connection if that goes away

This isn't always the case, sometimes a static message bus is a better option, sometimes pids can be checked ad-hoc, but for the purpose of this example we'll assume that this is exactly what we want as it'll be a nice end-to-end example of message passing and monitoring in Purerl.

# The internal server

So we'll define a basic gen server that keeps a state that's a map of pids to functions that receive data *(Binary -> Effect Unit)*, and set up a timer to send us a *Tick* message after 500ms - our message type will therefore just be either that *Tick* message, or a message telling us that a client has disconnected. We'll configure the gen server to use a *handleInfo* function when these come in (explored further below).


```haskell

type State = {
  handlers :: Map.Map Pid MessageHandler
}

type MessageHandler = (Binary -> Effect Unit)

data Msg = ClientDisconnected Pid
         | Tick

startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

init :: BookWatchingStartArgs -> Gen.Init State Msg
init args = do
  self <- Gen.self
  void $ Gen.lift $ Timer.sendAfter 500 Tick self
  pure $ {
    handlers: Map.empty
  }

```

We can export a function *registerClient* for clients to invoke in order to start receiving data, while we're still in the process that called us we can get its pid by calling out to '*Pinto.self*', and then in the context of the gen server, we'll get our own pid so we can add the monitor in the next function *addHandler*.

As we have the pid of our calling process, we can invoke *Monitor.pid*, and pass in a callback that disregards the message given to us when the monitor pops and just sends a message with the handler pid back to our *handleInfo*. Once we're monitoring the handler, we can add it to our map using the pid as a key so we can easily remove it later when we get the message telling us it went down.

```haskell

registerClient :: MessageHandler -> Effect Unit
registerClient handler = do
  handlerPid <- Pinto.self
  Gen.doCall serverName \state -> do
     self <- Gen.self
     newState <- Gen.lift $ addHandler handler self handlerPid state
     pure $ CallReply unit newState

addHandler :: MessageHandler -> Process Msg -> Pid -> State -> Effect State
addHandler handler self handlerPid state@{ handlers } = do
  void $ Logger.info1 "Adding handler ~p as it has connected" handlerPid
  void $ Monitor.pid handlerPid (\_ -> self ! ClientDisconnected handlerPid)
  pure $ state { handlers = Map.insert handlerPid handler handlers }

```

All that is left therefore, is to handle the messages we might receive into the handleInfo we configured earlier as part of startLink. 

- If we get a ClientDisconnected, we simply remove the handler from our map so we no longer send any data to it
- If we get a Tick, we invoke sendData on the map of handlers, before scheduling another tick for 500ms time

```haskell

handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state@{ handlers  } = do
  case msg of
     ClientDisconnected handlerPid -> do
        void $ Gen.lift $ Logger.info1 "Removing ~p as it disconnected" handlerPid
        pure $ CastNoReply $ state { handlers = Map.delete handlerPid handlers }
     Tick -> do
        Gen.lift $ sendData handlers
        self <- Gen.self
        void $ Gen.lift $ Timer.sendAfter 500 Tick self
        pure $ CastNoReply $ state 

```

Sending data is easy, seeing as the handlers are just a list of effectful callbacks of *(Binary -> Effect Unit)*

```haskell

sendData :: Map.Map Pid MessageHandler -> Effect Unit
sendData handlers = do
  freshData <- getDataFromSomeNativeCode
  void $ traverse (\handler -> do handler freshData) $ Map.values handlers 
  pure unit

```

So that's an entire gen server, which 

- Allows the registration of callbacks that'll accept data
- Monitors the pids of the calling process, and removes the callbacks when the monitor pops
- Ticks every 500ms and traverses over the callbacks to send the data

Note: Because of the callback/pids there is nothing stopping us using this code from any other Purerl (or indeed Erlang). None of this is Pinto specific and this is very much by design.

# Subscribing (and monitoring) from Stetson

Speaking of other Purerl, a lot of Purerl gets written using Stetson to support Rest/Websockets/Streams/etc; so that's where we'll subscribe to this data. We'll also then add a monitor to that streaming process that closes the connection when it goes away. (We could also just block while we wait for it to restart for example).

So, we'll define a data type for our messages, we're either receiving data that needs to be streamed, our data source died, or our data source was already down when we tried to connect to it.


```haskell

data DataStreamMessage = Data Binary
                       | DataSourceDied
                       | DataSourceAlreadyDown


```

We'll just kick off our handler with *Loop.handler*, start a streamed reply with a status code 200 and make sure that Stetson knows we're doing a Loop, we're typed as a *StetsonHandler DataStreamMessage Unit* because we receive *DataSteamMessage* and don't store any state of our own.

```haskell
                                           
dataStream :: StetsonHandler DataStreamMessage Unit
dataStream =
  Loop.handler (\req -> do
               req2 <- streamReply (StatusCode 200) Map.empty req
               Loop.initResult req2 unit)

```

In our *Loop.init*, we'll get our own typed process *(Process DataStreamMessage)*, invoking '*Process.send*' on this gives us a function of type (Msg -> Effect Unit) so we'll compose that with a constructor for our own data type (Data) giving us the correct function type of *(Binary -> Effect Unit)*

Using *Gen.monitor* with the server name of *MonitorExample*, we can detect when that process dies - there are two effectful callbacks for this, one for when the process dies and one for if the process is already down (there is no pid to monitor). 


```haskell

    # Loop.init (\req state -> do 
                      self <- Loop.self
                      void $ Loop.lift $ MonitorExample.registerClient $ send self <<< Data
                      void $ Loop.lift $ Gen.monitor MonitorExample.serverName (\_ -> send self DataSourceDied) (send self DataSourceAlreadyDown)
                      pure unit)

```

We receive those messages in the *Loop.info* callback

- if we get data then we can stream that directly to the client and carry on looping
- if the data source died then we unceremoniously terminate the stream
- if the data source is already down then we unceremoniously terminate the stream

```haskell
    # Loop.info (\msg req state ->  do
                case msg of
                     Data binary -> do
                        _ <- Loop.lift $ streamBody binary req
                        pure $ LoopOk req state

                     DataSourceDied ->  do
                       pure $ LoopStop req state

                     DataSourceAlreadyDown ->  do
                       pure $ LoopStop req state

```

And that's that, we have a gen server running which will send data to any subscribers and clean up when those subscribers terminate, and we have a loop handler that'll subcribe to that data source and clean up if that data source dies. There is a lot going on here but it's worth unpicking as there are a lot of useful concepts here neatly packaged into a single example.

By sticking to plain ol' pids and callbacks, all of this code remains portable and not tied to either of these libraries, which is handy because at some point somebody smarter than I will write something more Purerl idiomatic for web serving and OTP wrapping and we'll probably want to switch to those things..
