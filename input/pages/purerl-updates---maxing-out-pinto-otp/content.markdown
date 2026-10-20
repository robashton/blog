
Yes, another blog post about Purerl, following on from

- [Purerl Updates - Processes and Self](/entries/purerl-updates---processes-and-self.html)
- [Purerl Updates - Subscriptions](/entries/purerl-updates---subscriptions.html)
- [Purerl Updates - Untagged Unions](/entries/purerl-updates---untagged-unions.html)

When I first threw together [Pinto](https://github.com/id3as/purescript-erl-pinto), it was with the aim that there simply be a usable path for building supervision trees and gen servers, and not that we would have access to the full functionality of those things straight out of the gate. This proved to be a sensible choice because as we learned more about how we would like to write code in Purescript built around various messaging mechanisms it also turned out that this code would need to evolve more than a few times before we were all truly happy with the model.

A lot of usable application code was written with this "V1" pass, but it was always known that eventually we would want to not only have access to other "gen" models (such as statem), but the bullet would need biting and the model would need expanding to allow us to use the full gamut of functionality exposed by gen_server.

Thankfully, it turns out that the general concepts that had evolved during the development and real-world use of V1 carried over, and the changes as far as the end-user is concerned are largely additive. (Note: I didn't do most of the work on V2, that was [@philipstears](https://github.com/philipstears) and [@adrianroe](https://github.com/adrianroe), so kudos to them on gathering the will for it).

The types
---------

In V1 Pinto, the types around the spinning up of processes was very GenServer focused

```haskell
data ServerName state msg = Local Atom
                          | Global GlobalName
                          | Via NativeModuleName Foreign
```

Not only that, but the result of calling the various *startLinks* for Supervisors/GenServers was very simplistic - losing any information about whatever the process was that was started. (Hello Raw.Pid)

```haskell
data StartLinkResult
  = Ok Pid
  | Ignore
  | AlreadyStarted Pid
  | Failed Foreign
```

As a result, this meant things like Supervisor had their ServerName defined as 

```haskell
type SupervisorName = ServerName Unit Unit
```

Which isn't *wrong* as such, but is completely pointless. State and Msg are GenServer constructs and they aren't even the only ones we need for a GenServer which is about to get a revamp anway. The answer (as it often does), lies in adding type parameters - while we're at it, changing the API to use an Either because that's a good way to represent success and failure.

```haskell
data NotStartedReason serverProcess
  = Ignore
  | AlreadyStarted serverProcess
  | Failed Foreign

type StartLinkResult serverProcess
  = Either (NotStartedReason serverProcess) serverProcess
```


We can't add constraints to the type definion itself, but we can enforce that when these things are used that *HasPid serverProcess =>*, which is what Pinto will do in most of its APIs. By doing this, we can have GenServers export a new type (ServerPid state msg) and Supervisors export their own type (SupervisorPid) which both have Raw.Pid underlying them, but GenServer can also *(HasProcess msg (ServerPid state msg))* which Just Makes Sense.

A similar thing can be done to ServerName

```haskell
data RegistryName :: Type -> Type
data RegistryName serverType
  = Local Atom
  | Global Foreign
  | Via NativeModuleName Foreign
```

Why would we want that? Because calls into running processes (such as GenServer.call or Supervisor.startChild) might like to know type information in order to enforce some level of correctness, and while we're at it we can support doing this either either the Pid gained from startLink, or with the registered name of that child

```haskell
data RegistryReference :: Type -> Type -> Type
data RegistryReference serverPid serverType
  = ByPid serverPid
  | ByName (RegistryName serverType)
```

Now this has been done, we're free to re-write Supervisor (or indeed, as we will see, split it into two different modules), and re-write GenServer with all the types we want without that leaking across implementations.

GenServers
----------

Consider the following very boring GenServer in V1 that starts up, subscribes to a bus, and receives messages via handleInfo


```haskell
type BookWatchingStartArgs = {}
type State = {}

data Msg = BookMsg BookEvent 

serverName :: ServerName State Msg
serverName = Local $ atom "handle_info_example"

startLink :: BookWatchingStartArgs -> Effect StartLinkResult
startLink args =
  Gen.buildStartLink serverName (init args) $ Gen.defaultStartLink { handleInfo = handleInfo }

currentState :: Effect State
currentState = Gen.call serverName \state -> pure $ CallReply state state

init :: BookWatchingStartArgs -> Gen.Init State Msg
init args = do
  self <- Gen.self
  _ <- Gen.lift $ SimpleBus.subscribe BookLibrary.bus $ BookMsg >>> send self
  pure $ {}

 
handleInfo :: Msg -> State -> Gen.HandleInfo State Msg
handleInfo msg state = do
  case msg of
    BookMsg bookEvent -> 
      -- TODO: Something with bookEvent
      pure $ CastNoReply state

```

There are a few things of note here

- The GenServer *has* to have a name (not strictly true but..), this isn't really necessary for lot of  *simple_one_for_one* for example
- How can 'init' fail?
- How does one stop this gen server from inside the gen server?
- How does one defer a reply in response to a 'call' (Usually gen_server would give you a 'from' argument, this has been dropped here)
- The use of 'Gen.self' (see also: [Purerl Updates - Processes and Self](/entries/purerl-updates---processes-and-self.html)
- How does one do a handle_continue, and what is its type going to be?

As we can see, this largely comes down to missing items from the surface area of the gen server rather than a fundamental design flaw, but nevertheless there are some breaking changes to get to the point where these things are supposed. 

Starting a GenServer
--------------------

A few things need to change here

- A GenServer can ask to 'continue' an exchange by returning the current operation and re-entering with a message sent internally - we need a type for this
- A GenServer can ask to 'shutdown', and can supply a 'reason' for that shutdown that can be intercepted elsewhere/internally and it'd be nice if that was typed
- The return result of the startLink isn't typed *at all*, this should be a Pid of some sort that encapsulates that types of messages this GenServer can receive
- Name should be optional

Thus, we need two extra types added to our ServerName (Cont and Stop), and StartLinkResult actually needs to be typed around some sort of Pid.


```haskell
serverName :: RegistryName (GenServer.ServerType Unit Unit Msg State)
serverName = Local $ atom "handle_info_example"

startLink :: BookWatchingStartArgs -> Effect (StartLinkResult (GenServer.ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec (init args)) { name = Just serverName, handleInfo = Just handleInfo }

init :: BookWatchingStartArgs -> GenServer.InitFn Unit Unit Msg State 
init _args = do
  _ <- SimpleBus.subscribe BookLibrary.bus BookMsg 
  pure $ InitOk {}
```

So

- serverName is now a RegistryName parameterised with *GenServer.ServerType (Cont Stop Msg State)*
- GenServer.startLink now simply takes a record full of config for which a default can be summoned with defaultSpec
- serverName is completely optional (as is handleInfo, both are Maybes to indicate this)
- StartLinkResult is now parameterised with the type of Pid we're starting with, in this case a GenServer.ServerPid (Cont Stop Msg State)
- init now returns an indication as to whether it has actually succeeded or not..

We'll also note that with the addition of [HasSelf](/entries/purerl-updates---processes-and-self.html), we can now use subscribe without liftEffect, just relying on ambient context to get the messages sent to *self* (See also [Subscriptions](/entries/purerl-updates---subscriptions.html))

Our handleInfo has changed as well (as has every other callback, but in exactly the same way) as it needs to also include all the additional type information for this GenServer

```haskell
handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state = 
  case msg of
   BookMsg bookEvent -> 
      -- TODO: Something with bookEvent
      pure $ GenServer.return state
```

Conceptually, nothing has really changed in our basic use cases except more types have been added to various signatures to make additional functionality possible (which we can default to *Unit* until we need them), as well as all of the callbacks now operating inside a *HasSelf msg =>* (actually a *ResultT cont stop msg state*), so that all of the type information for this GenServer is available at all times.

A heap of methods now hang off GenServer to help return the appropriate responses from the various callbacks (CallFn, CastFn, InfoFn, ContFn, TerminateFn) which are all typed safely around whatever we've declared in our signature.

It's certainly a bit more verbose because of all of these extra types, but typically what ends up happening in our code is that we have business-process specific monads written which sit *inside* a GenServer and only expose the bits we need so that we don't have to write *Unit Unit Unit State* repeatedly. A lesson from the last couple of years with typed FP - with flexibility comes verbosity...

Supervisors
-----------

Supervisors in V1 were okay for most cases except *simple_one_for_one*. 

- A reading of the documentation for Erlang OTP reveals that in most cases, both the arguments *and* the return value of API calls into *supervisor:\** are special-cased for *simple_one_for_one*
- There was no way in V1 of tying the construction of the simple_one_for_one supervisor and the subsequent calls to *start_child* together, thus it was very hand-wavey with regards to type safety


The solution? The solution is to have two modules - one for "simple_one_for_one" (*Pinto.Supervisor.SimpleOneForOne*) and one for everything else (*Pinto.Supervisor*). This makes the API much easier because the return values of functions stop being dependent on the type of supervisor being ran (in Purescript at least) and we can disregard the special cases in both modules because the code doesn't allow for us to end up in situations where we're calling one type of supervisors with another's pid.

Just like with GenServers, we'll see that these are constructed around *RegistryName* and *StartLinkResult pid*, this is once again so that we have typed pids available for invocation into the various APIs in a standardised manner.

Normal supervisors (from [demo-ps](https://github.com/id3as/demo-ps)). 


```haskell
startLink :: Effect (StartLinkResult SupervisorPid)
startLink = do
  Supervisor.startLink (Just $ Local $ atom "example_sup") init

init :: Effect SupervisorSpec
init = do
  connectionString <- BookConfig.connectionString
  webPort <- BookConfig.webPort
  pure
    { flags:
        { strategy: OneForOne
        , intensity: 1
        , period: Seconds 5.0
        }
    , childSpecs:
        (worker "book_web" $ BookWeb.startLink { webPort })
        : (worker "empty_server" $ EmptyGenServer.startLink {})
        : (worker "book_library" $ BookLibrary.startLink { connectionString })
        : (worker "handle_info_example" $ HandleInfoExample.startLink {})
        : (worker "monitor_example" $ MonitorExample.startLink {})
        : nil
    }
worker ::
  forall childProcess.
  HasPid childProcess =>
  String -> Effect (StartLinkResult childProcess) -> ErlChildSpec
worker id start =
  spec
    { id
    , childType: Worker
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
    }

```

You'll note that the server has a name (Local $ atom "example_sup"), calls to startChild/terminateChild/deleteChild can be made with *ByName $ Local $ atom "example_sup"* if we so needed. In the case of simple_one_for_one (below), this server name has the start args and pid type of the children encoded in it so that calls to startChild/etc are typed.

simple_one_for_one:

```haskell
serverName :: RegistryName (OneForOne.SupervisorType OneForOneGenServerStartArgs OneForOneGenPid)
serverName = Local $ atom $ "one_for_one_example"

startLink :: Effect (StartLinkResult (OneForOne.SupervisorPid OneForOneGenServerStartArgs OneForOneGenPid))
startLink = OneForOne.startLink (Just serverName) init

init :: Effect (ChildSpec OneForOneGenServerStartArgs OneForOneGenPid)
init =
  pure { intensity: 100
    , period: Seconds 60.0
    , childType: Worker
    , start: OneForOneGen.startLink
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
  } 

startClient :: OneForOneGenServerStartArgs -> Effect OneForOneGenPid
startClient args = do
  crashIfChildNotStarted <$> Sup.startChild (ByName serverName) args

```

Monitors
--------

With untagged unions, it is entirely possible now to use monitors directly from [erl-kernel](https://github.com/id3as/purescript-erl-kernel), if our process is either single-purpose or is already using untagged unions with say, gun. However, in a lot of cases it still makes sense to use the message router in Pinto for this because we want our code to maintain simplicity with a simple ADT. For this convenience an implementation of Monitor still exists inside Pinto which implements this for us.

For anything that has a Pid, and assuming we're inside something that *HasSelf*, we can call

```haskell
ref <- Monitor.monitor pid ProcessDown
```

where ProcessDown is a constructor that takes a *Pinto.Monitor.MonitorMsg*

```haskell
data MyMsg 
  = Tick
  | ProcessDown Monitor.MonitorMsg
```

A MonitorMsg is presently defined as 

```haskell
type MonitorObject
  = Foreign
type MonitorInfo
  = Foreign
data MonitorType
  = Process
  | Port
data MonitorMsg
  = Down (MR.RouterRef MonitorRef) MonitorType MonitorObject MonitorInfo
```

Which will need expanding should anybody actually need that information. Because we have a router sat in the way, the usual usage of monitor is to pass all of the information we need into the message that will be sent to us and ignore the MonitorMsg entirely

```haskell
ref <- Monitor.monitor pid (const $ ProcessDown pid)
```
where

```haskell
data MyMsg 
  = Tick
  | ProcessDown Pid
```

Pull requests are accepted should somebody want to make the effort to expand those types into reality for some reason.

Timers
------

Timers have also had a slight change in line with the new *HasSelf* typeclass and thoughts around subscriptions, as well as importing the recently ported DateTime libraries so that we have sensible duration types available to us.

So instead of

```haskell
me <- self
liftEffect $ Timer.sendEvery 1000 me Tick
```
We have the far simpler

```haskell
Timer.sendEvery (Milliseconds 1000.0) Tick
```

This will probably be moved to [erl-kernel](https://github.com/id3as/purescript-erl-kernel) in time, because it no longer does anything special and looks exactly like the underlying API.


Summary
-------
Conceptually not a lot has changed and yet we've ended up in a place where we can use the full range of functionalities exposed by GenServers and Supervisors in a type-safe manner, we can chalk that up as a win - especially as it means this [Github issue](https://github.com/id3as/purescript-erl-pinto/issues/2) from March 2019 can finally be closed.

