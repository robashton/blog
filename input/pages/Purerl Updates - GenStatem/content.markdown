Seems we've ended up with a small handful of posts about the latest Purerl updates! Great - that's about one for every reader, let's get cracking!


- [Purerl Updates - Processes and Self](/entries/purerl-updates---processes-and-self.html)
- [Purerl Updates - Subscriptions](/entries/purerl-updates---subscriptions.html)
- [Purerl Updates - Untagged Unions](/entries/purerl-updates---untagged-unions.html)
- [Purerl Updates - Maxing out Pinto OTP](/entries/purerl-updates---maxing-out-pinto-otp.html)

One of the big motivations for the big push on [purescript-erl-pinto](https://github.com/id3as/purescript-erl-pinto) was to facillitate the addition of other OTP behaviours than GenServer.

One of our "go-to" servers is [gen_statem](https://erlang.org/doc/man/gen_statem.html). When writing any sort of implementation of a stateful network protocol for example, it is nice to model various steps of the protocol as 'states' on which only certain types of message can be enacted. A gen_statem has a list of these possible states (as an atom in this case), as well as 'Data' which is an arbitrary term.

The simplest example outside of this however, is the locked door. A door which is locked cannot be opened, and a door that is opened cannot be locked. If the door is open we can take the treasure behind the door and to unlock the door we need a key. This could be modelled with three states here, an

- Locked
- Unlocked
- Open

Some of a plain ol' gen_statem to represent this would look like this.

```erlang
init([]) ->
    {ok, locked , #{ key => <<"open sesame">>, treasure => <<"The moon is made of cheese">>}}.
    
handle_event({call,From}, { unlock, K1 }, locked, #{ key := K2, treasure := Secret }) 
  when K1 =:= K2 ->
  {next_state, unlocked, #{ treasure => Secret }, [{reply, From, ok}]};

handle_event({call,From}, { unlock, _K1 }, locked, Data) 
  {keep_state, Data, [{reply, From, bad_key}]};

handle_event({call,From}, { unlock, _K1 }, unlocked, Data) 
  {keep_state, Data, [{reply, From, ok}]};

handle_event({call,From}, open, unlocked, Data) 
  {next_state, open, Data, [{reply, From, ok}]};

```
And so on and so forth. 

A locked door has a key and a treasure, a call to 'unlock' with the right key will give you an open door with just the 'treasure', a call to unlock with the wrong key will result in an error, a call to unlock when already unlocked is just fine thank-you and a door that is unlocked can be opened. A few things of note that we're not a *huge* fan of here. 

- There is nothing to tie the current 'state' with the 'data', thus there is nothing stopping a locked door from not having a key (oh dear!)
- There are a *lot* of tuples with arbitrary atoms floating around and the possibility for typos is endless
- States aren't defined up front, any arbitrary atom can be returned and we'll now be in that state

We could be generous and say that two of these are actually a feature and not a bug, but beauty is in the eye of the beholder and if you're a fan of type systems then this has all the beauty of my macaroni art from pre-school.

From a Purescript perspective, it'd be nice if we could tie the current state with "state specific data" using an ADT, as well as have some state that is common to all.

```haskell
data State 
  = Locked { key :: String, treasure :: String }
  | Unlocked { treasure :: String }
  | Open

```

```haskell
startLink :: Effect (StartLinkResult DoorLockPid)
startLink = do
  GenStatem.startLink $ GenStatem.defaultSpec init
  where
    init = pure $ InitOk (Locked { key: "open sesame", treasure: "the moon is made of cheese" }) {}

````

Now, the only problem with our new *State* is that the extra data bundled with it is means that it can't be used directly as a 'state' in gen_statem because each variant would be a new state!

A typeclass provided therefore be able to extract a 'stateid' to pass to the underlying gen_statem implementation

```haskell
data StateId
  = StateIdLocked
  | StateIdUnlocked
  | StateIdOpen

derive instance eqStateId :: Eq StateId

instance stateHasStateId :: HasStateId StateId State where
  getStateId (Locked _) = StateIdLocked
  getStateId (Unlocked _) = StateIdUnlocked
  getStateId (Open _) = StateIdOpen
```

A default implementation could exist of course to simply extract an atom from the underlying representation, building that is left as an exercise for the reader..

Anyway, just like with GenServer and Supervisor, we can represent a GenStatem with a Pid and a Type in order to communicate with it and build up various callbacks and such. The type specs are quite daunting beause GenStatem has quite a few different 'message types' - most of thes can be ignored and set to Void/Unit until needed.

```haskell
newtype StatemType :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype StatemType info internal timerName timerContent commonData stateId state
  = StatemType Void

newtype StatemPid :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype StatemPid info internal timerName timerContent commonData stateId state
  = StatemPid (Process info)
```

Our very simple DoorLock Genstatem which doesn't have any messages outside of direct 'calls' could be defined as 

```haskell
type DoorlockType = StatemType Unit Unit Unit Unit {} StateId State
type DoorlockPid = StatemPid Unit Unit Unit Unit {} StateId State
```

With these types defined, a semi-complete implementation of our door-lock GenStatem could therefore look like

```haskell
serverName :: RegistryName DoorlockType
serverName = Local $ atom "doorlock"

startLink :: Effect (StartLinkResult DoorlockPid)
startLink = 
  Statem.startLink $ Statem.defaultSpec init { name = Just serverName } 

data UnlockResult = Unlocked | BadCode 

unlock :: String -> Effect UnlockResult
unlock k1 = GenStatem.call (ByName serverName) 
  \from (Locked { key: k2 } _ = 
    if k2 == k1 then do
      let actions = GenStatem.newActions # GenStatem.addReply (GenStatem.reply from Unlocked)
      pure EventNextStateWithActions (Unlocked { treasure }) {} actions
    else do
      let actions = GenStatem.newActions # GenStatem.addReply (GenStatem.reply from BadCode)
      pure $ EventKeepStateAndDataWithActions actions

```

The GenStatem functionality in Pinto is being used in a few places in our codebases now, but it is not complete. There are some hand-wavey bits around timers and messaging that will get fixed when somebody needs it, but the pattern and general types are pretty much now in place. Various callbacks can be supplied by overriding the default spec, those callbacks will receive certain kinds of messages that will need adding to the type signatures and various return values can be built up by using the functions provided in the GenStatem module.

There are other kinds of Genserver available that could do with representations in Pinto and as we saw in the last entry, now we have a generic way of representing 'pids' and 'types' that can supply and enforce their own internal messages, it should be possible to do this without making any substantial changes to pinto itself.

A note on Pinto's future
-----

Pinto is now 'conceptually' pretty complete and hopefully shouldn't undergo any more major changes. The types that have been added can be used to describe pretty much any running proces and various functionalities that can be exposed to interact with those proceses - from the simple "This process has a state and can receive messages" to "This process has a state, can receive this type of video stream and this type of audio stream, send these messages on a timer..." and then be stuck into Supervisors in the same way as any other server.

There are probably better ways of representing this in Purescript by deviating further from OTP itself and re-imagining a more Purescript central means of modelling these things. Eventually somebody is going to take the plunge and do this and it probably won't be us. Should this happen, I don't think it'll take long to move away from Pinto and to it - so if anybody fancies the task...


