Two blog posts in a week? Unheard of - following on from Tuesday's blockbuster smash hit epic "[Purerl Updates - Processes and Self](/entries/purerl-updates---processes-and-self.html)", today we have an entry describing our current thoughts on subscriptions in a Purerl world.

A standard procedure in Erlang, is to invoke an API that then proceeds to send messages back to the invoking process.

```erlang
some_api:subscribe()

receive 
  Msg -> ..
```

More often or not, an overload will be provided allowing a Pid to be passed in, in case you didn't want it sending to *self*.

```erlang
Pid = spawn_worker(),
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
