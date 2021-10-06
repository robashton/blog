It has been about three years since I first sat in a hotel in Lithuania throwing together the first versions of [purescript-erl-pinto](https://github.com/id3as/purescript-erl-pinto) and [purescript-erl-stetson](https://github.com/id3as/purescript-erl-stetson) so that I could get started on a project for one of our clients.

Quite a lot of code has been written against those projects internally and over time various improvements/patterns have been discovered within core libraries such as [purescript-erl-process](https://github.com/purerl/purescript-erl-process), a heap of Erlang specific packages have been written and released, another mountain of packages have been ported across from Purescript and indeed Pinto and Stetson have been upgraded by various colleagues as our understanding of what we need them to do has evolved.

I have updated the [purerl cookbook](https://purerl-cookbook.readthedocs.io/) for these latest releases but felt it worthwhile highlighting some of the changes in a rare blog post.

Who Am I? A question of 'self'
******************************
An increasing number of *Module.self* functions started showing up in our code as various monads writen around various typed processes wanted to expose the concept of 

```haskell
self :: forall msg. MyCoolMonad msg (Process msg)
```

This inevitably meant we ended up with the wrong 'self' imported at the wrong time, or needing multiple 'selfs'  (GenServer.self, Loop.self, WebSocket.self, etc) in a single module which... got irritating at best and confusing at worst.

We now have the following typeclasses available to us in the *purescript-erl-process* package.

```haskell

class HasProcess b a where
  getProcess :: a -> Process b

class HasPid a where
  getPid :: a -> Pid

class HasSelf (x :: Type -> Type) a | x -> a where
  self :: x (Process a)

```

The first two are not particularly hard to understand, some types will be able to give us pids or processes if we have an instance of them already.

For example any *Process msg* will clearly have an untyped pid underlying it, a Process simply being a newtype around Pid with a phantom message type.

```haskell

instance processHasPid :: Raw.HasPid (Process b) where
  getPid (Process pid) = pid

```

And funnily enough Pids also have Pids


```haskell
instance pidHasPid :: HasPid Pid where
  getPid = identity
```

This makes it possible to write a function around the general concept of 'something that has a pid' without having to go through contortions to get hold of a pid from what can often be quite the stack of newtypes. 

```haskell
subscribe :: forall HasPid a => a -> Effect Unit
```

This is less useful than its *HasProcess msg* counterpart, which expresses not only that a process can be gotten hold of, but that it can only receive certain types of message.

```haskell
subscribe :: forall HasProcess msg process => process -> Effect Unit
```

These are all very well and good for the cases where the code already has hold of some sort of reference to a running process via *Process.spawnLink*, *GenServer.startLink* or similar, but very often the code being written is being executed *inside* one of these processes and the common pattern in Erlang is the trustworthy call to 'self' as mentioned at the start of this section.

```erlang
Self = self(),
api:subscribe(Self)
```

With the addition of HasSelf to most of the common process containers across Erl.Process, Stetson, and Pinto, this becomes a case of the following - regardless of what the process container is being used in that case.

```haskell
subscribe =<< self
```

*self* being a typed pid of `Process msg`, and subscribe being be a call that takes a `Process msg`, it becomes hard to subscribe to messages that the code cannnot actually receive in whatever *handleInfo*, *wsInfo*, *loopInfo* callback or call to *receive* that whichever process container being used exposes for that purpose.

Most subscribe calls don't actually look like this - as it's more convenient in most cases to accept a callback that  allows the lifting of messages into a different type more convenient foo the subscriber.

```haskell
subscribe :: forall msg. (msg -> Effect Unit) -> Effect Unit
subscribe cb = ...
```

Thus, a call to a subscribe function will very often look like this


```haskell
me <- self
subscribe $ send me <<< Msg
```

Which reads very well indeed. 

While this *is* the predominant pattern across existing code, this subscribe call *is* taking an *(msg -> Effect Unit)* callback. This isn't ideal because it means the user can execute arbitary code in the API's process - that's a pretty obnoxious (and dangerous) thing to allow but with the typeclasses that are now available, it is actually possible to write a function that subscribes whichever process we're in to a set of messages, given some means of lifting the message type into the appropriate container and therefore limiting the damage that can be done.

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

And thus, our call to this becomes

```haskell
saferSubscribe Msg
```

And if we really need to be able to provide a process, instead of relying on *HasSelf* (for example, sending messages to a child process)


```haskell
saferSubscribeTo ::
  forall msg.
  Process msg -> (Msg -> msg) -> Effect Unit
saferSubscribeTo p f = do subscribe $ send p <<< f
```

There is a chance that this may end up being formalised at some point.


Erl.Process Upgrades
********************

Stetson Upgrades
****************

Pinto Upgrades
**************

New Erlang Packages
*******************

Ported Purescript Packages
**************************


