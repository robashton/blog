It has been about three years since I first sat in a hotel in Lithuania throwing together the first versions of [purescript-erl-pinto](https://github.com/id3as/purescript-erl-pinto) and [purescript-erl-stetson](https://github.com/id3as/purescript-erl-stetson) so that I could get started on a project for one of our clients.

Quite a lot of code has been written against those projects internally and over time various improvements/patterns have been discovered with core libraries such as [purecript-erl-process](https://github.com/purerl/purescript-erl-process), a heap of Erlang specific packages have been written and released, another mountain of packages have been ported across from Purescript and indeed Pinto and Stetson have been upgraded as our understanding has evolved.

I have updated the [purerl cookbook](https://purerl-cookbook.readthedocs.io/) for these latest releases but felt it worthwhile highlighting some of the changes in a rare blog post.

Who Am I? A question of 'self'
******************************

An increasing number of Module.self functions started showing up in our code as various contexts wanted to expose the concept of 

```haskell

self :: forall msg. Effect (Process msg)

```

This inevitably meant we ended up with the wrong 'self' imported at the wrong time, or needing multiple 'selfs' in a single module which... got confusing.

We've now got the following typeclasses available to us in the *purescript-erl-process* package.

```haskell

class HasProcess b a where
  getProcess :: a -> Process b

class HasPid a where
  getPid :: a -> Pid

class HasSelf (x :: Type -> Type) a | x -> a where
  self :: x (Process a)

```

The first two are not particularly hard to understand, some types will be able to give us pids or processes if we have an instance.

For example any *Process msg* will clearly have an untyped pid underlying it.

```haskell

instance processHasPid :: Raw.HasPid (Process b) where
  getPid (Process pid) = pid

```










Stetson Upgrades
****************

Pinto Upgrades
**************

New Erlang Packages
*******************

Ported Purescript Packages
**************************


