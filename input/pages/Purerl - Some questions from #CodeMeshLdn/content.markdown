Talk at CodeMesh now given, because of a demo god intervention (I discovered a Purerl compiler bug mid-demo, how's that for luck?) I didn't have time to receive questions during my actual session, but I kept a note of questions asked afterwards and will cover them here for those not present during their discussion.

I'll carry on linking these Purerl posts from each other before that however, 'cos it serves as a handy indexing mechanism in lieu of having a real blog engine..

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)
- [Shared code twixt Purescript server and client](/entries/shared-code-twixt-purescript-server-and-client.html)
- [Purescript interop with native Erlang, interaction with Redis](/entries/purescript-interop-with-native-erlang---interacting-with-redis.html)
- [Codemesh 2019 - Purerl and OTP](/entries/codemesh-2019---purerl-and-otp-talk.html)

Useful links
==

- [demo-ps](https://github.com/id3as/demo-ps) The demo codebase we're talking about here
- [erl-pinto](https://github.com/id3as/purescript-erl-pinto) (the opinionated bindings to OTP we're using)
- [erl-stetson](https://github.com/id3as/purescript-erl-stetson) (the opinionated bindings to Cowboy we're using)


Some questions then.
==

- What if I use the wrong types in my FFI
- What if I don't use Effect in my FFI
- How do you map complex union types back into Purerl when doing FFI (specifically, errors)
- Are there any situations where you feel the types in Purescript would get in the way and you'd just write Erlang instead?

Wrong types
==

In one demo I did a very simple import of the base64 module from base Erlang:

```erlang
-module(cool@foreign).

-export([base64/1]).

base64(String) -> base64:encode(String).

```

Consumed in purescript with the following import

```haskell

foreign import base64 :: String -> String

```

And all is right and proper, from the erlang shell, I can call this function

```bash
Eshell V10.5  (abort with ^G)
1> l(cool@ps).
{module,cool@ps}
2> cool@ps:base64(<<"foo">>).
<<"Zm9v">>
3>
```
So.. what if we lie about the types at play here?

```haskell

foreign import base64 :: String -> Int

```
Well, the result of calling the purescript function from Erlang isn't going to change because it doesn't care about types

```bash
Eshell V10.5  (abort with ^G)
1> l(cool@ps).
{module,cool@ps}
2> cool@ps:base64(<<"foo">>).
<<"Zm9v">>
3>
```

How about using it in a purescript application?

```haskell

doSomething :: Effect Unit
doSomething = do
  let result = base64 "wow"
  _ <- Console.log $ show result
  pure unit

```

```bash

> (cool@ps:doSomething())().
** exception error: bad argument
     in function  integer_to_binary/1
        called as integer_to_binary(<<"d293">>)
     in call from data_show@foreign:showIntImpl/1 (output/Data.Show/data_show@foreign.erl, line 4)
     in call from main@ps:'-doSomething/0-fun-0-'/1 (/home/robashton/talks/purescript_beam/demo_2/src/Main.purs, line 19)

```
Passing the variable around in Purescript land, nothing cares - it is assumed that if you've said something is a certain type, that it is that type and all type checking will be done on that basis - sooner or later, every bit of data will end up getting somewhere where it needs to be serialized whether for display, storage or transmission and that will involve passing the data into code that makes assumptions based on the type being passed in and you'll get a runtime crash.

So obviously the answer is "don't do it" - when building FFI, getting types correct is essential - definitely something worth testing.

What if I don't use Effect in my FFI
==

You maniac, why would you want to lie about side effects??! Well it turns out to not actually be a big deal, the main reason you're telling the type system about side effects is for your own benefit and if you choose to lie then at some point you will be hoisted on your own petard and we will all laugh at you. An example of places where this could happen is with legacy code and logging, I guess, probably, maybe.

```erlang

-module(cool@foreign).
-export([ add/2 ]).

add(X,Y) ->
  io:format(user, "WOW, I AM ADDING ~p and ~p", [ X, Y ]),
  X + Y.

```

```haskell

foreign import add :: Int -> Int -> Int

doSomething :: Effect Unit
doSomething = do
  let result = add 5 6
  _ <- Console.log $ show result
  pure unit

```

```bash
8> (main@ps:doSomething())().
WOW, I AM ADDING 5 and 611
unit
```

Is this the future you want to be a part of? No I didn't think so - friends don't let friends write effectful code without declaring it as effectful.


How do you map complex union types back into Purerl 
==

Consider the [read\_dir API](http://erlang.org/documentation/doc-6.2/lib/kernel-3.0.3/doc/html/file.html#list_dir-1) in classic Erlang: 

```erlang
  list_dir(Dir) -> {ok, Filenames} | {error, Reason}

  Types:

  Dir = name_all()
  Filenames = [filename()]
  Reason = posix()
         | badarg
         | {no_translation, Filename :: unicode:latin1_binary()}

```

There are some decisions to be made when writing code that exposes this API to the Purerl world, and none of them are *wrong* per se. The default position is usually "Map the exact API across as it is, and if you don't like it then fix it by writing a somewhat more Purescripty wrapper".

So that would be.

```haskell

module Erl.File where

ListDir  :: String -> Effect (Either ListDirFailure (List String))

data ListDirFailure = ListDirPosixFailure Atom
                    | ListDirBadArg
                    | ListDirNoTranslation Binary

```


Which means then passing in the constructors to the FFI so this can be constructed in Erlang without knowing about these types at the top level.

```haskell

foreign import ListDir_ :: (Atom -> ListDirFailure)  
                            -> ListDirFailure 
                            -> (Binary -> ListDirFailure) 
                            -> ListDirFailure -> (Either ListDirFailure (List String))
                            -> (List String -> (Either ListDirFailure (List String)))
                            -> String -> Effect (Either ListDirFailure (List String))

ListDir  :: String -> Effect (Either ListDirFailure (List String))
listDir = listDir_ ListDirPosixFailure ListDirBadArg ListDirNoTranslation Left Right

data ListDirFailure = ListDirPosixFailure Atom
                    | ListDirBadArg
                    | ListDirNoTranslation Binary

```

Youch that's a mouthful, obviously we can make that a bit more legible with

```haskell

foreign import ListDir_ :: (Atom -> ListDirFailure)  
                            -> ListDirFailure 
                            -> (Binary -> ListDirFailure) 
                            -> ListDirFailure -> ListDirResult
                            -> (List String -> ListDirResult
                            -> String -> Effect ListDirResult

ListDir  :: String -> Effect (Either ListDirFailure (List String))
ListDir = listDir_ ListDirPosixFailure ListDirBadArg ListDirNoTranslation Left Right

type ListDirResult = Either ListDirResult (List String)
data ListDirFailure = ListDirPosixFailure Atom
                    | ListDirBadArg
                    | ListDirNoTranslation Binary

```

And the FFI then looks like

```erlang

listDir_(ListDirPosixFailure, ListDirBadArg, NoTranslationError, Failure, Success, Dir) ->
  fun() ->
    case file:list_dir(Dir) of
      { ok, Filenames } -> Success(Filenames);
      { error, { no_translation, Encoded } } -> Failure(NoTranslationError(Encoded));
      { error, badarg } -> Failure(ListDirBadArg);
      { error, Posix } -> Failure(ListDirPosixFailure(Posix));
  end.

```

If you wanted a nicer API on top of this, let's call it "CoolFileApi" for example, you'd then wrap *that* binding with something nicer - having at least type-safe-ified the original API in all of its glory, and then you're doing the work in Purescript rather than Erlang, I haven't ran this through a compiler so I probably got it a bit wrong but you'll get picture..

```haskell
module CoolFileApi where

import Erl.File as File
import Data.Newtype (unwrap, wrap)

newtype Filepath = Filepath String
derive instance ntFilepath :: Newtype Filepath _

ListDir :: Filepath -> Effect (Either CoolListDirFailureReason (List Filepath))
ListDir Filepath = do
  result <- File.ListDir $ unwrap Filepath
  pure $ either (Left <<< nativeFailureToShinyFailure) (Right <<< wrap)


nativeFailureToShinyFailure :: File.ListDirFailure -> CoolListDirFailureReason
nativeFailureToShinyFailure reason =
  case reason of
    ListDirBadArg -> CoolListDirBadArg
    ListDirNoTranslation filename -> CoolListDirNoTranslation filename
    ListDirPosixFailure a -> CoolListDirPosixFailure (atomToCool a)

atomToCool  :: Erl.Atom -> CoolListDirPosixFailure
atomToCool a = 
  case a of
    (atom "eacces") -> Eaccess
    (atom "enoent") -> Enoent
    --- etc

```

Essentially getting rid of those arbitary atoms and Erlang data types and representing them as native Purerl data types. You *could* just write your binding directly in this way from the get-go, but it's considered polite to write the low level bindings as directly as possible to the original API because it saves on mental load (and doing as little work as possible in the Erlang code), the documentation for the original API is then applicable to the low level bindings too.


Aren't types a huge pain in the ass?
==

I mean that's what I heard for this question, and it's a bit of a loaded one - people that aren't used to working in a sensibly typed environment tend to look at the typed environment as being a form of burden, instead of an aid to help you write better code. Some of the conversation around this was about whether we'd be re-writing some of our core functionality in Purescript, and whether that would even be desirable as the types might get in the way of productivity.

I think that actually there is a lot of desire internally for us to do our next re-write of our orchestration logic in Purerl for example, but because  we're in a different language with a different way of looking at the world, the API and even the design of that would probably not look anything like the original (and the same goes for our media workflow engine too). We're not afraid of re-writes, we write code  to be thrown away after all - but usually when doing a re-write of a module, you get to learn from the previous iterations and Do It Better this time - the pendulum of compensation from past mistakes slowly zeroes in around an ideal design and by version 3 or 4 you're laughing.

The main pain of having to do things differently is while there will be some lessons that can be passed into the rewrites into Purescript, a lot of the design decisions don't come through to the new world and you're essentially going back to version 1/2 again for that component. That'll be the pain that we'll go through but we'll come out the other side as better Purescript developers so we're not afraid of that.

I really can't see a point where I'd say that types are going to get in the way of Just Getting Stuff Done, in my experience so far, the type system is just a more honest way of looking at effort, when you're working in a sloppy-typed environment you can pretend that it's really easy and fun and you're dead smart so it's okay, but you end up paying the cost in bugs and mistakes eventually - the upfront cost might not be there but you've just shifted it elsewhere. Types also typically enable you to write better abstractions to lessen the load of repeated operations - you can be a lot more clever when the compiler has your back.

It's very hard to currently view what a typed version of some of our core IP would look like, but that's down to our inexperience rather than it being an impossible task; it'll come in time.


Anyway
==

That's some answers, if you have more questions about Purerl/Stetson/Pinto [hit me up on Twitter](http://twitter.com/robashton) and I'll do my best to help..

