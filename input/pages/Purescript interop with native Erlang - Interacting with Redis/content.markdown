All the posts so far..

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)
- [Shared code twixt Purescript server and client](/entries/shared-code-twixt-purescript-server-and-client.html)

Useful links

- [demo-ps](https://github.com/id3as/demo-ps) The demo codebase we're talking about here
- [erl-pinto](https://github.com/id3as/purescript-erl-pinto) (the opinionated bindings to OTP we're using)
- [erl-stetson](https://github.com/id3as/purescript-erl-stetson) (the opinionated bindings to Cowboy we're using)

We've handwaved over the contents of the [Redis](https://github.com/id3as/demo-ps/blob/master/server/src/Native/Redis.purs) module so far in the demo app, showing only that we can use it from our [BookLibrary.purs](https://github.com/id3as/demo-ps/blob/master/server/src/BookLibrary.purs) like any other module as follows.

```haskell

update :: Book -> Effect (Either String Book)
update book =
  Gen.doCall serverName \state@{ connection } -> do
    Redis.put (dbId book.isbn) book connection
    pure $ CallReply (Right book) state

```

That's pretty tidy, so let's look at the type of that function so we understand the Purescript side of things first and foremost

```haskell

put :: forall a. WriteForeign a => DbId -> a -> RedisConnection -> Effect Unit

```

For any type 'a' that implements WriteForeign, we're a function that takes a DbId, an A and a RedisConnection to produce an Effect of type Unit - this pretty much makes sense on the surface, DbId is just a newtype around String, and given a Key and a Value (which we can get from calling writeJSON on our type 'a'), we can dump stuff in Redis - but how?

Let's take a step back a moment and approach this from the *other* end.

There is an application for accessing Redis in Erlang, called [eredis](https://github.com/wooga/eredis) imaginatively enough, the usage of which looks a little like this.

```erlang

   { ok, C } = eredis:start_link(ConnectionString),

   eredis:q(C, [ <<"SET">>, <<"key">>, <<"value">> ]).

```

So what we want to do is surface this meaningfully to Purescript is define some foreign imports in our *Redis.purs*, which map onto their native counterparts in our *Redis.erl.*

```haskell

foreign import data ConnectionString :: Type
foreign import data RedisConnection :: Type
foreign import open :: ConnectionString -> Effect RedisConnection

```

By importing a foreign type, we're saying to Purescript "Hey, this thing exists in Erlang but we don't know what is inside it, but we want to model it as something we can pass around thanks", the ConnectionString comes from sys.config and the RedisConnection is actually a Pid but we don't need to know that.

By importing a foreign function, we're saying that there is a function in Erlang with this name and signature and we'd like to call it from Purescript. The "open" function we're importing from Erlang takes one of those ConnectionStrings and produces an Effect of type RedisConnection.

```erlang

open(ConnectionString) ->
  fun() ->
      { ok, C } = eredis:start_link(ConnectionString),
      C
  end.

```

We wrap our actual functionality in a function, because that's how an Effect is modelled in Purerl, and we can return our 'Connection' as an opaque type to Purescript when this Effect is processed. I've made the decision here to match directly on *{ ok, C }* which means we'll crash at runtime if we can't open a connection - an alternative API could look like this.

```erlang

open_(ConnectionString, Just, Nothing) ->
  fun() ->
      { ok, C } = case eredis:start_link(ConnectionString) of
                    { ok, C } -> Just(C);
                    _ -> Nothing
                    end
  end.

```

where

```haskell

foreign import open_ :: ConnectionString -> (RedisConnection -> Maybe RedisConnection) -> Maybe RedisConnection  -> Effect (Maybe RedisConnection)

open :: ConnectionString -> Effect RedisConnection
open connectionString = open_ connectionString Just Nothing

```

I've chosen to "let it crash", as that fits with how I'd build the supervsion tree in Erlang around this (Restart periodically every 30s until the connection works please), but either approach is valid and not one I have strong opinions on at this time. (an Either with the failure reason would also be an option).

Anyway, going back to that *put* function with all of this in mind, we want to keep the Erlang pretty minimal, so some transformation on the Purescript side will be required. 

```haskell

foreign import put_ :: DbId -> String -> RedisConnection -> Effect Unit

put :: forall a. WriteForeign a => DbId -> a -> RedisConnection -> Effect Unit
put id obj conn =
  put_ id (writeJSON obj) conn

```

Purescript knows how to turn our obj into JSON so we'll do that there, and then use a foreign function that takes just the primitive objects for use within Erlang with eredis.


```erlang

-define(SET(Key, Value), [ <<"SET">>, Key, Value ]).

put_(Id, Data, Pid) ->
  fun() ->
      { ok, <<"OK">>} = eredis:q(Pid, ?SET(Id, Data)),
      ok
  end.

```

Now, this is not necessarily how we should approach building an FFI for a native library, as we're making some decisions in Erlang that could be made in Purescript, a more traditional FFI would look like this.


```erlang

q(Pid, Operation) ->
  eredis:q(Pid, Operation).

```

```haskell

foreign import q :: RedisConnection -> List String -> Effect (Tuple2 Atom Binary)

```

And then we could build the API we *actually* want on top of *that* instead - I've rather skipped that step in my demo for clarity, but if I was producing a library wrapper for publication that's probably the approach I'd take, as unwieldy as it is to actually use it means we end up writing more Purescript and less Erlang.

This is the approach that erl-cowboy and Stetson took, whereas erl-pinto takes the direct approach to "desired API", making the concession that it'll be easier to build without 1-1 FFI in existence. Either way, it's pretty easy to call into existing code in Erlang.

Next up
==

We've pretty much covered the surface area of the demo project, so I'll be going off on a little journey talking about some of the common things that we like to do in Erlang and how to get that behaviour into Purerl, if I get any questions about these posts I'll follow up with answers in blog format also.
