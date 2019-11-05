Just a note that I'll be appearing at [Codemesh](https://www.codemesh.io/conferences/code-mesh-ldn/) on Thursday (11:25) to give a talk where I go over some of the stuff we've been working on at [Work](https://www.id3as.com/home)(tm) this year - with a focus on some of the implementation details and nitty gritty/etc.

I silently blogged some of this recently, basically showing off how we've now got the ability to write OTP applications in Purescript these days, and indeed are. The posts are all linked below for the majority of folk who are sensible enough not to not subscribe to this sorry excuse for a blog.

Here is a sneaky peek as to what a gen server looks like in this world, tasty no?

```haskell

findByIsbn :: String -> Effect (Maybe Book)
findByIsbn isbn = 
  Gen.doCall serverName \state@{ connection } -> do
    result <- Redis.get (dbId isbn) connection
    pure $ CallReply result state

findAll :: Effect (List Book)
findAll = 
  Gen.doCall serverName \state@{ connection } -> do
    books <- Redis.findAll dbPrefix connection
    pure $ CallReply books state

-- Nothing special about this, just a function that returns a certain type
-- We can supply arbitrary arguments to this via the gensup
startLink :: BookLibraryStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

-- And those arguments can then end up in here, which just needs to return an effect of our State type
init :: BookLibraryStartArgs -> Effect State
init args = do
  connection <- Redis.open args.connectionString
  pure $ { connection }

```

The blog entries
==

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)
- [Shared code twixt Purescript server and client](/entries/shared-code-twixt-purescript-server-and-client.html)
- [Purescript interop with native Erlang, interaction with Redis](/entries/purescript-interop-with-native-erlang---interacting-with-redis.html)

Useful links
==

- [demo-ps](https://github.com/id3as/demo-ps) The demo codebase we're talking about here
- [erl-pinto](https://github.com/id3as/purescript-erl-pinto) (the opinionated bindings to OTP we're using)
- [erl-stetson](https://github.com/id3as/purescript-erl-stetson) (the opinionated bindings to Cowboy we're using)


