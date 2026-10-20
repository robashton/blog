All the posts so far..

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)

Useful links

- [demo-ps](https://github.com/id3as/demo-ps) The demo codebase we're talking about here
- [erl-pinto](https://github.com/id3as/purescript-erl-pinto) (the opinionated bindings to OTP we're using)
- [erl-stetson](https://github.com/id3as/purescript-erl-stetson) (the opinionated bindings to Cowboy we're using)

We've got a basic model representing our 'book'

```haskell

module Books where

type Book = { isbn :: String
            , title :: String
            , author :: String
            }

```

Which is pretty terribly exciting - the key thing to note here is that this is a plain ol' record containing primitive types - this makes it pretty shareable as far as view models go (Let's ignore that we're using it for persistence here, demo code gonna demo code).

We've shoved Books.purs into a folder called 'shared' which has been softlinked from both the client and server directories, which means it will be separately compiled into JS And Purescript respectively.

There is a great library over in Purescript world called [simple-json](https://github.com/justinwoo/purescript-simple-json) which defines a *ReadForeign* and *WriteForeign* for these basic types, which means that the functions readJSON and writeJSON will do the back and forth between JSON and the Purescript types.

There is also (thanks to the efforts of [@nwolverson](https://github.com/purerl/purescript-simple-json), a port of this library exists on the Purerl side of things which works in exactly the same way, using [JSX](https://github.com/talentdeficit/jsx) under the hood. This is a fairly common pattern across the Purerl world, and you'll see when browsing the org repos that a lot of the code from the JS Purescript world has been ported across with minimal changes so that the two worlds look as alike as possible.

So, in the previous post we loaded a list of Books out of our genserver in Cowboy and simply called writeJSON on it.

```haskell

# Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)

jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)

```

If we open up the client code, we can see

```haskell

maybeBook <- H.liftAff $ loadItem $ "/api/books/" <> isbn

-- where

loadItem :: forall a. ReadForeign a => String -> Aff (Either String a)
loadItem uri = do
  response <- AX.get AXResponse.string uri
  case response.body of
     Left err -> pure $ Left "No"
     Right json -> pure $ bimap show identity $ readJSON json

```

That is all that is required to shift data between the server Purescript and the client Purescript which is pretty tidy indeed! Because we're using the same library, any custom implementations of *ReadForeign* and *WriteForeign* we might choose to write can then also be shared between the two.

Obviously sharing code between both client and server is something that should be entered into with caution, typically sharing a lot of business logic is the sign of a problem - but I can easily see us ending up in a world where we can start using Halogen on the server-side to pre-render HTML for serving to the client much in the same way that the ReactJS community have started doing similar with NodeJS.

Next up
==

We'll look at our bindings to Redis, to get a feel for what interop with "native" Erlang with FFI looks like.
