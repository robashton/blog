A continuation of progress updates on Pinto/Stetson then..

# Previous Purerl posts

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)
- [Shared code twixt Purescript server and client](/entries/shared-code-twixt-purescript-server-and-client.html)
- [Purescript interop with native Erlang, interaction with Redis](/entries/purescript-interop-with-native-erlang---interacting-with-redis.html)

# Updates

- [Nix overlays for Purerl/etc](/entries/updates-to-pinto+stetson---purerl-in-progress.html)
- Typed routing for Stetson
- Emitter based messages for handle\_info in Gen Server
- Monitors for arbitrary pids from Gen servers + Stetson handlers
- WebSocket handlers in Stetson
- Streaming handlers in Stetson
- MessageRouting in Pinto to easily bind to legacy code that sends us messages


# How it was

The initial blast of [Stetson](https://github.com/id3as/purescript-erl-stetson) was thrown up around [Cowboy](https://github.com/ninenines/cowboy) with the express goal of "getting me started on our first client project written ini Purerl. As such it wasn't fancy and routing/dispatch looked like this.

```haskell

Stetson.configure
    # Stetson.route "/api/books" books
    # Stetson.route "/api/books/:isbn" book
    # Stetson.static "/assets/[...]" (PrivDir "demo_ps" "www/assets")
    # Stetson.static "/[...]" (PrivFile "demo_ps" "www/index.html")

```


Where a handler operating over  'id' to get a specific item might look like this

```haskell

book :: StetsonHandler (Maybe Book)
book = 
  Rest.handler (\req -> do
                          let id = binding (atom "isbn") req
                          book <- maybe (pure Nothing) BookLibrary.findByIsbn id
                          Rest.initResult req book)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.PUT : Stetson.DELETE : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.resourceExists (\req state -> 
                             Rest.result (isJust state) 
                             (maybe (setBody "This book does not exist" req) (\_ -> req) state)
                             state)
    # Rest.deleteResource (\req state -> do
                              _ <- maybe (pure unit) (\book -> BookLibrary.delete book.isbn) state
                              Rest.result true req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil) req state)

```


Urgh, so we've got *:isbn* as a binding in our route, which we're pulling out as a *Maybe String* in our init handler, more hand waving here than at a parade. To compound matters, our client is building these urls like so

```haskell

getBook :: String -> Maybe Book
getBook id = fetchJson "/api/books/" <> id

```

This was fine for a year or so, but once you've got a few real applications running on top of this stuff and a great many URLs indeed you start running into issues where typos, incorrect types, etc start rearing their head - especially if you're making changes. (Quiet down at the back if you mention rest, url discovery and client independence to me I'll throw something at you, this isn't that).

Thankfully, we have [nwolverson](http://twitter.com/nwolverson) working with us and he's unafraid of the wonders of [Data.Symbol.SProxy](https://pursuit.purerl.fun/packages/typelevel-prelude/3.0.0/docs/Type.Data.Symbol) and spent a chunk of time re-working [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex) from the client world so that it compiles/works/is-usable in the world of Purerl and Stetson.  Did I ever mention that the great thing about re-purposing an existing language/toolset for Erlang is that code already exists for most things you'd want to do in that language? I'll mention it again here because it's pretty great.

Gone is the hand-waving, for our apps now have a shared module twixt client and server describing the routes available and the types they accept and no mistakes will be accepted by the compiler.

*Our routes*

```haskell

data Route
  = Books
  | Book Isbn
  | Assets (Array String)
  | Index
  | Index2 String (Array String)

```

*How the routes map to paths*

```

-- | This combinator transforms a codec over `String` into one that operates on the `Isbn` type.
isbn :: RouteDuplex' String -> RouteDuplex' Isbn
isbn = asNewtype

apiRoute :: RouteDuplex' Route
apiRoute = path "" $ sum
  { "Books": "api" / "books" / noArgs
  , "Book": "api" / "books" / isbn segment
  , "EventsWs": "api" / "events" / "ws"
  , "EventsFirehose": "api" / "events" / "firehose"
  , "Assets" : "assets" / rest
  , "Index" : noArgs
  , "Index2" : segment / rest
  }


```

Note that our Newtype 'isbn' is an integrated part of both of these APIs, *Book* is a route that is available over *api/books/:isbn* where *:isbn* is of type *Isbn*. No messing around; yes it's just a newtype in this case, but there is nothing stopping us doing more elaborate parsing here into more complicated types. We can't accidentally miss any routes off, those strings are checked against the record at compile type thanks to the magic of SProxy and such.

*On the server*

Rather than try and make this stuff optional in Stetson, we decided to just lump it in as code code - why would you choose strings with more strings and hand waving when you've got types at your disposal? We're not Javascript programmers after all - this means that these routes are accepted as a first class citizen in this world.

```haskell

_ <- Stetson.configure
    # Stetson.routes
      Routes.apiRoute {
          "Book": book
        , "Books": books
        , "EventsWs": eventsWs
        , "EventsFirehose": eventsFirehose
        , "Assets": PrivDir "demo_ps" "www/assets"
        , "Index": PrivFile "demo_ps" "www/index.html"
        , "Index2": (\(_ :: String)  -> PrivFile "demo_ps" "www/index.html")
      }


```

We can see here that once again we are supplying a record with the names from the ADT and these are once again type-checked against that ADT so you can't miss any out or get the types wrong. What types you ask? Well this is the 'book' handler from earlier.


```haskell


book :: Isbn -> StetsonHandler (Maybe Book)
book id =
  Rest.handler (\req -> do
                          book <- BookLibrary.findByIsbn id
                          Rest.initResult req book)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.PUT : Stetson.DELETE : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.resourceExists (\req state ->
                             Rest.result (isJust state)
                             (maybe (setBody "This book does not exist" req) (\_ -> req) state)
                             state)
    # Rest.deleteResource (\req state -> do
                              _ <- maybe (pure unit) (\book -> BookLibrary.delete book.isbn) state
                              Rest.result true req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil) req state)

```

Say what now? Because we define

```haskell

  | Book Isbn

```

in our Routes ADT, we have 


```haskell

  , "Book": "api" / "books" / isbn segment

```

in our RouteDuplex definition, and we have


```haskell

  "Book": book

```

In our Stetson routes, the compiler knows that 'book' needs to be a function that accepts an 'Isbn' and returns a StetsonHandler. So what if I change the type of 'book' to integer? to integer? to integer? to integer?

```haskell
  book :: Int -> StetsonHandler (Maybe Book)
  book id =
```


```
Error found:
in module BookWeb
at src/BookWeb.purs:122:58 - 122:60 (line 122, column 58 - line 122, column 60)

  Could not match type

    Int

  with type

    Isbn

```

hot damn yes, and what if I do a typo in my handlers?

```haskell

      Routes.apiRoute {
          "B00k": book
        , "Books": books
        , "EventsWs": eventsWs

```

Well

```

  Could not match type

    ( "Book" :: t0
    ...
    | t1
    )

  with type

    ( "Assets" :: StaticAssetLocation
    , "B00k" :: Isbn -> StetsonHandler Unit (Maybe ...)

```

You get the picture, by up-front defining the routes and the types they expect as input to the handler functions, we've just done away with the handwaving and given ourselves a pile of safety.

*On the client*

Remember our *getBook :: String -> Maybe Book*? 

```haskell

routeUrl :: Route -> String
routeUrl = RouteDuplex.print apiRoute

getBook :: Isbn -> Maybe Book
getBook id = fetchJson $ routeUrl (Book id)

```

No more strings, no more guessing, thanks Nick!
