All the posts so far..

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)

Useful links

- [demo-ps](https://github.com/id3as/demo-ps) The demo codebase we're talking about here
- [erl-pinto](https://github.com/id3as/purescript-erl-pinto) (the opinionated bindings to OTP we're using)
- [erl-stetson](https://github.com/id3as/purescript-erl-stetson) (the opinionated bindings to Cowbou we're using)

Having now gotten a basic OTP application up and running, it'd be nice to get some data out to the world.

Cowboy
==

Cowboy is the defacto web server in the Erlang world, each route loosely maps to a module which has callbacks defined in it to handle various stages of the decisioning process.

```erlang

-module(my_handler).

-export([init/2,
         get_text/2,
         content_types_provided/2
        ]).

init(Req, _Opts) ->
  { cowboy_rest, Req, #state{} }.

content_types_provided(Req, State) ->
  {[{ <<"text/plain">>, get_text}], Req, State}.

get_json(Req, State) ->
  { <<"Hello World">>, Req, State }.

```

This is directly representable in Purescript, using [erl-cowboy](https://github.com/purerl/purescript-erl-cowboy).


```haskell
module MyHandler where

init :: forall a. InitHandler a a
init = mkEffectFn2 \req c -> pure (initResult c req)

content_types_provided :: forall s. ContentTypesProvidedHandler s
content_types_provided =  mkEffectFn2 \req s -> pure $
  restResult
    (contentTypesProvidedResult $ fromFoldable
      [ tuple2 (ContentType "text" "plain" AnyParams) (ProvideCallback $ atom "asText") ]
    )
    s req


asText :: forall s. EffectFn2 Req s (Tuple3 String Req s)
asText = mkEffectFn2 \req s -> pure $ tuple3 "Hello World" req s

```

However, this doesn't make the best use of Purescript itself - and writing out a few dozen handlers like this would soon get a bit tedious, which is why I went away and wrote [Stetson](https://github.com/id3as/purescript-erl-stetson). Purescript is a functional programming language and it makes sense that rather than provide a bunch of loosely typed callbacks referred to by name using strings, that we built an API that took functions to do all the heavy lifting.


```haskell

helloWorld :: StetsonHandler Unit
helloWorld =
  Rest.handler (\req -> Rest.initResult req unit)
    # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "text/html" asText) req state)
    # Rest.yeeha
    where 
      asText req state = do
        Rest.result "Hello World" req state)

```

The idea of course being that we can configure Stetson/Cowboy at the top level by providing a pile of functions and abstract over the common bits like "This is an accept handler that turns state into JSON because State has the typeclass "WriteForeign", and start to get rid of a lot of duplication across our routes.

```haskell

init :: BookWebStartArgs -> Effect State
init args = do
  Stetson.configure
    # Stetson.route "/api/books" books
    # Stetson.route "/api/books/:isbn" book
    # Stetson.static "/assets/[...]" (PrivDir "demo_ps" "www/assets")
    # Stetson.static "/[...]" (PrivFile "demo_ps" "www/index.html")
    # Stetson.port args.webPort
    # Stetson.bindTo 0 0 0 0
    # Stetson.startClear "http_listener"
  pure $ State {}

```
where books and book are handlers as described above. In our own applications, we have ended up with pretty much the entire web server and all routes in a single file - which is in stark contrast to our usual Erlang apps where we have a folder containing dozens of separate erlang modules.

```haskell
books :: StetsonHandler (List Book)
books =
  Rest.handler (\req -> do
                        state <- BookLibrary.findAll
                        Rest.initResult req state)
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.POST :  Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted (\req state -> Rest.result ((tuple2 "application/json" acceptJson) : nil)
                                req state)
    # Rest.yeeha
    where 
          acceptJson req state = do
            body <- allBody req mempty
            result <- either (pure <<< Left <<< show) BookLibrary.create $ readJSON $ unsafeCoerce body
            case result of
                 Left err -> Rest.result false (setBody err req) state
                 Right c -> Rest.result true req state

jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)

```

So that's a handler that has a state of type *'List Book'*, which it gets from our *BookLibrary* via a call (as in the previous blog entry), jsonWriter being a function as decscribed above - simply taking that model and spitting it out as JSON, leveraging our model which happens to implement that type class.

We'll look more into that in the next entry, where we talk about that model and how we're using it on both client and server.

