Previous entries..

- [Intro](/entries/functional-erlang---purescript-on-the-beam---intro.html)
- [Getting Started](/entries/purescript-on-the-beam:-getting-started.html)
- [Writing some basic code](/entries/purescript-on-the-beam---writing-some-basic-code.html)
- [Basic interop with Erlang](/entries/purescript-on-the-beam---basic-interop-with-erlang.html)
- [Calling Purecript from Erlang](/entries/purescript-on-the-beam---otp-from-scratch.html)

Armed with the tools from the last explorations, it should be relatively trivial to start interacting with OTP in Purescript, and we'll give that a bash directly in this entry to see how well *that* goes..

# Writing a Gen Server in Purescript

Ignoring applications and supervisors for a moment, we can probably write a genserver fairly trivially in Purescript directly using an FFI to call into OTP and shuffle some data types

The low level FFI could look a little like this, where the call is just an effect that results in a genserver starting.

```erlang

    -module(genServer@foreign).

    -export([startLinkImpl/3]).

    startLinkImpl(ServerName, Module, Args) ->
      fun() ->
        gen_server:start_link(ServerName, Module, Args, [])
    end.

```

```haskell

    module GenServer where

    import Prelude
    import Erl.Atom
    import Erl.Data.List
    import Erl.Data.Tuple
    import Effect.Uncurried (mkEffectFn1, EffectFn1)
    import Effect

    foreign import data StartLinkResult :: Type

    foreign import startLinkImpl :: forall args. (Tuple2 Atom Atom) -> Atom -> args -> Effect StartLinkResult

    startLink :: forall args. (Tuple2 Atom Atom) -> Atom -> EffectFn1 args StartLinkResult
    startLink serverName mod =
      mkEffectFn1 \args -> startLinkImpl serverName mod args

```

Note the mkEffectFn1 allowing us to pass this effectful function into Erlang code, and the parameterised argument type allowing us to have custom arguments for the gen server we're writing.

Allowing us to write a gen server that looks like this:


```haskell

    module TestServer where

    import Prelude
    import Erl.Atom
    import Erl.Data.List
    import Erl.Data.Tuple
    import GenServer as GenServer
    import Effect.Uncurried (mkEffectFn1, EffectFn1)
    import Effect.Console (log)

    newtype State = State {}

    startLink :: EffectFn1 String GenServer.StartLinkResult  
    startLink = GenServer.startLink (tuple2 (atom "local") (atom "testServer")) (atom "testServer@ps")

    init :: EffectFn1 String (Tuple2 Atom State)
    init = mkEffectFn1 \args ->  do
      _ <- log $ "Gen server started with args: " <> args
      pure $ tuple2 (atom "ok") (State {})

```

In this case, we've decided our start args are a string and we'll just log that out on startup, and we return a newtype with a record containing our gen server state from the init function, and of course we can just plug this into a standard Erlang supervision tree and we'll end up with a gen server running which if sent any messages will simply crash :).


```erlang

    init([]) ->
        {ok, { {one_for_all, 0, 1}, [ #{ start => { testServer@ps, startLink, [<<"Your args">>] },
                                         type => worker,
                                         id => test_server
                                       }
                                    ]} }.

```

Already we can see that we've not gained an awful lot by writing this thin wrapper allowing us to write gen servers in this way

- There are no guarantees that the arguments passed in from the supervisor are the right type
- Erlang probably can't even construct the arguments properly if it's anything more complicated than a string (say, a record or ADT)
- There is no requirement for init/startLink to align their types, the args are going to be coerced back and forth and we'll get runtime crashes if we make a mistake there
- Low level wrappers are cumbersome, and gen servers are a very common tool in our arsenal, this could get old fast.
- We haven't even started to cover passing of arbitrary messages into the gen server (handle call, info, etc) (Spoiler alert: They're cumbersome too)
- There is no guarantee that certain methods (handle_info/etc) are actually present, although most of these behaviours are optional these days thankfully!

We can see that manually writing and using 1:1 mappings between Purescript and Erlang code for use across an application is not going to be a sustainable ideal - and yet we will press on and look at a few of the attempts made to do this in the following blog entries before finally trying to do something a little more idiomatic.
