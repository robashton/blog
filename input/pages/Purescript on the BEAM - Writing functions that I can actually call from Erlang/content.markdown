Previous entries..

- [Intro](/entries/functional-erlang---purescript-on-the-beam---intro.html)
- [Getting Started](/entries/purescript-on-the-beam:-getting-started.html)
- [Writing some basic code](/entries/purescript-on-the-beam---writing-some-basic-code.html)

I'd like to be able to actually invoke functions that are written in Purescript, *from* Erlang - this will be necessary when say, providing the function to a supervisor for use in the following way.

    init([]) ->
      {ok, { {one_for_all, 10, 10}, [
                                     #{id => my_server, start => {myPurescriptModule@ps, startLink, [[]]}}
                                    ]} }.


At time of writing this post, there is no "native" Purescript OTP support, so you're not going to be writing supervision structures in Purescript (Give us some time and I think we probably will end up with all of this), so that means needing to support interop in this direction.

The problem is that a function in Purescript that returns an effect is actually a function that returns a 0-arity function which returns the value that would in Purescript just be a value -  these functions are designed to be executed inside the Purescript environ, and what we need is a function that simply returns the thing that we want to our Erlang code.

Going back to my previous example

    module MyCode where

    import Effect.Console (log)
    import Effect (Effect)
    import Prelude

    hi :: Effect Unit
    hi = do
      log "Hi everybody"
      pure $ unit

It turns out there are a pile of types that represent *native* functions of various arities in a handy module called *Effect.Uncurried*, and by declaring that we have an EffectFnX (where X is our arg count), we can actually end up with something usable from Erlang.

    module MyCode where

    import Effect.Console (log)
    import Effect.Uncurried (EffectFn1, mkEffectFn1)
    import Prelude
    
    hi :: forall a. EffectFn1 a Unit
    hi = mkEffectFn1 \_ -> do
      log "Hi everybody"
      pure $ unit

There is no EffectFN0, so our "hi" function now needs to take a argument in order to work - however this means we now have a function that is invokable from Erlang in the usual way. For those of us coming from Haskell, the forall. syntax is a bit weird, but effectively if you've got a polymorphic function, you're expected to declare this explicitly (You can then add restrictions on what that type is) - the equivalent extension in Haskell can be found over here at [ExplicitForAll](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/explicit-forall)

    1> myCode@ps:hi(this_is_ignored).
    Hi everybody
    unit
    2> 

For the extra curious, this is what happens if you try invoking it without the expected argument..

    2> myCode@ps:hi().               
    #Fun<myCode@ps.0.61314832>
    3> 

Well indeed, this makes sense when you consider how currying works in languages like Purescript/Haskell. Don't forget the args I guess.

I'm nearly armed enough at this point to consider writing a gen server in purescript, oh hell.
