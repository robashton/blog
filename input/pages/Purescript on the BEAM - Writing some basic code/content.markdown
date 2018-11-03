Previous entries..

- [Intro](/entries/functional-erlang---purescript-on-the-beam---intro.html)
- [Getting Started](/entries/purescript-on-the-beam:-getting-started.html)

I've got the tools for building Purescript, I've got an empty repo - what now then?

### Writing some code

I don't want to leap off into writing a fully fledged gen server in Purescript, I'd settle with being able to spawn an Erlang shell and invoking a function I've written in - baby steps and all that. 

So, creating a file in ps_src called "mycode.purs", let's get a hello world sorted and see what we can see.

    module MyCode where
    
    import Effect.Console (log)
    import Effect (Effect)
    import Prelude
    
    hi :: Effect Unit
    hi = do
      log "Hi everybody"
      pure $ unit
    
I guess the first thing we notice here is that we have as many import statements as we do lines of code - this seems to be a thing with languages like this - indeed some of our Elm modules have nearly a whole page of imports at the top of them. Apparently the trick is to get something installed to help you with managing the damned things - I've not done that yet, I need to do that - maybe I'll do that soon - but for now, manually importing stuff as the compiler bitches at me seems to be getting me just as far as I need.

We have a function called 'hi' that returns an *[Effect](https://github.com/purescript/documentation/blob/master/guides/Eff.md)* which yields a value of type *Unit*, bleh - let's not worry about monads and stuff and just accept that you need to return Effects from functions if you want side effects like printing to screen or writing to a database, or spinning a new process up, that'll just be a theme from now on. When interacting with native Erlang code this is typically going to be the shape of things, as Erlang is very much not a pure functional environment.

The code is as simple as, "use do notation to perform some side effects, then return "unit" to whatever is going to call our function (either in another do block or so from Erlang itself).

    rebar3 compile

A pile of stuff happens, and I end up with some beams in ebin, notably I end up with *"myCode@ps.beam"*  - Purescript modules end up camelCase with a @ps suffix so core Purescript modules don't clash with the default Erlang modules, this may well change over the coming year but for now that's now it is.

    erl -pa _build/default/lib/*/ebin

And in the shell:

    Erlang/OTP 21 [erts-10.0.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]
    
    Eshell V10.0.5  (abort with ^G)
    1> l(myCode@ps).
    {module,myCode@ps}
    2> myCode@ps:hi().
    #Fun<myCode@ps.0.117199341>
    
What? Well - we're returning an Effect, which it turns out under the covers is just a function that's waiting to be evaled at a higher level, I can actually do

    3> Fn = myCode@ps:hi().
    #Fun<myCode@ps.0.117199341>
    4> Fn().
    Hi everybody
    unit

And I get my side effect and my return result from within the Purescript - this isn't terribly useful if I'm to be calling Purescript code from Erlang code, and given that OTP kinda requires that we have the ability to do that, that'll be something I need to sort out in the next post.
