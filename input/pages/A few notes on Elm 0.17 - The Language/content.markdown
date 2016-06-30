Continuing on then

- [A few notes - Intro](/entries/a-few-notes-on-elm-0.17---intro.html)
- A few notes - The Language

So it's Haskell?
==

Not really, there are a lot of things in common with Haskell but on closer inspection they are typically same-same-but-different.

```haskell
    -- Haskell
    doSomething :: Foo -> Bar
    
    -- Elm
    doSomething : Foo -> Bar
```

or

```haskell
    -- Haskell
    doSomething x y = x $ somethingElse y
    
    -- Elm
    doSomething x y = x <| somethingElse y
```    

I don't really care about any of these differences, as they are largely arbitrary syntax/naming decisions - although I do feel as though maintaining more of a similarity with Haskell seeing as these choices *are* arbitrary would make it easier to port code or read documentation across the two.  (The Elm compiler is hosted in Haskell after all anyway...)

The lack of typeclasses has lead to things like this though

```haskell
    -- Haskell
    maybe input "default"
    
    -- Elm
    Maybe.withDefault input "default"
```

Agh.

Typeclasses
==

No type classes, there is a whole history of conversation about the lack of need for them (and the ability for ADTs to do the job admirably), I have yet to see this promised land however, all I see is a bunch of modules for different data structures with methods dangling off them and it reminds me a bit of Erlang where my ability to write sensibly composed functional code is often hampered by its fudge of modules.

By getting rid (in name anyway) of Monads, Applicative, etc - Elm seems much more acessible to the average user. That's not to say that they don't exist in some form in the codebase but side effects/etc have been shoved into the more domain specific "[Task](https://github.com/elm-lang/core/blob/master/src/Task.elm)" and Effect managers and in the Elm Architecture itself it's rare you have to touch anything that isn't "just data". 

This is a sensible design decision on the surface, as users who are new to functional programming aren't overwhelmed by concepts but I can't help but feel that the data-oriented patterns in Elm couldn't be exposed in Haskell itself without surfacing those "tricky" concepts whilst keeping them available (there's a weekend project then). 

Moving on, by placing focus on ADTs and records and inferring expected structure from usage the error messages can be a bit perplexing to start off with, despite them being very well written.

Consider this function for example

```haskell
    type alias Model = {
      realField : String
    }

    extractThingy : Model -> String
    extractThingy model = model.missingField

```

The error from this both simultaneously very useful and counter-intuitive:

<pre>
    -- TYPE MISMATCH ------------------------------------------------------ Test.elm

    The type annotation for `extractThingy` does not match its definition.

    23| extractThingy : Model -> String
                        ^^^^^^^^^^^^^^^
    The type annotation is saying:

        { realField : ... } -> String

    But I am inferring that the definition has this type:

        { b | missingField : ... } -> a

    Detected errors in 1 module.  
</pre>

Instead of "Hey, Model doesn't have a field called missingField" we get a complaint that the code is inferring a structure with 'missingField' by usage and it doesn't match the definition it has been supplied. This is because the compiler works backwards from the point of usage and gives its error messages from that context.

I'm not the only one who got confused initially by this and with more complicated structures and nested code the errors being dumped out can be pretty daunting. It's something you get the hang of though and the descriptive nature of the error messages make them look a lot more excessive than they really are.

Functional Records
==

[This has been covered before](http://lexi-lambda.github.io/blog/2015/11/06/functionally-updating-record-types-in-elm/), but records have 'getters' generated for them so we can do things like this

```haskell
    type alias Model = { 
          realField : String
        }

    extractThingy : List Model -> List String
    extractThingy items = List.map .realField items
  
```

This can be quite handy if we're writing functional code around records, sadly no equivalent setters are generated so it's hard to compose operations against records.

Development environment
==

I'm in Emacs these days on account of our company's switch to [Rebar3](https://www.rebar3.org/) in the [Erlang](http://erlang.org/) world and my reluctance to sit there and fix my Vim plug-ins; I'm therefore using [elm-mode](https://github.com/jcollard/elm-mode) which gives me access to some integration with elm-reactor and elm-format which we use and don't even bother arguing about.

Following the [elm-style guide](https://github.com/NoRedInk/elm-style-guide) from No-Red-Ink leaves us one less thing to worry about when writing code.

I currently really miss the following things from my Haskell development:

- [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/)
- [hoogle](www.haskell.org/hoogle/)
- compile-on-save insta-feedback-in-repl

I think I can probably do the latter in an hour or so with existing tooling, but right now I feel one of the biggest benefits of having a sensible type system is missing: Being able to inspect the darned types and work out how functions are supposed to compose without having to constantly jump to documentation.

Hoogle or a hoogle-like is a must, even with something as low on surface area as Elm; when you know the types of what you're working with and you know the types of the things you want then Hoogle is one of the most useful things in your arsenal.

That said, a lot of the time in Haskell we are dealing with parametric types and the functions we are looking for go from (a -> b) -> b, in our record and ADT oriented world we don't have the interchangeability that we'd get in that world and Hoogle is less useful. I don't know how I feel about that yet.

Next up..
==

I'll look at the structure of our web apps and highlight some of the issues and decisions we've encountered so far in 0.17
