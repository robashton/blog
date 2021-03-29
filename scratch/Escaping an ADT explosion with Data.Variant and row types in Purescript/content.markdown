At work we are in the middle of a wholescale re-imagining of how we build our software, after writing a heap of control/management plane logic in [Purescript](https://www.purescript.org/) over the last couple of years ([Compiled to Erlang](https://purerl-cookbook.readthedocs.io/en/latest/) of course), using [Stetson](https://github.com/id3as/purescript-erl-stetson) and [Pinto](https://github.com/id3as/purescript-erl-pinto/) we're stepping back to consider next steps. Some team members are [dusting off](https://github.com/id3as/purescript-erl-pinto/pull/15) Pinto and fixing some of the niggles we've put off because we've been too busy building actual software and others are deep into a re-think of our more core technologies. Principally that is the capturing of video/audio sources, the transformation of this content into other versions of that content and the swift dispatch of that aforementioned video/audio content (yes, like all software it can be boiled down to moving bytes from one place to another..).

All in all it's good fun and I'm discovering things about programming in Purescript that I never thought I'd be capable of learning, never mind having the time to spend learning - one of the biggest '*woah hang on*' moments has to have been some of the things we're managing to push into the type system via what was our initial use of Data.Variant and then the rabbit-hole we fell down as we started bashing typeclasses out to validate even more at compile time and avoid some of the potential runtime issues we would usually devote our valuable time/money avoiding by adding complexity to the runtime code itelf.

A common issue
===

A standard hilarity in what we do is that there are not only multiple flavours of media content floating around out there, but within those multitude of flavours there are almost infinite sub-variations of those flavours and a large part of our efforts can be summarised as getting those flavours into a common format internally so we can process them with a lot of the same code. Even once we've converted data into its raw format there are still plenty of different colour spaces, pixel formats, various means of laying out bytes in memory so that they can be processed one way or another by various libraries and boy do we have a lot of code to do those things and therefore end up still needing to convert between them as a workflow does its job.

One of the interesting findings when pulling old code into our new workspace was how much of that code was duplicated about the place becaue you could never really trust the content you were getting was indeed one thing or another, while we have *incredibly* rich metadata attached to each and every frame of data we process that we can use to decision on, all of that decision is at runtime and it's often easier to just shuffle some bytes about than insist up front that everything be one thing or another.

Wouldn't it be nice if we could just not do that any more? Express in the type system based on metadata within the content itself that certain code is only meant to be invoked with content that fits a certain structure.

*I only want that thing... or that thing*

It's entirely possible using plain ol' records and ADTs to express nearly all of the concepts in the world and this is definitely the first place we'd go to when modelling within a problem space. Let's exit our own domain for the rest of this blog post because it's too vast and also I don't want to be copying and pasting code from our codebase in order to explain it. Let's find something new to talk about.

Imagine if you will, ~~a burrito~~ a childrens puzzle toy. Square pegs, round holes - you know how it goes, and a factory that produces these things in a pipeline, and let's model that with ADTs and functions the easiest way we can think of - if we can allow a certain amount of leeway here for the rest of the blog post because this domain is a lot simpler than the one we have at in the office and falls into the classic '*making up a contrived problem in order to explain a solution that doesn't exactly fit the old problem*' trap that is common for this sort of blog post.

We might start off with something that looks like this.

```haskell

  type PuzzlePiece 
    = { maxWidth :: Int
      , maxHeight :: Int
      , shape :: PuzzleShape
      , colour :: PuzzlePaintJob
      }

  data PuzzleShape = Square { size :: Int }
                   | Circle { radius :: Int }
                   | Rectangle { width :: Int, height :: Int }
                   | Crescent { radius :: Int, arc :: Int }
                   | Triangle { length :: Int }

  data PuzzlePaintJob = Unpainted
                      | Painted Colour

  data Colour = Red | Green | Blue | Yellow

  type Box = List PuzzlePiece

```

Hopefully this is a sufficiently simple example of some code where we have attributes that are common to all objects, and then attributes that only exist when the objects are in one state or another. Now let's imagine that we want to construct a workflow out of the following functions. 

```haskell

  buildSquare :: Int -> PuzzlePiece
  buildSquare size = { maxWidth: size 
                     , maxHeight: size 
                     , shape: Square { size }
                     , colour: Unpainted
                     }

  buildCircle :: Int -> PuzzlePiece
  buildCircle radius = { maxWidth: radius*2 
                       , maxHeight: radius*2
                       , shape: Circle { radius }
                       , colour: Unpainted
                       }

  buildRectangle :: Int -> Int -> PuzzlePiece
  buildRectangle width height = { maxWidth: width 
                                , maxHeight: height 
                                , shape: Rectangle { width, height }
                                , colour: Unpainted 
                                }

  buildCrescent ::  Int -> Int -> PuzzlePiece
  buildCrescent radius arc = { maxWidth: radius*2 
                             , maxHeight: radius 
                             , shape: Crescent { radius, arc }
                             , colour: Unpainted 
                             }

  buildTriangle :: Int -> PuzzlePiece
  buildTriangle length = { maxWidth: length
                         , maxHeight: floor $ ((toNumber length) * 1.7) / 2.0
                         , shape: Triangle { length }
                         , colour: Unpainted 
                         }

  paintQuad :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintQuad = throw "calculate how much paint we need here and take it"

  paintRound :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintRound = throw "calculate how much paint we need here and take it"

  paintTriangle :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintTriangle = throw "calculate how much paint we need here and take it"

  pack :: PuzzlePiece -> Box -> Box
  pack = (:)

  emptyBox :: Box
  emptyBox = Nil

```

Our workflow needs to (for some reason), construct a puzzle box from a sequence of these functions, consuming paint from some sort of central system as it goes..

```haskell

buildPuzzleBox :: Effect Box 
buildPuzzleBox = do
  redSquare <- paintQuad Red $ buildSquare 100
  yellowRectangle <- paintQuad Yellow $ buildRectangle 25 50
  greenCircle <- paintRound Green $ buildCircle 50
  blueTriangle <- paintTriangle Blue $ buildTriangle 23
  pure $ pack redSquare 
        $ pack yellowRectangle
        $ pack greenCircle 
        $ pack blueTriangle
        $ emptyBox


```

So for the purposes of this post, we've got a variety of different types of shape, and a smaller number of functions to go and do 'some work' with those shapes, and some of those functions support more than one of the shapes.

A wise and learned person might look at this and say "*hey what*, you're losing what the actual shape of the puzzle piece is when you contruct that puzzle piece, how on earth are you going to validate that you are indeed passing in the appropriate shape to the appropriate function and not trying to write code that paints a triangle as if it's a rectangle" and yes that is indeed a bit of an issue when you're working with concrete types and ADTs - at some point you'll want to only operate on a subset of the ADT and then you've got a problem.


```haskell

  -- "TODO": Calculate paint amount based on rect..
  paintQuad :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintQuad colour p@{ shape: (Rectangle rect) } = pure $ p { colour = Painted colour }
  paintQuad colour p@{ shape: (Square square) } = pure $ p { colour = Painted colour }
  paintQuad _ _ =  unsafeCrashWith "Bad argument: expected one of the rectangles!"

  -- "TODO": Calculate paint amount based on arc/radius..
  paintRound :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintRound colour p@{ shape: (Crescent crescent) } = pure $ p { colour = Painted colour }
  paintRound colour p@{ shape: (Circle circle) } = pure $ p { colour = Painted colour }
  paintRound _ _ =  unsafeCrashWith "Bad argument: expected one of the round pieces!"

  -- TODO: Calculate paint amount based on triangle area..
  paintTriangle :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintTriangle colour p@{ shape: (Triangle triangle) } = pure $ p { colour = Painted colour }
  paintTriangle _ _ =  unsafeCrashWith "Bad argument: expected one of the triangle pieces!"

```

So there's our imagined problem in a nutshell, by allowing the stuffing of square pegs into round holes we're going to end up with runtime errors if we aren't careful. We should at this point look at what the common solutions to this are in our codebases...

*Just have a single 'paint' method that knows how to paint all the things*

Yup, definitely - if the domain is fixed and small then this is the real solution to this problem, thanks blog post over, let's get back to work, thanks for reading!

*Have a typeclass for Paintable and make have things implement it*

Not a bad idea, but the issue then becomes 'what if I want to write *other* code that just does things with these structures?' I either need to declare the instances for the typeclases next to the definitions or I need to declare them alongside the typeclass definition; this again doesn't scale beyond the initial example once we start writing real code where we don't know all of our types.. You also can't have heterogenous lists containing implementors of a typeclass so this is not typically a good solution for this sort of imagined scenario (And isn't, as far as I can tell, what typeclasses are for anyway).

*Sub modules, sub types*

This is probably the most common approach to ensuring safety of sorts across a larger project where certain modules only take a subset of the overall model - re-expressing parts of that model to make their inputs and outputs concrete and obvious. If you force code consumers to create or transform/select aspects of a model. There is a benefit that further code can always be written without impinging on those sub modules because the addition of new types doesn't automatically mean having to add clauses to the functions of those sub modules..

```haskell

module Quad where

  paint :: forall a. Colour -> { colour :: PuzzlePaintJob | a } -> { width :: Int, height :: Int } -> Effect { colour :: PuzzlePaintJob | a }
  paint colour i { width, height } = do
    _ <- drainPaint colour (width * height)
    pure $ i { colour = Painted colour }


```

Of course this then means that whilst some records can remain intact, a certain amount of mapping is required to contract the concrete types down into the right input shape required by these sub modules.. and then indeed using case statements or functions to expand the types so we can do that in the first place.


```haskell

  case obj.shape of
    Square s -> Quad.paint Yellow obj { width: s.size, height: s.size }
    Rect r -> Quad.paint Yellow obj r
    Triangle t -> Triangle.paint Yellow obj { o: t.length, a: t.length, h: t.length }
    _ -> etc
  
```

Typically the easier understood solutions tend towards the 'make things more concrete' side of things which is a *good thing* for a lot of code and most of the time is the right thing to do.

Once you've firmly placed your boots in the realm of '*we have thousands of these, and they all take different subsets of these few dozen dimensions*' then this still might be the right approach as burdensome as it is, it is at least simple and 'just code' - but we know better and we know that there is a concept floating around out there, a solution in search of a problem to solve and that solution looks like [Data.Variant](https://github.com/natefaubion/purescript-variant).

Data.Variant 101
===

Now it's not immediately apparent (unless you're already way ahead of me) how this ties into our problem here, but we'll continue to work towards that...

```haskell

  type KnownShapes = ( rect :: { width :: Int, height :: Int }
                     , circle :: { radius :: Int }
                     , square :: { size :: Int }
                     , triangle :: { length :: Int }
                     )
  
  type PuzzlePiece 
    = { maxWidth :: Int
      , maxHeight :: Int
      , shape :: Variant KnownShapes
      , colour :: PuzzlePaintJob
      }

```

To support the use of this variant, we'll need to define a bunch of helper functions using SProxy (don't worry about it, it just lets you do compile time stuff with strings)

```haskell

  _rect = (SProxy :: SProxy "rect")
  _circle = (SProxy :: SProxy "circle")
  _square = (SProxy :: SProxy "square")
  _triangle = (SProxy :: SProxy "triangle")

  rect :: { width :: Int, height :: Int } -> Variant KnownShapes
  rect = inj _rect

  circle :: { radius :: Int } -> Variant KnownShapes
  circle = inj _circle

  square :: { size :: Int } -> Variant KnownShapes
  square = inj _square

  triangle :: { length :: Int } -> Variant KnownShapes
  triangle = inj _triangle


```

So what we've got is a row type with the symbols 'rect/circle/square/triangle' and the means of constructing instances of that variant via our helper functions, that mean we can re-write our construction functions using these building blocks


```haskell

  buildSquare :: Int -> PuzzlePiece
  buildSquare size = { maxWidth: size 
                     , maxHeight: size 
                     , shape: square { size }
                     , colour: Unpainted
                     }

  buildCircle :: Int -> PuzzlePiece
  buildCircle radius = { maxWidth: radius*2 
                       , maxHeight: radius*2
                       , shape: circle { radius }
                       , colour: Unpainted
                       }

  buildRectangle :: Int -> Int -> PuzzlePiece
  buildRectangle width height = { maxWidth: width 
                                , maxHeight: height 
                                , shape: rect { width, height }
                                , colour: Unpainted 
                                }

  buildTriangle :: Int -> PuzzlePiece
  buildTriangle length = { maxWidth: length
                         , maxHeight: floor $ ((toNumber length) * 1.7) / 2.0
                         , shape: triangle { length }
                         , colour: Unpainted 

```


Now those 'paint' functions... oh sadness! We still need an unsafe crash because our PuzzlePiece defines a 'Shape' as *anything* in the List of '*KnownShapes*'

```haskell

  paintQuad :: Colour -> PuzzlePiece -> Effect PuzzlePiece
  paintQuad colour p@{ shape } = do
    let selector = default (unsafeCrashWith "Not supported" :: Int)
                      # on _rect (\r -> r.width * r.height)
                      # on _square (\s -> s.size * s.size)
    _ <- drainPaint colour $ selector shape
    pure $ p { colour = Painted colour }

```

Data.Variant *allows* us to express that an object could be one of several things at runtime, using row types and symbols to get the compiler to tell us if we're not exhaustively checking all options at compile time. The crux of it being that if a function can take a *superset* of of the values you *know* you could have, then it's always going to be safe to call that function. In our example above defining every puzzle piece as being possibly 'one of these items from this superset' isn't buying us anything over just using an ADT.

Well in order to do this, we're going to have to allow the parameterisation of the Variant being held to represent *shape* and flow that through to the existing types.

```haskell

  type PuzzlePiece knownShapes
    = { maxWidth :: Int
      , maxHeight :: Int
      , shape :: Variant knownShapes
      , colour :: PuzzlePaintJob
      }

  type Box knownShapes = List (PuzzlePiece knownShapes)

```

In this example, we're not locking down our known shapes to anything at all (!!), but if were to parameterise this PuzzlePiece with a concrete type (KnownShapes) then we'd be saying "This could be any of the values in KnownShapes". 

```haskell

  type APuzzlePieceOfAnyKnownShape = PuzzlePiece KnownShapes
  type ABoxOfPuzzlePiecesOfAnyKnownShape = Box KnownShapes 

```

Similarly, if we were to define a type for 'only things that are quadrilaterals' and parameterise with that..? The list can *only* contain values that are contained within that variant.

```haskell

  type Quadrilaterals = ( rect :: { width :: Int, height :: Int }
                        , square :: { size :: Int }
                        )

  type OnlyQuads = PuzzlePiece Quadrilaterals
  type ListOfOnlyQuads = Box Quadrilaterals

```

Absolutely *crucially* here, if we have functions that can take a superset of this list then they are *absolutely safe* to call.

Before we proceed let's tidy the definitions up of what our shapes are as we're about to write the same type definition over and over again repeatedly unless we do! Instead of inline record syntax, we define some type aliases and use them in our row definition for 'possible values of KnownShapes'

```haskell

  type Rect = { width :: Int, height :: Int }
  type Circle = { radius :: Int }
  type Square = { size :: Int }
  type Triangle = { length :: Int }
  type Crescent = { radius :: Int, arc :: Number }

  type KnownShapes = ( rect :: Rect
                     , circle :: Circle
                     , square :: Square 
                     , triangle :: Triangle
                     , crescent :: Crescent
                     )
````

With this done, we can make some super concrete definitions for the code responsible for constructing shapes for our puzzle box...


```haskell

  _rect = (SProxy :: SProxy "rect")
  _circle = (SProxy :: SProxy "circle")
  _square = (SProxy :: SProxy "square")
  _triangle = (SProxy :: SProxy "triangle")
  _triangle = (SProxy :: SProxy "crescent")

  rect :: Rect -> Variant ( rect :: Rect )
  rect = inj _rect

  circle :: Circle -> Variant ( circle :: Circle )
  circle = inj _circle

  square :: Square -> Variant ( square :: Square )
  square = inj _square

  triangle :: Triangle -> Variant ( triangle :: Triangle )
  triangle = inj _triangle

  crescent :: Crescent -> Variant ( crescent :: Crescent )
  crescent inj _crescent

```

So now if we want to talk about creating a variant that can be a rect... that's all it can be, it can't be anything else, this can follow all the way down into our factory functions for creating our container shapes.


```haskell

  buildSquare :: Int -> PuzzlePiece ( square :: Square )
  buildSquare size = { maxWidth: size 
                     , maxHeight: size 
                     , shape: square { size }
                     , colour: Unpainted
                     }

  buildCircle :: Int -> PuzzlePiece ( circle :: Circle )
  buildCircle radius = { maxWidth: radius*2 
                       , maxHeight: radius*2
                       , shape: circle { radius }
                       , colour: Unpainted
                       }

  buildRectangle :: Int -> Int -> PuzzlePiece ( rect :: Rect )
  buildRectangle width height = { maxWidth: width 
                                , maxHeight: height 
                                , shape: rect { width, height }
                                , colour: Unpainted 
                                }

  buildTriangle :: Int -> PuzzlePiece ( triangle :: Triangle )
  buildTriangle length = { maxWidth: length
                         , maxHeight: floor $ ((toNumber length) * 1.7) / 2.0
                         , shape: triangle { length }
                         , colour: Unpainted 
                         }

```

So holy moly, what we now have is a pile of functions that tell you *exactly* what they create and if you're working with these specific types there can be no ambiguity. So how on earth do we use them? Well as mentioned already, being "correct" in thi world is all about ensuring you only ever pass objects into functions that take a superset of the possible values for that object. Being incredibly specific as above, each of the functions that build our puzzle pieces are all a subset of the total set of "KnownShapes", but it's equally possible for them to be a subset of other subsets of that superset.


How about re-defining our paintQuads function to only take the shapes that are quads..


```haskell

  type QuadShapes = ( rect :: Rect
                    , square :: Square 
                    )
  
  paintQuad :: Colour -> PuzzlePiece QuadShapes -> Effect (PuzzlePiece QuadShapes)
  paintQuad colour p@{ shape } = do
    let selector = case_ 
                    # on _rect (\r -> r.width * r.height)
                    # on _square (\s -> s.size * s.size)
    _ <- drainPaint colour $ selector shape
    pure $ p { colour = Painted colour }
  
```

Wahey, gone is the unsafe crash, it is no longer possible to pass in variants containing the wrong type of data because at compile time the build will stop you. Now we have a whole other bit of fun on our hands, how do we go and call this function?

We have a *'PuzzlePiece ( square :: Square )'*, and the function accepts *'PuzzlePiece ( rect :: Rect, square :: Square )'*. These are not the same thing and attempting to call this function directly will result in the following compiler error..

```haskell
  redSquare <- paintQuad Red $ buildSquare 100
```

```

  Could not match type

    ( ... )

  with type

    ( rect :: { height :: Int
              , width :: Int
              }
    ...
    )

```

Rude but somewhat understandable, the types don't match - and even though *we* know we'll never have a value of 'rect :: Rect' in our variant, the function *accepts* something that might and therefore we need to *expand* our type in order to say "This *could* be a rect or a square, even though we totally know it's a square"... Thankfully this is a common problem and one with a solution inside of Data.Variant itself.


```haskell
  -- | Every `Variant lt` can be cast to some `Variant gt` as long as `lt` is a
  -- | subset of `gt`.
  expand
    ∷ forall lt a gt
    . R.Union lt a gt
    => Variant lt
    -> Variant gt
  expand = unsafeCoerce
```

What in the.. is this even? Well - it turns out that you can use typeclass constraints to do more than simply check if something has "implemented Eq" (who knew?!), in this case we're checking that an instance of the typeclass R.Union exists for our two variants, where R.Union is checking if the rows in 'lt' are a subset of the rows in 'gt'. Read that a couple of times if you don't get it, it took me a couple of goes too even though to anybody who already understands it it's incredibly 'simple'.

Now, we don't have a *Variant of lt* exactly, we have a *Record of lt*, that itself contains a *Variant of lt*, this is somewhat dull as it means we can't just go ahead and call 'expand' in our broken function. Naively we might attempt something like this for this is *spiritually* what we are *trying* to do..

```haskell

expandPuzzlePiece :: forall lt gt. PuzzlePiece lt -> PuzzlePiece gt
expandPuzzlePiece = p = p { shape = expand p.shape }

```

But this won't work, trying to change the type of something that's paramterised by the type of the containing record... doesn't *actually* make any sense. *expand* doesn't actually *do* anything, the data is exactly the same.. no, all we really want to achieve here is to tell the compiler "I know I said that it was a PuzzlePiece of lt, but actually it's a PuzzlePiece of gt and that is a safe assertion to make because lt is a subset of gt", we could therefore just go and write...


```haskell
  expandPP
    ∷ forall lt a gt
    . R.Union lt a gt
    => PuzzlePiece lt
    -> PuzzlePiece gt
  expandPP = unsafeCoerce

```

This will certainly work and allow me to do

```haskell
  redSquare <- paintQuad Red $ expandPP $ buildSquare 100
```

That's kinda neat, and allows us to go off and write even more of our painting pipeline..


```haskell
  redSquare <- paintQuad Red $ expandPP $ buildSquare 100
  yellowRectangle <- paintQuad Yellow $ expandPP $ (buildRectangle 25 50) 
  greenCircle <- paintRound Green $ expandPP $ buildCircle 50
  blueTriangle <- paintTriangle Blue $ buildTriangle 23
```

But now we have *another* problem, our next line of code wants to stuff all these painted shapes into a single list of type 'PuzzlePiece KnownShapes', but what we have here is a collection of  PuzzlePiece (RoundShapes), PuzzlePiece (QuadShapes), etc.. All of those are *subsets* of that superset "KnownShapes", so we could go ahead and write the code like this...


```haskell

  buildPuzzleBox ::Effect (Box KnownShapes)
  buildPuzzleBox = do
    redSquare <- paintQuad Red $ expandPP $ buildSquare 100
    yellowRectangle <- paintQuad Yellow $ expandPP $ (buildRectangle 25 50) 
    greenCircle <- paintRound Green $ expandPP $ buildCircle 50
    blueTriangle <- paintTriangle Blue $ buildTriangle 23
    pure $ pack (expandPP redSquare)
          $ pack (expandPP yellowRectangle)
          $ pack (expandPP greenCircle)
          $ pack (expandPP blueTriangle)
          $ emptyBox

```

The phrase we've started using for code like this is "that's absolutely honking". It turns out that if you use concrete types everywhere and rely on expansion to slowly widen the possible variants a type could hold then 50% of the code you write ends up being calls to various forms of 'expand' (It's possible to write a generic expandRowTypes function that can take any (#Type -> Type) and do the subset check so you can convert things like (List lt -> List gt) without having to write an expandList function but it turns out that it's not a great way to build a codebase. No - a better (in our opinion) idea is to leave the types of the variants alone and use more typeclass constraints to make sure that calls to functions are safe and reduce the need to actually change the types of the objects.

Take our *paintQuad* function for example, there isn't actually a good reason for us to be changing the type of *PuzzlePiece a* just to call this function. If I have a *PuzzlePiece ( square :: Square )* I should be able to just check that ( square :: Square ) is indeed a subset of QuadShapes in that function head itself and return exactly the same type I was given in the first place.

The answer becomes "Scrap the expands and add appropriate type constraints to the function being called and then parameterise *that*". In essence what we want is


```haskell
  paintQuad :: forall quadShape. 
               R.Union quadShape a QuadShapes =>
               Colour -> PuzzlePiece quadShape -> Effect (PuzzlePiece quadShape)
```

In this manner, we're not going to be changing the type of what we're given in this function, just doing the right thing based on the current value and then passing it on.

We seeing as we don't need to do anything to the content of the variant itself, we can expand the type at the last moment before running it through Data.Variant's *case_* function and know this is safe because we have defined our union in the type signature.

```haskell

  paintQuad :: forall shape a. 
               R.Union shape a QuadShapes =>
               Colour -> PuzzlePiece shape -> Effect (PuzzlePiece shape)
  paintQuad colour p@{ shape } = do
    let selector = case_ 
                    # on _rect (\r -> r.width * r.height)
                    # on _square (\s -> s.size * s.size)
    _ <- drainPaint colour $ selector $ (expand shape :: Variant QuadShapes)
    pure $ p { colour = Painted colour }

```

Wahey, rewriting all of the paintXXX functions with this union suddenly means half of the expands can be removed from our workflow..


```haskell
  buildPuzzleBox ::Effect (Box KnownShapes)
  buildPuzzleBox = do
    redSquare <- paintQuad Red $ buildSquare 100
    yellowRectangle <- paintQuad Yellow $ (buildRectangle 25 50) 
    greenCircle <- paintRound Green $ buildCircle 50
    blueTriangle <- paintTriangle Blue $ buildTriangle 23
    pure $ pack (expandPP redSquare)
          $ pack (expandPP yellowRectangle)
          $ pack (expandPP greenCircle)
          $ pack (expandPP blueTriangle)
          $ emptyBox
```

The key learning here is that we should only be expanding when totally necessary (usually at the edges of the domain using these variants when things need to be concrete again). As soon as we widen a type, we can't contract it (safely) because there are no guarantees that it can't contain a value that doesn't exist in the contracted type.







