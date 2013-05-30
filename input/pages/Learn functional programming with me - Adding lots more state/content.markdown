I've got a single moving square and a small pile of Clojure written so far, but now what I want to do is have a collection of moving squares because no game of Space Invaders is complete without aliens, and last I checked there was more than one of them.

- [Mission statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)
- [Improving my workflow](/entries/learn-functional-programming-with-me---improving-my-workflow.html)


Well, if I want lots of squares I'm going to have to look at some means of representing collections in Clojure and my searches take me to [Sequences](http://clojuredocs.org/clojure_core/clojure.core/seq)

To create a sequence, I can do something like this

    (defn createSeq []
      (for [x [0 1 2 3]]
        x
      )
    )

Which returns me a sequence containing 0 1 2 3, an important point here is (at least I think), is that this sequence is *lazy*, it has not been executed yet. 

Now, my collection of invaders will actually be 2D, so what I can actually do is


    (defn createSeq []
      (for [x [0 1 2 3] y [0 1 2 3]]
        [x y]
      )
    )

Which returns me a sequence containing lots of [x y] pairs, so [0 0] [0 1] [0 2] etc.

Of course, there are functions for me here, so I can make a range

    (defn createSeq []
      (for [x (range 0 100 10) 
            y (range 0 100 10)]
        [x y]
      )
    )

Pretty nifty if I do say so myself, so what I can start with if I want to draw a grid of yellow squares is something like

    (defn initState []
     (for [x (range 0 100 10)
           y (range 0 100 10)]
       [x y]
     )
    )

And I can call my initial tick method, passing in this state

    (defn ^:export init []
      (tick (initState)) 
    )

And then loop through this

    (defn tick [enemies]
      (let [ctx (context)] 
        (clearScreen ctx) 
        (doseq [[x y] enemies] (drawSquare ctx x y 5 5))
        (js/setTimeout (fn []
          (tick enemies) 
        ) 33  )
      )
    )

Note that I'm able to expand the content of this sequence as part of the doseq call

      (doseq [[x y] enemies] (drawSquare ctx x y 5 5))

Gotta like that at least a little bit!

About this doseq, this doseq is *explicitly* created to allow for side effects, I could have done

      (for [[x y] enemies] (drawSquare ctx x y 5 5))

But nothing would happen because this is lazy too - had my head scratching for a while on that one...
