What good is Space Invaders if you can't shoot anyone?

- [Mission statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)
- [Improving my workflow](/entries/learn-functional-programming-with-me---improving-my-workflow.html)
- [Creating lots of state](/entries/learn-functional-programming-with-me---adding-lots-more-state.html)
- [Mutating lots of state](/entries/learn-functional-programming-with-me---mutating-lots-of-state.html)
- [Improving our data structure with maps](/entries/learn-functional-programming-with-me---improving-our-data-structure-with-maps.html)
- [Moving our red square with keyboard input](/entries/learn-functional-programming-with-me---keyboard-input-for-our-red-square.html)

So far I have a fixed number of entities in my scene, which means the pattern

    logic(state) => newState

Has been working great for me, but how might I manage a variable number of these entities? How might I hook up the creation of these entities to keyboard input?

Well, actually I don't think it is going to be that hard - but we'll see as I give it a go...

**The basics once again**

The same deal as everything else so far (okay, my next entry might be on re-factoring all this to avoid duplication)


*This is how we create a bullet*

    (defn initBullet [x y w h]
     {
      :x x
      :y y
      :w w
      :h h
     }
    )


*Bullets logic is taking the current sequence of bullets and moving them on the vertical axis*

    (defn bulletsLogic [state]
      (for [bullet (:bullets state)]
        {
          :x (:x bullet)
          :y (dec (:y bullet))
          :w (:w bullet)
          :h (:h bullet)
        }
      )
    )


*And I'm going to draw a black square for each bullet*

    (defn bulletsRender [ctx state]
      (doseq [bullet (:bullets state)] 
        (let [{:keys [x y w h]} bullet]
          (drawSquare ctx x y w h "#000")
        )
      )
    )

There is nothing special or new about the above, although I've dropped the 'let' (as compared to the enemies render function) and pulled the bullets out of the state as part of the doseq call.

My bullets to begin with?

*Create an empty list*

    (defn initState []
     { 
       :direction 1
       :enemies (for [x (range 0 16 2)
                      y (range 0 8 2)]
                  (initEnemy x y 20 20)
       )
       :player (initPlayer 200 430 20 20)
       :bullets '()
     } 
    )

I found out how to do that on Google so I think it's right.

I also found out that apparently an empty list is not a sequence, although it is sequenceable which means I can safely do the above (I think)

*Calling the appropriate methods*

    (defn doLogic [state]
      {
        :direction (directionLogic state)
        :enemies (enemiesLogic state)
        :player (playerLogic state)
        :bullets (bulletsLogic state)
      }
    )

    (defn renderScene [ctx state]
      (enemiesRender ctx state)
      (playerRender ctx state)
      (bulletsRender ctx state)
    )


Now, all of this achieves the square root of diddly squat, although if I run the program it doesn't fall over in a heap pile of flames so that's pretty good going for all of this code.

**Hooking up some input events**

Well okay, for now what I'll do is say is "if the spacebar is pressed we'll fire a bullet", so we can do this in a similar manner to everything else thus far...


    (defn bulletsLogic [state]


*Get the player out, and update the bullets into a seq called 'existingBullets'*


      (let [player (:player state)
            existingBullets 
            (for [bullet (:bullets state)]
              {
                :x (:x bullet)
                :y (dec (:y bullet))
                :w (:w bullet)
                :h (:h bullet)
              }
            )
          ]

*If the space bar is down, then create a new sequence with a new bullet and the existing bullets*

        (if (@keyStates 32)
          (cons 
            {
             :x (:x (:player state))
             :y (:y (:player state))
             :w 5
             :h 5
            }
            existingBullets
          )

*Else just return the existing bullets*

          existingBullets
        )
      )
    )

This is pretty messy, although it's my first iteration at this so that's okay, I suspect I can be a bit cleverer about this and probably will end up being so!

**Don't change the collection, create a new one**

What we can see though, is that I don't "add the new bullet to the sequence", I instead "create a new sequence out of the old one plus the new items". That's what *cons* does according to the Clojure docs.

This is counter-intuitive to those of us used to our mutable collections, but apparently Clojure is quite clever about this stuff and I should trust that this is okay. (Just like so far I'm trusting that everything else is going to be okay too).

At some point soon I'll start reading about these sequences, collections, maps etc and see how they're implemented - now I have some "real" examples of their usage.

**The result**

<img src="/img/constant_flow.png">


