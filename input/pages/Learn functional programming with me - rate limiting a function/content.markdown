I'm able to stream out a load of bullets in a long fish-poo like line, but what I need to do is rate limit these somehow...

- [Mission statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)
- [Improving my workflow](/entries/learn-functional-programming-with-me---improving-my-workflow.html)
- [Creating lots of state](/entries/learn-functional-programming-with-me---adding-lots-more-state.html)
- [Mutating lots of state](/entries/learn-functional-programming-with-me---mutating-lots-of-state.html)
- [Improving our data structure with maps](/entries/learn-functional-programming-with-me---improving-our-data-structure-with-maps.html)
- [Moving our red square with keyboard input](/entries/learn-functional-programming-with-me---keyboard-input-for-our-red-square.html)
- [Adding items to a sequence and firing bullets](/entries/learn-functional-programming-with-me---adding-items-to-a-sequence.html)

**My state is... in a state**

My bullets subsystem is just a list of bullets, and I actually need more state to represent this.

So I had a little change to create a map which represented

- The bullets active within the scene
- The time since we last fired a bullet

Not too complicated:

    (defn initBullets []
      {
        :lastFiringTicks 0
        :active ()
      }
    )


Of course, this has made a bit of a mockery out of my bullets logic...

    (defn bulletsLogic [state]
      (let [player (:player state)
            bullets (:bullets state)
            existingBullets 
            (for [bullet (:active bullets)]
              {
                :x (:x bullet)
                :y (dec (:y bullet))
                :w (:w bullet)
                :h (:h bullet)
              }
            )
          ]
        (if (@keyStates 32)
          {
            :lastFiringTicks (:lastFiringTicks bullets)
            :active (cons 
                    {
                     :x (:x (:player state))
                     :y (:y (:player state))
                     :w 5
                     :h 5
                    }
                    existingBullets
                    )
          }
          {
            :lastFiringTicks (:lastFiringTicks bullets)
            :active existingBullets
          }
        )
      )
    )

So much for that statement about Functional Programming forcing you to write code that is easy to reason about, a fool will write foolish code in whatever language ;-)

**Refactoring away from pain**

I can refactor this of course - now I know that there is an ability to 'modify' a single field within a map with 'assoc', I can create functions that don't need to know about the whole state and have little sub-functions for handling the logic of my bullets system.

    (defn bulletsLogic [state]
      (tryAndFire
        (moveBullets state)
      )
    )

How about moving the bullets, and then passing whatever the state is after that point to the tryAndFire function, and then returning the state of that to the outside world?

Moving the bullets becomes a matter of calling assoc on each bullet with a modified y, this is much nicer than having to copy across each property of the bullet.

    (defn moveBullets [state]
      (let [bullets (:bullets state)
            active (:active bullets)]
        (assoc state :bullets 
          (assoc bullets :active
            (for [bullet active]
              (assoc bullet :y (dec (:y bullet)))
            )
          )
        )
      )
    )

And trying to fire becomes a matter of adding an item to the list, or just returning original state


    (defn tryAndFire [state]
      (let [bullets (:bullets state)
            active (:active bullets)
            player (:player state)]
        (if (@keyStates 32)
          (assoc state :bullets 
            (assoc bullets :active
              (cons 
                {
                 :x (:x player)
                 :y (:y player)
                 :w 5
                 :h 5
                }
                active
              )
            )
          )
          state
        )
      )
    )

**Refactoring towards readability**

 Of course this can be re-factored a bit further for readability by pulling out another function
 
    (defn tryAndFire [state]
      (if (@keyStates 32)
        (fire state)
        state
      )
    )

Keeping the activity of firing to its own little function

    (defn fire [state]
      (let [bullets (:bullets state)
            active (:active bullets)
            player (:player state)]
        (assoc state :bullets 
          (assoc bullets :active
            (cons 
              (initBullet (:x player) (:y player) 5 5)
              active
            )
           )
         )
      )
    )

And yes, I also pulled out the code for creating a new bullet to make it even more obvious what is going on.

Now, actually because of my main logic function looking like this:

    (defn doLogic [state]
      {
        :direction (directionLogic state)
        :enemies (enemiesLogic state)
        :player (playerLogic state)
        :bullets (bulletsLogic state)
      }
    )

I've got to do some contortions to return 'just the bullets' from my bullets logic

    (defn bulletsLogic [state]
      (:bullets (tryAndFire
        (moveBullets state)
      ))
    )

**Refactoring away from the acrobatics**

But how about taking my new pattern further and applying each sub-system's changes to the state as a sequence of modifications to the state

    (defn doLogic [state]
      (bulletsLogic
        (playerLogic
          (enemiesLogic
            (directionLogic state)
          )
        )
      )
    )

Overall I think I like this approach better than trying to make each system return its own state, and as most of the logic seems to require sections of state from all over the show having the state all readily accessible seems to make sense.

That said, I'd normally shy away from this sort of approach in an OO language/manner because shared data implies coupling. 

Of course my normal solution is often the duplication of state via events or some other in-direct means and the reason for this tends to be to avoid accidental mutation which isn't going to be such a problem here where it's such an explicit decision.

I'll see how that plays out as I roll forwards, but now I've applied this refactoring step I'm in a much better position to get in the feature I wanted in the first place. 

If I end up not liking the current mutation capabilities, I can always change how I update the state from the outside-most part of my program by doing my assoc there. That mutation is such a visible decision in my code seems to be one of the bonuses of FP.

**Updating nested data with assoc-in**

I don't like all the nested assocs, so I hit up the Clojure docs now I'm not on a plane and discover 'assoc-in' and 'get-in', which do pretty much what you'd expect.

Instead of

    (defn fire [state]
      (let [bullets (:bullets state)
            active (:active bullets)
            player (:player state)]
        (assoc state :bullets 
          (assoc bullets :active
            (cons 
              (initBullet (:x player) (:y player) 5 5)
              active
            )
           )
         )
      )
    )

I can actually do

    (defn addBulletInPlayerLocation [state]
      (let [player (:player state)]
        (assoc-in state [:bullets :active]
          (cons 
            (initBullet (:x player) (:y player) 5 5)
            (get-in state [:bullets :active])
          )
        )
      )
    )

So I've done that everywhere it makes sense to as it makes the code more readable.

**Applying that time limiter**

So, what kicked off my re-factoring efforts was the addition of a little bit of state to my map ala

    (defn initBullets []
      {
        :lastFiringTicks 0
        :active ()
      }
    )

How to use this? Well, the algorithm goes something like this

    Is lastTicks nonZero? 
      Increase lastTicks
      is lastTicks equal to firing rate?
        Set lastTicks to Zero

    Are we trying to fire? 
      Yes? Is lastTicks 0?
        Yes? Fire, increase lastTicks
    

So we have two steps

- UpdateFiringTicks
- TryAndFire

**Managing the firing ticks**

Well first off, let's add this to the chain of logics we want to execute for our bullet logic

    (defn bulletsLogic [state]
      (tryAndFire
        (updateFiringTicks
          (moveBullets state)
        )
      )
    )

And we can simply execute the logic described above here

    (defn updateFiringTicks [state]
      (let [bullets (:bullets state)
            ticks (:lastFiringTicks bullets)]
        (if (= ticks 0) 
          state
          (if (= (rem ticks 30) 0)
            (assoc-in state [:bullets :lastFiringTicks] 0)
            (assoc-in state [:bullets :lastFiringTicks] (inc ticks))
          )
        )
      )
    )

And firing just needs to update this value now so...

    (defn fire [state]
      (incrementFiringTicks
        (addBulletInPlayerLocation state)
      )
    )
    
    (defn incrementFiringTicks [state]
      (assoc-in state [:bullets :lastFiringTicks] 1)
    )

Now my little craft only fires about once a second, mission accomplished.

**Summary**

This was quite a lengthy process to add a simple feature, I got side-tracked by what I hope was useful learning. I'm left a little uncomfortable with how I'm dealing with state and I feel like there is something more elegant I could be doing. Maybe I'll discover this as I continue through.


