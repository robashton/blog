Okay, so I [looked at records](/entries/learn-functional-programming-with-me---implementing-records.html) and how they might help, but my code hasn't actually been improved all that much yet if I'm aiming towards [composability](/entries/learn-functional-programming-with-me---but-rob,-it-needs-to-be-more-composable.html).

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
- [Refactoring my state transitions](/entries/learn-functional-programming-with-me---refactoring-my-state-transitions.html)
- [More idiomatic clojure](/learn-functional-programming-with-me---more-idiomatic-clojure.html)
- [Adding collision detection](/entries/learn-functional-programming-with-me---adding-collision-detection-to-the-game.html)
- [Functional difficulty levels](/entries/learn-functional-programming-with-me---functional-difficulty-levels.html)
- [It needs to be more composable](/entries/learn-functional-programming-with-me---but-rob,-it-needs-to-be-more-composable.html)
- [Implementing records](/entries/learn-functional-programming-with-me---implementing-records.html)

### Let's go back to the core problem

My code isn't very composable, we're passing buckets of state around the place and not being selective about what functions actually need what state. This means I end up with lots of weird access patterns over my data and it feels a bit brittle.

What are the problems with this?

- The code feels brittle, changing the structure of my ball of state means breaking a lot of code
- some of the code is hard to read because of the weird access patterns

I can organise some of this stuff by making records and operations over those records, or I can... well not. It seems to me that the best way to think about composability is to look at the functions I've already got and only pass in the state I need to those functions to make them work.

That'll break some stuff but working back from that perhaps I'll be able to unravel some of the bad patterns I've ended up with.

While I'm at it, I'm going to carry on pushing methods into the record and make some smaller operations for those states.

I think at the end of this, I'll end up deciding I don't need the records but we'll see.

### Inverting the enemies position

    (defn update-direction [state]
      (if (enemies-reached-edge (:enemies state) (:direction state))
        (invert-enemies-direction state) state))

    (defn invert-enemies-direction [state]
      (assoc state 
            :direction (* (:direction state) -1)
            :enemies (map 
                        (fn [enemy] (assoc enemy :y (+ (:y enemy) (enemy-descent-speed state))))
                          (:enemies state))))


There are actually two things going on here

- Changing the direction so the invaders go the other way
- Bringing the enemies down one notch

I'm okay with these two things being in this one function, the chain that I'm following up is pretty simple from here but there is quite a lot going on in here that I could perhaps factor out into more basic building blocks.

    (defn move-enemy-down [enemy] 
      (assoc enemy :y (+ (:y enemy) (enemy-descent-speed state))))

    (defn invert-enemies-direction [state]
      (assoc state 
            :direction (* (:direction state) -1)
            :enemies (map move-enemy-down (:enemies state))))

And is there a chance I could make that move-enemy-down function a little more happy?

Well I could make a protocol that describes a moveable "thing"

    (defprotocol Mobile
      (move [this dx dy]))

And implement it 

    (defrecord Rect  [x y w h]
    Bounds
      (left [this] x)
      (right [this] (+ x w))
      (top [this] y)
      (bottom [this] (+ y h))
    Mobile
      (move [this dx dy]
        (assoc this 
        :x (+ (:x this) dx)
        :y (+ (:y this) dy))))


That along with splitting up this functionality has a dramatic impact on what this code looks like

    (defn move-enemy-down [enemy] 
      (move enemy 0 enemy-descent-speed))

    (defn switch-direction [direction]
      (* direction -1))

    (defn move-enemies-to-next-row [state]
      (assoc state 
            :direction (switch-direction (:direction state))
            :enemies (map move-enemy-down (:enemies state))))


Again, I feel I could very much achieve this without the record/protocol but that's the direction I've chosen to go in for now. I also wonder if I could do without my 'move-enemy-down' function as there is probably a clever way of using apply or something similar to take this indirection for me.


# Taking the state out of the equation

I've got tons of methods where I'm being super lazy and  just passing the whole game state in.

    (defn update-player [state]
      (let [left (@key-states 37)
            right (@key-states 39)]
        (cond (= left true) (update-in state [:player :x] #(- % (player-speed state)))
              (= right true) (update-in state [:player :x] #(+ % (player-speed state)))
              :else state)))

I feel if I'm going to move towards this goal of "composability", then removing this from as many places as I can might be a good start. (This is nothing to do with my record usage or protocols at this point)

    (defn update-player [player level]
      (let [left (@key-states 37)
            right (@key-states 39)]
        (cond (= left true) (move player (player-speed level))
              (= right true) (move player (negate (player-speed level))
              :else player)))

This makes it really clear that this function is all about hte player, and needs the level to help with this work.

Unfortunately this means that the responsibility for this is passed up the chain to my update function

    (defn update-state [state]
      (validate-end-conditions
        (update-bullets
          (update-player
            (update-enemies
              (update-direction state))))))

Oh well, this isn't the end of the world, I suspect this will get cleared out if I rinse and repeat towards the goal of having a 'collection of entities' with polymorphic behaviours or whatever.

```clojure
    (defn update-state [state]
      (validate-end-conditions
        (update-bullets
          (assoc state :player (update-player
            (update-enemies
              (update-direction state)) (:level state))))))
```

*shudder*, this is a temporary state of affairs while I work this stuff out

### To its natural conclusion

I carry on with this process until all functions only take in the state they need to do their job, and also make quite a few functions in the process. 

This brings me to a file with the ugliness removed and another ugliness added in its place - I'm hoping in the next entry when I go all polymorphic on its ass that this will go away.
