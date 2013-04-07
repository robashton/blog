It's time to accelerate my efforts in getting the space invaders game have some sort of end-to-end story. Part of this is having the space invaders dropping down and firing back at our hero.

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

**I did a tidy up**

I got rid of my stupid nested structures, and put the data for bullets into the top-level map, instead of

    [:bullets :active]
    [:bullets :lastFiringTicks]

I now have

    [:bullets]
    [:last-firing-ticks]

It makes things much simpler, the code at the start of this entry can be found [here](https://github.com/robashton/clojure-spaceinvaders/blob/085c0249e54c19d189dfba6e006205c90914fc56/game.cljs).

**The space invaders need to invade some space**

When the invaders reach the end of the row, their direction changes - they also need to drop down by a certain amount of space and thus attack the earth.

    (defn update-direction [state]
      (let [{:keys [direction enemies]} state]
        (if (= direction 1)
          (let [right (apply max (map :x enemies))]
            (if(> right 600) (assoc state :direction -1) state))
          (let [left (apply min (map :x enemies))]
            (if(< left 0) (assoc state :direction 1) state)))))

I guess what I'll do here is break this up a bit (which I'm okay with anyway because the above is quite hard to read)

    (defn rects-max-x [rects]
      (apply max (map :x rects)))

    (defn rects-min-x [rects]
      (apply min (map :x rects)))

    (defn enemies-reached-edge [enemies direction]
      (cond (and (= direction 1) (> (rects-max-x enemies) 600)) true
            (and (= direction -1) (> (rects-min-x enemies) 0)) true
            :else false))

    (defn invert-enemies-direction [state]
      (assoc state :direction (* (:direction state) -1)))

    (defn update-direction [state]
      (if (enemies-reached-edge (:enemies state) (:direction state))
        (invert-enemies-direction state) state))


Now I've done this, it should be fairly easy to update their y positions as part of that invert direction call.

    (defn invert-enemies-direction [state]
      (assoc state 
             :direction (* (:direction state) -1)
             :enemies (map 
                        (fn [enemy] (assoc enemy :y (+ (:y enemy) 50)))
                          (:enemies state))))

Fairly sure there are some tricks I'm missing here for making that sort of thing prettier, but it does the job pretty well and my invaders come to say hello. 

- Associate this expression with the enemies field in the map
- The expression is a map of the current enemies field
- The map associates a modified 'y' with each enemy

**Ending conditions**

Two basic conditions now present themselves for victory or defeat

- Enemies reach the bottom of the screen
- Enemies are all destroyed

Taking the easiest approach here, I'll just perform a page re-direct on failure, and start the next level on success.

    (defn update-state [state]
      (validate-end-conditions
        (update-bullets
          (update-player
            (update-enemies
              (update-direction state))))))

and

    (defn validate-end-conditions [state]
      (cond (enemies-are-all-dead (:enemies state)) (start-next-level)
            (enemies-are-at-the-gate (:enemies state)) (show-game-over)
            :else state))

Now obviously *show-game-over* isn't going to actually return any state, but we'll be on a different page then so that's no big deal. *start-next-level* will have an opportunity to create completely new state for our next level so that works out nicely too.

**Game over dude**

    (defn enemies-are-at-the-gate [enemies]
      (> (apply max (map :y enemies)) 400))

    (defn show-game-over []
      (set! (. js/document -location) "gameover.html"))

This is another example of how delightfully terse Clojure can be at times :)

**The enemies are dead, long live the enemies**

    (defn enemies-are-all-dead [enemies]
      (not (first enemies)))

Apparently nil and false in Clojure are our only falsy values, so the above will work (first returns nil if the sequence is empty).

    (defn start-next-level []
      (create-state))

For now, let's just go with a re-start of the whole thing when we've killed all the enemies so everything starts from the beginning once more.


**Difficulty levels**

Now we have game over and next level, let's look at next level and what it means.

Currently I have a few hard-coded values lying around

- How far do enemies drop down each pass
- How fast do enemies move from left to right?
- How frequently do our bullets fire?
- How fast are our bullets?
- How fast does our defender move?

A lot of the time in my JS games, this is all modelled as state local to my entities and this is quite awkward. I can probably do better here.

The only piece of state I need for any of this is "current level", so I'mma go ahead and stick that around

    (defn create-state [level]
    { :direction 1
     :level level
     :enemies (for [x (range 0 480 60)
                    y (range 0 240 60)]
                (create-rect x y 20 20))
     :player (create-rect 200 430 20 20)
     :bullets () 
     :last-firing-ticks 0})

    (defn start-next-level [state]
      (create-state (inc (:level state))))

    (defn ^:export init []
      (hook-input-events)
      (let [ctx (context 640 480)] 
        (tick ctx (create-state 1))))

We'll start at level 1, and then when creating the new state for the new level, we'll pass in level+1.

Now then, how to use this for firing rate?

    (if (= (rem (:last-firing-ticks state) (firing-rate state)) 0)

and enemy movement

    func (if(= direction 1) #(+ % (enemy-speed state)) #(- (enemy-speed state))

and bullet speed

    (update-in bullet [:y] #(- % (bullet-speed state))))))

and for the player movement?

    (defn update-player [state]
      (let [left (@key-states 37)
            right (@key-states 39)]
        (cond (= left true) (update-in state [:player :x] #(- % (player-speed state)))
              (= right true) (update-in state [:player :x] #(+ % (player-speed state)))
              :else state)))


And the implementation (for now) 

    (defn firing-rate [state] (min 15 (- 30 (* 2 (:level state)))))
    (defn enemy-speed [state] (:level state))
    (defn bullet-speed [state](:level state))
    (defn player-speed [state] (* 2 (:level state)))
    (defn enemy-descent-speed [state] 25)


This is quite tidy and gives me the ability to adjust these values easily when experimenting with the game. 

I'm not so happy with passing the complete 'state' into each of these methods, but pulling out the level at the point of invocation seems a greater evil in this case.

Next up, I'll look at scoring as a side effect of enemy destruction, and re-visit how I manage the logic and state around that.



