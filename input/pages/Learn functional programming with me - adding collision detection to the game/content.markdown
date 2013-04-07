Can't have a game without collision detection now can we? Let's see about getting our bullets to collide with the enemies and see what problems get in my way this time...

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

**The collision detection itself**

Well, collision is a nice easy pure function to write and because I have my rect primitive I can pretty use these.

My first stab looks like this

    (defn collides-with [one two]
      (let [one-left (:x one)
            one-right (+ (:x one) (:w one))
            one-top (:y one)
            one-bottom (+ (:y one) (:h one))
            two-left (:x two)
            two-right (+ (:x two) (:w two))
            two-top (:y two)
            two-bottom (+ (:y two) (:h two))]
        (cond (< one-right two-left) false
              (> one-left two-right) false
              (< one-bottom two-top) false
              (> one-top two-bottom) false
              :else true)))

I kinda feel as if this is over-doing the point a little though, and those extra variables I've gone and made could probably just be represented by functions.

    (defn rect-right [rect] (+ (:x rect) (:w rect)))
    (defn rect-bottom [rect] (+ (:y rect) (:h rect)))

    (defn collides-with [one two]
        (cond (< (rect-right one) (:x two)) false
              (> (:x one) (rect-right two)) false
              (< (rect-bottom one) (:y two)) false
              (> (:y one) (rect-bottom two)) false
              :else true))


It's a small change, but if I want to use these concepts elsewhere then I'll be able to far easier and this is far easier on the eyes.

**Using the collision detection**

Okay so I'm being quite lazy here, the efficient way to do collision detection across a scene is to use a linear hashmap and only test rects in adjacent cells. I only have a few space invaders though so I'll take the hit of a more brute force solution (although I think this would be quite fun to implement in a functional manner so I'll come back to it)

So, obviously I need to adjust my sequences based on whether bullets are intersecting with aliens.

This is probably the complicated bit, and I'll start off being really lazy about how I evaluate this.

- We need to remove bullets from the collection if they're intersecting
- We need to remove aliens from the collection if they're intersecting
- *in the future we'd need to increase points*
- *in the future we'd need to show some animation*

I'm getting the feeling that doing all of this with my ball of state is going to get un-wieldy and I might want to look into something more functional [like in this series](http://prog21.dadgum.com/23.html).

Nevertheless, I'll press forwards with my current solution because I want something that works.

**My first attempt**

My first attempt was a failure, I thought it'd be simple to just loop through several times and do something like

    (defn bullet-collides-with-enemy [bullet state]
      (not (not-any? 
         (fn [enemy] (collides-with enemy bullet)) 
         (:enemies state))))

    (defn enemy-collides-with-bullet [enemy state]
      (not (not-any? 
         (fn [bullet] (collides-with bullet enemy)) 
         (get-in [:bullets :active] state))))

And filter out enemies/bullets in the seq for these things, this is an awful idea because of multiple iterations it's also an awful idea because written in this way it's hard to follow.

**Second attempt**

A better idea would be to iterate once, ask for all the colliisons that have taken place and then use this information to create a new version of the state without the affected enemies/bullets in it.

This moves me closer in the direction described by the article I linked above so I'll give it a go.

I can write a function that does this and returns a list of the collisions.

    (defn get-bullet-enemy-collisions [state]
      (for [bullet (active-bullets state)]
        { :bullet bullet
          :alien (first (enemies-colliding-with-bullet bullet state))}))

But this won't actually be effective I don't think, because I'm returning a map of maps which would need comparing against the other maps in order to remove them from the collection they're in.

By-reference comparison won't work, maps are (as I understand it) compared by value - and while this would work it's not "correct". Identity is what we want to compare against here and our bullets and aliens don't have identies.

**Third attempt**

I've found [map-indexed](http://clojuredocs.org/clojure_core/clojure.core/map-indexed) and [keep-indexed](http://clojuredocs.org/clojure_core/clojure.core/keep-indexed), which will allow me to do pretty much the same as above but return a list of indexes into the collections which represent collisions.


    (defn get-bullet-enemy-collisions [state]
      (keep-indexed #({ :bullet %1
                        :alien (first (enemies-colliding-with-bullet bullet state))})
                    (active-bullets state)))

    (defn enemies-colliding-with-bullet [bullet state]
      (keep-indexed #(if (collides-with %2 bullet) %1) (:enemies state)))


I've also discovered that little # symbol, which appears to be shorthand for an anonymous function with a 1-indexed list of arguments.

Anyway, now I have a list of collisions that have taken place in my game world, I can filter out the items I don't want any more.

I couldn't figure out an easy way of using this data though, so back to

**Option one again**
    
    (defn collide-bullets [state]
      (assoc 
        (assoc-in state [:bullets :active]
          (remove #(collides-with-any % (:enemies state)) (active-bullets state)))
        :enemies
          (remove #(collides-with-any % (active-bullets state)) (:enemies state))))

    (defn collides-with-any [one, others]
      (some #(collides-with % one) others))

Iterate through the list twice and just remove any affected entities.

Note I can use the old verson of the state when doing my *collides-with-any* calls, and stick the results into multiple calls on top of the new state. This is much easier to understand than my first attempt however so at least doesn't have the pitfall of being completely opaque.

I'm still really not liking this solution as it involves multiple iterations of the two collections:

- Compare each bullet against all the enemies
- Compare each enemy against all the bullets

I'll come back to this no doubt when it comes to adding scoring, explosions and sound, because I'm going to need to execute a lot more logic based on these collisions.

I'm going to press on and add some more gameplay though, as once I have an end-to-end game I'll hopefully have gained more understanding and be able to reason about all of this better.












