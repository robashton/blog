Having uncovered the need for composability, and thought a little bit about what that means, there is an obvious refactoring route to now take...

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

This is the point where I wish I'd written some tests like I usually do when learning something hyper-new, but I'll have to make do with hitting refresh and seeing if "stuff is broke" *OH WELL ONWARDS*

### Making records

I've decided that I'm going to have a record describing a rect, and start building up behaviour over the top of it, this looks something like this:

    (defrecord Rect  [x y w h]) 

So far so good, "this is the name of my record and these are the fields that it has", I guess the next thing to do is see about slotting it in and break all my code.

I have a function for creating a rect already

    (defn create-rect [x y w h]
    { :x x
      :y y
      :w w
      :h h })

So how to swap out this code? Well it turns out that once you have a record they can be used just like the maps I've been using so far.

    (defn create-rect [x y w h]
      (Rect. x y w h))

Re-compiling gives me the same old red square defending the world against yellow blocks we've come to know and love. Obviously what I'm better off doing is replacing all the calls to create-rect with that call so that's what I do. So far the only advantage I can see of this is that I get "slightly better performance on field look-up" (which I don't even know if that applies to the compiled JS)

### Methods with protocols

Can I add methods that are now explicitly for this record?  Will this help with the readability of some of my code? Starting with something pure is probably a good idea as the amount of code I'm going to have to cut away will be small. Here is a function which determines whether two rects are over-lapping

    (defn collides-with [one two]
      (cond (< (rect-right one) (:x two)) false
            (> (:x one) (rect-right two)) false
            (< (rect-bottom one) (:y two)) false
            (> (:y one) (rect-bottom two)) false
            :else true))

I've a couple methods here for 'bottom' and 'right', can I put them on the Rect?

    (defn rect-right [rect] (+ (:x rect) (:w rect)))
    (defn rect-bottom [rect] (+ (:y rect) (:h rect)))


Well no, but I can make a protocol for these things and then make implementations for them on the record (this effectively maps to an interface/class in Java land)
    
    (defprotocol Bounds 
      (left [this])
      (right [this])
      (top [this])
      (bottom [this]))
    
Implementing this is a simple matter of providing methods in the record for this protocol

    (defrecord Rect  [x y w h]
    Bounds
      (left [this] x)
      (right [this] (+ x w))
      (top [this] y)
      (bottom [this] (+ y h))) 

Calling these is kinda magical

    (defn collides-with [one two]
      (cond (< (right one) (left two)) false
            (> (left one) (right two)) false
            (< (bottom one) (top two)) false
            (> (top one) (bottom two)) false
            :else true))

Well that's certainly pretty, but I could have probably made that happen with this instead!

    (defn left [rect] (:x rect))
    (defn top [rect] (:y rect))
    (defn right [rect] (+ (:x rect) (:w rect)))
    (defn bottom [rect] (+ (:y rect) (:h rect)))

Which isn't as pretty, but gives me the same result and doesn't come with the explicit interface stuff.

### I'm still not sure about records

Before I got to protocols, I wasn't sure why I'd be declaring explicit records over just using maps. I can still see that I could just use methods over maps and forego records and protocols altogether.

In this vein, my foray into records hasn't really helped me generate "[more composable](/entries/learn-functional-programming-with-me---implementing-records.html)" code yet, although thinking about the overall behaviours being exposed over a certain data structure could be helpful.

I'm wondering if I can declare my bullets/invaders/player as entities in this way, and have a single collection like I would in an ordinary game/scene-graph thing.

That's skipping ahead a bit though, right now I have composability to make happen. I think I'll carry on trying to do it with records as it will co-erce me into thinking about my data structures and maybe I'll work out some advantages to these along the way.
