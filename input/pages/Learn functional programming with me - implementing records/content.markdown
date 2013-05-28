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

Re-compiling gives me the same old red square defending the world against yellow blocks we've come to know and love. Obviously what I'm better off doing is replacing all the calls to create-rect with that call so that's what I do.

### Encapsulation with records

Can I add methods that are now explicitly for this record?  At the same time can I move to stop passing that ball of state around?




