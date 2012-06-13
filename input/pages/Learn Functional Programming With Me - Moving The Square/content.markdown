So, last session I managed to get a square being drawn, as if I haven't done that a thousand times in JS - now to do something a bit more dangerous (because it involves state), and more likely to get me in trouble - let's move the square around using the keyboard.

**Reminder**: The source for all of this can be found at [github.com/robashton/clojure-spaceinvaders](https://github.com/robashton/clojure-spaceinvaders)

Parameterising the function
------------

This is a no-brainer, I'm going to need to know where to draw this thing, I start off by passing in the state required to draw a rect to my function.

    (defn drawRect [x y w h]
      (let [target (.getElementById js/document "target")
            context (.getContext target "2d")]
         (.fillRect context x y w h)           
      )
    )

So far so good - but now I'm going to need to not only pass in those variables, but - well they're going to have to be variable (somehow) because they're going to have to change each frame and it's stuff like this that makes me break out in a cold sweat because I haven't discovered the patterns to solve this type of problem yet.

Let's explain - you see that 'let' statement, the values I've defined there, those key value pairs are *immutable*, this means I cannot change them - for example, if I want to make this square move of its own voilition, the literal equivalent of

    for(var x = 0; x < 1000; x++)
      drawRect(x, 0, 100, 100);

Doesn't exist.

Now, if I were to make my best guess at how to solve this, I'd say that we need a function that takes in x, and then calls itself with x+1, for example at a conceptual level:

    function drawAndMove(x) {
      drawRect(x, 0, 100, 100)
      if(x <= 1000)
        drawAndMove(x+1)
    }
    drawAndMove(0);

This actually maps across to a construct in our chosen world that looks something like the following:

    (loop [x 0]
      (drawRect x 0 100 100)     
      (if (<= x 1000)
        (recur (inc x))
      )
    )

- I want to loop, I want the following values available in this loop (x = 0)
- Please draw the rect at x,0,100,100
- If x <= 1000, recurse and increment x

Okay, this is a bit crazy and verbose - perhaps there is a better more shorthand way of doing this (anybody care to chip in around now?), but Imma press ahead and say that actually, that is not the greatest loop of all time.


We actually have to yield to the UI thread each 'frame' which blows this whole thing out of the water - that's not to say that this code isn't functional - because I love having a row of black drawn over my canvas on the UI thread with no user interaction whatsoever - but we actually need something like the following

    function tick() {
      logic();
      render();
      setTimeout(tick, 33);
    }

Again, cutting a corner here because I don't want to write a full on game loop in clojure just yet.

Figuring it out, I've ended up with something like this

    (defn tick [x]
      (drawRect x 0 100 100)
      (if (<= x 1000)
        (js/setTimeout (fn []
           (tick (inc x))
        ) 33  )
      )
    )


    (defn ^:export init []
       (tick 0)
    )

- Define a function called tick, which takes in the current position of our object
- Draw a rect at that location
- If x is still less than 1000, then
- In 33 milliseconds, call tick again but with x+1


I've actually gone far enough for a single blog entry at this point so I'll leave keyboard input till another day - I have some thoughts about this work so far however:

- That setTimeout doo-hick is taking in an anonymous function, am I creating this function every frame? I wouldn't do that in most of my JS, it's effectively the same as creating a closure in a loop - not good?
- I saw something like this that used global state and an atom, and then tick could be called with no parameters - is this a better solution? It seems somewhat against what functional programming is about
- Thoughts?
