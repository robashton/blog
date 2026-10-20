So last time I left the program, I had a black square which moved across the page but left a delightful black smear across the window, it looked something like this:

![Black smear](/img/blackline.png)

I promised I'd add keyboard input this time around, and I don't like to lie - so while I sit here at my client's rather amazing karting day I've decided to give adding this a go as I don't really like karting that much ;-).

First things first, I need to clear my screen in between frames or I'm going to keep on having an ugly black smear instead of a moving black square, in JS this would look like.

    context.clearRect(0, 0, width, height);

Not rocket science, right?

Well, right now, I have the function for drawing a square, which gets the context and draws a square, this looks like

    (defn drawSquare [x y w h]
      (let [target (.getElementById js/document "target")
            context (.getContext target "2d")]
        (.fillRect context x y w h)
      )
    )

Great, but in order to have the moving square, I'm going need that context more than once, and making multiple requests to get the context is a bad idea because it means lots of calls to the DOM (slow).

Clearly this means I'm going to need three functions for my purposes:

    (defn clearScreen [ctx]
      (.clearRect ctx 0 0 WIDTH HEIGHT)
    )

    (defn drawRect [ctx x y w h]
      (.fillRect ctx x y w h)
    )

    (defn context []
      (let [target (.getElementById js/document "target")]
      (.getContext target "2d"))
    )

Meaning I can do

    (defn drawScene [x]
      (let [ctx (context)]
        (clearScreen ctx)
        (drawRect x 0 100 100)
      )
    )

Now, you'll have noticed (if you're actually paying attention), that I have a 'Width' and 'Height' parameter for clearScreen that I haven't bothered trying to filter - that's because they're attributes of the Canvas Element and I need to get the darned things from that element and I haven't worked out a clean way of passing these things around (or retrieving them all from a function so I have the values present at that time. (Remember that I am trying to avoid global state, and as far as I know I haven't got "Types" for storing collections of data in.

Okay - so first things first, how do I get attributes from an HTML element? HTML elements? Attributes? These aren't Clojure concepts - I can do a quick Google and see that this is possible:

    (. target -width)

According to the Wiki on Clojurescript, this is just part of the "host interop" featureset - I'm not entirely sure how this maps to typical Clojure.

Now - this still means I have the problem of how to get these out of a function, in JS I'd probably have something like


    var Rendering = function(element) {
      this.context = element.getContext('2d');
      this.width = element.width;
      this.height = element.height;
    }

Or something trite like that (see my other canvas codes in Github for what this actually looks like).

Can't do this here as far as I know, turns out I can return a vector, or a list or a set or something from a function (duh), so I can do something like

    (defn context []
      (let [target (.getElementById js/document "target")
        [
          (.getContext target "2d")
          (. target -width)
          (. target -height)
        ]
      )
    )


Which will return a vector containing

    [context width height]

I can pass this into my clearRect function for example with

    (defn drawScene [x]
      (let [ctx (context)]
        (clearScreen ctx)
        (drawRect x 0 100 100)
      )
    )

So nothing different here, but I'll need to unpack that vector in order to use it

    (defn clearScreen [ctx]
      (let [[context width height] ctx
        (.clearRect context 0 0 width height)
      )
    )
    
Which is a bit verbose, or doing a bit of research it appears I can automatically unpack that vector when I call the function ala

    (defn clearScreen [[context width height]]
      (.clearRect context 0 0 width height)
    )

I'm not entirely sure how I feel about this, I'm just passing blobs of data around and assuming their structure in my functions - does Clojure have any inference over this stuff for verifiability (I'm not sure) - seems I'm losing a lot of the safety I'd have with OO constructs (regardless of type safety or whatever).

Either way, my complete program currently looks like this:


    (ns game)

    (defn context []
      (let [target (.getElementById js/document "target")]
        [
          (.getContext target "2d") 
          (. target -width)
          (. target -height)
        ]
      )
    )

    (defn clearScreen [[ctx width height]]
      (set! (. ctx -fillStyle) "#FFF")
      (.clearRect ctx 0 0 width height) 
    )

    (defn drawSquare [[ctx width height] x y w h]
      (set! (. ctx -fillStyle) "#000")
      (.fillRect ctx x y w h) 
    )


    (defn tick [x]
      (let [ctx (context)] 
        (clearScreen ctx) 
        (drawSquare ctx x 0 100 100)  
        (if (<= x 1000) 
          (js/setTimeout (fn []
            (tick (inc x)) 
          ) 33  )
        )
      )
    )

    (defn ^:export init []
      (tick 0) 
    )

This has the effect of drawing a square, clearing the rect and drawing the square again, which means my square floats across the screen.

Seems I've once again run out of room to talk about input to this program from the keyboard, hopefully I'll reach that next entry!

I still have some pending questions from these two entries:

- The recursive setTimeout with the anonymous closure?
- Passing that vector around - is this a good idea?

Anybody feel free to chip in at any time.
