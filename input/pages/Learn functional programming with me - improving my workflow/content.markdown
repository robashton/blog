So I ended up picking this up again towards the end of March (yes I know, it's near the end of May now, I've ended up writing and doing a lot...)

This is where I was up to last time:

- [Missing statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)


And the Github repo is here: [github.com/robashton/clojure-spaceinvaders](https://github.com/robashton/clojure-spaceinvaders)

I advise you pop back and read those if you're just tuning in, as I've had to as well in order to start up again!


**Improving my workflow**

I'm writing this after I've learned a lot about managing my workflow to keep myself interested in tasks, and I'm writing this after writing a silly number of posts about writing an [OData parser in OMeta](/entries/ometa-odata-odear---polishing-it-off.html), which reminded me an awful lot of what functional programming was like, so I came back here.

So, what have I done to make my life easier with this Clojure malarkey?

Well, I want faster feedback, I don't want to have to keep spawning up Java and I don't want any manaul build process.

Here is what I came up with (I know there is a repl and stuff, but I honestly just want to get some Clojure written at this stage and this seemed nice)

I'm using [cljs-watch](https://github.com/ibdknox/cljs-watch), and I've written a Makefile that looks like this:

    default: build
    all: build

    build:
      cljsc game.cljs \
          '{:optimizations :simple :pretty-print true}' \
          > ./game.js

    develop:
      cljs-watch game.cljs \
        '{:optimizations :simple :pretty-print true :output-to "./game.js"}'
     

Hacky, but if I type "make develop", I can get on and write code without having to worry about compilation so I'm happier with life, now I just make changes and hit refresh and they're there.

**Reminding myself of where I was**

*First off, I specify that I'm sticking this all in the namespace 'game'*

    (ns game)

*I define a method called 'context', which takes no args*

    (defn context []

*I let 'target' be thbe result of calling .getElementById (interop)*

      (let [target (.getElementById js/document "target")]

*Return a 'vector' containing the context, the width, and the height*

        [
          (.getContext target "2d") 
          (. target -width)
          (. target -height)
        ]
      )
    )

*Define a function called clearScreen, taking in a vector of 'ctx, width, height' - see above*

    (defn clearScreen [[ctx width height]]

*Set a property on the context of fillStyle '#FFF'*

      (set! (. ctx -fillStyle) "#FFF")

*Call clearRect on the context, with 0,0,width,height*

      (.clearRect ctx 0 0 width height) 
    )

*Same again, only parameterised so we're drawing a square*

    (defn drawSquare [[ctx width height] x y w h]
      (set! (. ctx -fillStyle) "#FF0")
      (.fillRect ctx x y w h) 
    )


*Now we have a function called tick which will call clearScreen over and over again, with drawSquare over again*

    (defn tick [x]
      (let [ctx (context)] 
        (clearScreen ctx) 
        (drawSquare ctx x 0 100 100)  
        (if (<= x 1000) 
          (js/setTimeout (fn []

*And every frame, we call tick with a new version of the state, in this case an increased 'x'*

            (tick (inc x)) 
          ) 33  )
        )
      )
    )

*We export a function called 'init' so I can call this from JS and make the game happen*

    (defn ^:export init []
      (tick 0) 
    )


Wow. There is a lot to (re)-take in here.

First off, some observations

- Dom interop is horrible
- I remember now that I had state issues, I think I had that question answered on Github though, so we'll have a look at that next

ONWARDS WITH THIS PROJECT


