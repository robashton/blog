So.. setting up in ClojureScript isn't the most exciting thing I've ever done - however, following the instructions at the [project wiki](https://github.com/clojure/clojurescript/wiki/Quick-Start) got me set up with a basic build pipeline which allows me to include ClojureScript into the browser.

You can find what I've done and all the work so far here: [github.com/robashton/clojure-spaceinvaders](http://github.com/robashton/clojure-spaceinvaders).

Intro
----------

My mission today will be to draw a square on a canvas object, nothing more than that - as there will be quite a few concepts to learn just to get this far.

Moving on then, I'm going to start off by doing most of my work in 'game.js' and I don't think I need to export any variables to the outside world, so that keeps things tidy.

About that - as far as I see it, what I want to build is a simulation of Space Invaders, which is as stateless and as side effect free in nature (or as far as is practical - whatever that means). This simulation will be the core of everything - and the side effects will exist on the periphery, in so far as updating the canvas and interacting with the user goes.

Some basic syntax
---------------

So, how do I begin? Well, I guess I'd better get the rest of you caught up with me in understanding Clojure Syntax.

*this is a traditional function call*

    foo(x, y)

*this is a Clojure function call*

    (foo x y)

*this is a traditional function definition*

    function foo(x, y) {
       console.log(x, y)
    }

*this is a Clojure function definition*

    (defn foo [x, y]
      (log x y)
    )

This should be enough to get us started at least. Apparently some of these things we put after the first paren are not only functions, but special forms or macros - but as I haven't learned anything about them yet, I'm happy in my world of pretending they're all just functions.

So, how am I going to start off in my happy world of Clojurescript? I need to create an HTML5 Canvas Context so I can do  things with it, and in order to do that I need to need to wait for the DOM ready event and oh oh oh - oh dear, *all of this looks like it might be hard because these are all external systems to my happy ClojureScript world and will involve some initial pain - oh well I wanted to learn how to do something practical and nearly all things practical are going to involve talking to external systems so I'd better get the pain over with **OH IT HURTS IT HURTS.***

Ahem.

Getting a function called on start-up
-----------------

Ever the pragmatist, rather than re-implement the standard fuzz around this stuff in Clojure, or pull in jQuery or anything I'd do in JS, we'll just go with the body onload event, and rely on it calling a global function that I export from my game.js.

    (ns game) 
    (defn ^:export init [] 
       
    ) 

Given a body declaration of something like

    <body onload="game.init();">

That funky export thing just tells the Google Closure compiler not to mangle the name of the function (the ClojureScript compiler runs output through Closure - yes, that is confusing when you read it out loud).

How do I test it works? Well inside my init function, let's access our first native object and see what the syntax is for that.

    (.log js/console "Hello World")

Actually not too bad, "Please call .log - which exists on the object js/console, and pass in "Hello World" as parameters.

Great - so I'm now printing hello world, how are we going to draw a square to our canvas? Well first we'll need a context object, and in order to get that context object, we'll have to get a canvas object and call a method on it - this will mean having a variable of some sort from which we can gain other functionality.

Introducing some more syntax.
----------------

*Traditional variable declaration*

    var x = 0;

*Clojurescript variable declaration*

    (let [x 0] 
      (doStuff x)
    )

So, two things here - we have this 'let' thing, into which you pass in a vector of key-value pairs (x and 0 in this case, so x = 0), and then a body of code doing things with those values.

Ah yes, Clojure has vectors, which are square brackets which contain a bunch of things. (parens are just lists of things, hence 'LISP' apparently). The difference between the two? Well, for my purposes right now, one has square brackets and the other has parentheses - I'm sure it will become clearer in the future.

Drawing my square
--------------------

Anyway, expanding all of this we can get the following in order to do draw a square on our canvas

      (ns game)


      (defn drawSquare []
        (let [target (.getElementById js/document "target")
              context (.getContext target "2d")]
           (.fillRect context 0 0 100 100)
        )
      )

      (defn ^:export init []
         (drawSquare)
      )

- Create a function called drawSquare, taking in no parameters
- Let target = the result of calling getElementById on the document with a parameter of 'target'
- Let context = the result of calling getContext on target with a parameter of '2d'
- Call fillRect on context, with parameters of 0,0,100,00
- When init is called, call drawSquare with no parameters


I guess we learn something else here, which is you can make a nice little chain of lets that all feed into each other, as you'd kinda hope.

Next, we'll see about drawing a basic representation of our 'defender' and move it around using the keyboard (probably)
