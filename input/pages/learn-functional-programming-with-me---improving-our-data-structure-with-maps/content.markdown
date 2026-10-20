Carrying on..

- [Mission statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)
- [Improving my workflow](/entries/learn-functional-programming-with-me---improving-my-workflow.html)
- [Creating lots of state](/entries/learn-functional-programming-with-me---adding-lots-more-state.html)
- [Mutating lots of state](/entries/learn-functional-programming-with-me---mutating-lots-of-state.html)

I mentioned in the [last entry](/entries/learn-functional-programming-with-me---mutating-lots-of-state.html) that I was uncomfortable passing vectors around the place, because it means all of the functions that take in these vectors dependent on the *order* of information in these vectors and it's not at all clear what we're passing around.

I've been pointed in the direction of Maps and Records, and for now I'll start with Maps as they seem the most lightweight way of improving this situation.

Let's look at where we create our initial state first, as we'll refactor our way up from there.

    (defn initState []
     [
       1
       (for [x (range 0 16 2)
             y (range 0 8 2)]
         [(* x 30) (* y 30) 20 20]
       )
     ]
    )

What I'm doing here is creating a vector that looks like this

    [ 1 [0 0] ]

What is 1? What is the second vector? What is this nonsense?

How about returning a map instead of a vector from initState?

    (defn initState []
     { 
       :direction 1
       :enemies (for [x (range 0 16 2)
                      y (range 0 8 2)]
                   [(* x 30) (* y 30) 20 20]
       )
     } 
    )

So the difference is we're now using curly braces, and specifying key-value pairs for our values. (Bear in mind that this returned map is for all intents and purposes going to be immutable, so we haven't got to worry too much about exposing this state (I think)

Well actually, the rest of this is bit opaque as well, why not split out the enemy constructor into its own function?

    (defn initEnemy [x y w h]
     [(* x 30) (* y 30) 20 20]
    )

    (defn initState []
     { 
       :direction 1
       :enemies (for [x (range 0 16 2)
                      y (range 0 8 2)]
                  (initEnemy x y 20 20)
       )
     } 
    )

And if we've gone that far, why not go further and make that a map as well?

    (defn initEnemy [x y w h]
     {
      :x (* x 30)
      :y (* y 30)
      :w w
      :h h
     }
    )

Now my data structure looks like this

    {
      direction: 1
      enemies: [
        { x: 0, y: 0, w: 0, h: 0 },
        { x: 0, y: 0, w: 0, h: 0 },
        { x: 0, y: 0, w: 0, h: 0 },
        { x: 0, y: 0, w: 0, h: 0 },
        // etc

      ]
    }


Bloomin' marvellous. Now, this all still builds - although if I run it it'll fall over in disaster. This is because of mis-matched function calls, as I'm passing these maps around and the functions are expecting vectors.

Type safety was mentioned in relation to Maps and Records and perhaps Records give me some of that, I'll look at those later if it becomes a burden.

I need to update functions where I'm using these data structures (which is everywhere), so let's have a look how I'll do that:

    (defn tick [ctx state]
      (let [[dir enemies] state]
 
Well, here is my first change, I'm passing in 'state' to 'tick', but the de-structuring assignment assumes that state is a vector and un-packs it accordingly, this is a valid operation against a map but is junk to my code so nothing happens.

Instead what I want to do is unpack the state I need from the map so I can use it.

    (defn tick [ctx state]
      (let [enemies (:enemies state)]

Now, this is going to say, let 'enemies' equal the result of the value found at the key :enemies, so this is how we access maps.

'*enemies*' is now a sequence of maps, and in order to render I'll have to un-pack that too if we're taking a direct approach to getting this code working again.

    (doseq [enemy enemies] 
      (let [{:keys [x y w h]} enemy]
        (drawSquare ctx x y w h)
      )
    )

In this case, I'm unpacking several keys from enemy at the same time using the special form ":keys", that's kinda cool.

Our call to doLogic remains unchanged, as we merely pass in the state and expect the new state back in the same form.

    (js/setTimeout (fn []
      (tick ctx (doLogic state))
    ) 33  )

However, the doLogic function expects a vector and does things with more vectors, so it'll need changing as well.

    (defn doLogic [[direction enemies]]
      [
        (getNextDirection direction enemies)
        (for [[x y w h] enemies]
          (if(= direction 1)
            [(inc x) y w h]
            [(dec x) y w h]
          )
        )
      ]
    )

I'll not bother un-packing here, at the moment everything needs all of the state, but I will split out the enemy logic into its own function for easier comprehension.

    (defn doLogic [state]
      {
        :direction (directionLogic state)
        :enemies (enemiesLogic state)
      }
    )

*Now this is the old directionLogic*, we'll need to pull our enemies and direction out of our map.

    (defn directionLogic [direction enemies]
      (if (= direction 1)
        (let [right (apply max (map (fn [[x y w h] e] x) enemies))]
          (if(> right 600) -1 1)
        )
        (let [left (apply min (map (fn [[x y w h] e] x) enemies))]
          (if(< left 0) 1 -1)
        )
      )
    )

So step by step

*Our new signature and un-packing statement*

    (defn directionLogic [state]
      (let [{:keys [direction enemies]} state]

*We can pass in :x as the function to invoke (in this case the 'map' operation will take :x out of each enemy)*

    (if (= direction 1)
      (let [right (apply max (map :x enemies))]
        (if(> right 600) -1 1)
      )

*And the same for the other direction*

      (let [left (apply min (map :x enemies))]
        (if(< left 0) 1 -1)
      )

I'll look at tidying this up in a moment, but first I want to sort out my enemiesLogic function and check that my code still works
and I still have moving yellow squares in my browser.


*Our old enemies logic*

    (for [[x y w h] enemies]
      (if(= direction 1)
        [(inc x) y w h]
        [(dec x) y w h]
      )
    )

*Well first off, we now have a function for this, so that looks like this*


    (defn enemiesLogic [state]
      (let [{:keys [direction enemies]} state

*But I'm going to do something different here*

        func (if(= direction 1) inc dec)
       ]
Rather than the old if statement, I'm going to choose a function (called 'func') based on direction to create the new x with.

*Creating the new list of enemies*

    (for [enemy enemies]
      {
        :x (func (:x enemy))
        :y (:y enemy)
        :w (:w enemy)
        :h (:h enemy)
      }
    )

So here I copy the old state out of the old enemy, and make the new state with a modified x.

Hitting F5 in my browser

<img src="/img/yellow_squares.png" alt="A load of yellow squares" title="Space invaders">

Success.

**Summary**

I'm slightly happier with maps, as it means the order in which I put data into these structures becomes irrelevant, my OO self is crying out though because I'm passing around effectively property bags on which I'm performing decisions and logic from.

It feels as if I'm going to be repeating myself a lot if I have to keep addressing state this way, and hopefully I'll discover a better way as I progress, as this just feels like procedural code written in Clojure.
