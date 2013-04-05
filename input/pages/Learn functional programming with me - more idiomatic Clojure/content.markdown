I went through my code in the last entry and did a bit of re-factoring in an effort to get my state under control, I've also had some feedback that some of my Clojure could be a bit more idiomatic so let's sort that out.


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

**assoc-in vs update-in**

Here is some code I wrote

    (defn enemiesLogic [state]
      (let [direction (:direction state)
            enemies (:enemies state)
            func (if(= direction 1) inc dec)
           ]
        (assoc state :enemies
          (for [enemy enemies]
            (assoc enemy :x (func (:x enemy)))
          )
        )
      )
    )

What I'm essentially doing is 

- Selecting a function to apply based on the direction
- Creating a new enemies collection where that function has been applied to each enemy

Well, why bother with this? We can do

    (defn enemiesLogic [state]
      (let [direction (:direction state)
            enemies (:enemies state)
            func (if(= direction 1) inc dec)
           ]
        (assoc state :enemies
          (for [enemy enemies]
            (update-in enemy [:x] func)
          )
        )
      )
    )

update-in takes in a func to enact on the value, rather than the new value and saves us precious reading time. Thinking about it, we can actually save some effort here too.

This makes a big difference in cases where I've been lazy and done this

    (assoc bullet :y (dec (:y bullet)))

Looks better like

    (update-in bullet [:y] dec)
    
Passing around functions seems pretty functional.


**State doesn't define an object**

Why do I have this?

    (defn initEnemy [x y w h]
     {
      :x (* x 30)
      :y (* y 30)
      :w w
      :h h
     }
    )

    (defn initPlayer [x y w h]
     {
      :x x
      :y y
      :w w
      :h h
     }
    )

    (defn initBullet [x y w h]
     {
      :x x
      :y y
      :w w
      :h h
     }
    )


Seems to me that everything in my game is a rect, and what changes between these everything is the behaviour that is performed over that state.

So, death to these things and in with

    (defn initRect [x y w h]
     {
      :x x
      :y y
      :w w
      :h h
     }
    )

Actually, I'm pulling this structure apart in a number of places, such as my drawing functions

    (let [{:keys [x y w h]} bullet]
      (drawSquare ctx x y w h "#000")
    )

A solution to this would be to make drawSquare take in this map and de-construct it there. I've established that this is a primitive I want in my application and I'll use it as such.

I could further solidify this by turning it into a Record but I'm not yet feeling any pain from not having it as one so I won't bother..


    (defn drawSquare [[ctx width height] rect c]
      (set! (. ctx -fillStyle) c)
      (let [{:keys [x y w h]} rect]
        (.fillRect ctx x y w h) 
      )
    )

    (defn enemiesRender [ctx state]
      (let [enemies (:enemies state)]
        (doseq [enemy enemies] 
          (drawSquare ctx enemy "#FF0")
        )
      )
    )

    (defn bulletsRender [ctx state]
      (doseq [bullet (:active (:bullets state))] 
        (drawSquare ctx bullet "#000")
      )
    )

    (defn playerRender [ctx state]
      (let [player (:player state)]
        (drawSquare ctx player "#F00")
      )
    )


In other thoughts, these functions are all the same apart from the colour of the rect, it seems a bit daft to me, how about

    (defn renderRects [ctx rects colour]
      (doseq [rect rects] 
        (drawSquare ctx enemy colour)
      )
    )

    (defn enemiesRender [ctx state]
      (renderRects ctx (:enemies state) "#FF0")
    )

    (defn bulletsRender [ctx state]
      (renderRects ctx (get-in state [:bullets :active]) "#000")
    )

    (defn playerRender [ctx state]
      (drawSquare ctx (:player state) "#F00")
    )

Keeping the number of data structures to a minimum and re-using tiny little functions seems to be a thing in these parts.

**Naming clojure constructs**

I've been a naughy boy and stuck to my JavaScript ways of doing things with the camelCase all up in the show.

Turns out that Clojure fiends prefer hyphenation-on-their-names and I can kinda get behind that.

I'll also take the opportunity to formally start using 'rect' everywhere instead a combination of  'square' and 'rect' etc, and do a general tidy up of names.

I'll not show them all, but in essence 

    (defn bulletsRender [ctx state]
      (renderRects ctx (get-in state [:bullets :active]) "#000")
    )
 
Will become

    (defn render-bullets [ctx state]
      (render-rects ctx (get-in state [:bullets :active]) "#000")
    )

And so on (so don't get confused in latter entries where the names of things have changed!)

The only downside to this arrangement is ctrl-p is now broken in vim for method names, this is a chance for the Emacs people to point and laugh at me.


**Parenthesis white space**

I'm getting little value from new-lining my parentheses.

    (defn update-state [state]
      (update-bullets
        (update-player
          (update-enemies
            (update-direction state)
          )
        )
      )
    )

When re-factoring or moving code around, I'm using the vim shortcuts 

- vi(
- va(
- ci(
- ca(

And so on, and actually it would be easier if I could just use left and right to move to the brace I want to grab the contents of.

So, I'm going to sort that out too and go with
    
    (defn update-state [state]
      (update-bullets
        (update-player
          (update-enemies
            (update-direction state)))))

Gasp. Radical. This will reduce my vertical space usage and allow me to see more context on my screen (I'm a k+r person in C#/JS etc so I'm totally up in this way of doing things)

The fully tidied up source code can be found at [this snapshot on Github](https://github.com/robashton/clojure-spaceinvaders/blob/d7df1f4671ffe86dd2391d368e7b0169ab263fae/game.cljs) if you want to catch up with where I am so far.

With this, I can go and look at collision detection!

