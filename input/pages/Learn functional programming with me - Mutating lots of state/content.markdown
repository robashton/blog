It's all very well and good being able to render a load of squares based on a sequence of positions, but what I actually want to achieve now is moving all of those squares.

- [Missing statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)
- [Improving my workflow](/entries/learn-functional-programming-with-me---improving-my-workflow.html)
- [Creating lots of state](/entries/learn-functional-programming-with-me---adding-lots-more-state.html)


**Mutating lots of state**

Thing is, what I can't do is pop into that sequence and start *changing* state, what I want to do each frame is (I think), create a *new* sequence and pass that into the next call of 'tick', *erk*.

Well, actually this shouldn't be such a big deal, how about creating a method that takes in a sequence and returns a new sequence where each item is slightly different from the original?

    (defn doLogic [enemies]
      (for [[x y] enemies]
        [(inc x) y]
      )
    )

doLogic takes in a sequence of enemies, then uses a list comprehension to create a new list of enemies but with 'x' increased by one.

Now all we have to do is pass this new sequence into the next call of 'tick'

    (tick (doLogic enemies))

Okay, that was *too* easy, from what I see here this is also going to be lazy up until the point we call 'doseq', it makes me feel kinda weird though that we're apparently creating a completely new sequence every single frame - memory-wise this has to be a bad idea and in the JS environment this is going to make me cry.

I have no idea what is going on under the hood though, so if any Clojurescript guru can enlighten me I'd appreciate it.

**Making our invaders move left to right and back again**

Well, the first thing I actually need to do is set up my invaders properly in some sort of known 'game space', this isn't really related to the functional programming bit so I'm going to pretty much side-step over it by modifying the code and explaining what I've done. (normally I'd use a camera system and be a bit more clever, but not the point of this exercise)

*The first thing I've done is moved the ctx call outside of tick*

    (defn ^:export init []
      (let [ctx (context)] 
        (tick ctx (initState)) 
      )
    )

And actually, I'm going to specify the width and height as 640x480 and hard-code the whole thing

    (defn ^:export init []
      (let [ctx (context 640 480)] 
        (tick ctx (initState)) 
      )
    )
    
    (defn context [width height]
      (let [target (.getElementById js/document "target")]
        [
          (.getContext target "2d") 
          (set! (. target -width) width)
          (set! (. target -height) height)
        ]
      )
    )

And now I've done this, I can do a little bit of maths and say that if I make my space invaders 20x20, then I can fit 8 of them on the screen horizontally and 4 vertically (leaving space for our good guy and between them!)

    (defn initState []
     (for [x (range 0 16 2)
           y (range 0 8 2)]
       [(* x 30) (* y 30) 20 20]
     )
    )

I'm also now including a width and height with each of my little invaders, which means I'll need to modify my logic function

    (defn doLogic [enemies]
      (for [[x y w h] enemies]
        [(inc x) y w h]
      )
    )


And rendering them now looks like this

    (doseq [[x y w h] enemies] (drawSquare ctx x y w h))

Now this doesn't solve that what I want to do is change the direction of my squares when they reach the edge of the playing area, this is bit more interesting.

**Variable state changes**

Well, when do my space invaders switch direction? Well, I went on [Youtube](http://www.youtube.com/watch?v=437Ld_rKM2s) to look at the original behaviour of space invaders and it seems that if you destroy a column of invaders then it won't switch direction until at least one invader has reached the edge of the screen.

What's an efficient way of handling this I wonder, well I'll need a direction as part of my main game state so I'll get that in first

    (defn initState []
     [
       1
       (for [x (range 0 16 2)
             y (range 0 8 2)]
         [(* x 30) (* y 30) 20 20]
       )
     ]
    )

Okay, so now initState will return a vector containing a direction (1), and a sequence of enemies. I'm really not confortable with these arbitrary lists of state so I'll look for some way to make them tidier shortly I think.

    (defn tick [ctx state]
      (let [[dir enemies] state]
        (clearScreen ctx) 
        (doseq [[x y w h] enemies] (drawSquare ctx x y w h))
        (js/setTimeout (fn []
          (tick ctx (doLogic state))
        ) 33  )
      )
    )

In my tick method, I'll use a destructuring assignment to unpack this vector into a direction and an enemies sequence so I can easily go through that sequence.

    (defn doLogic [[direction enemies]]
      [
        direction
        (for [[x y w h] enemies]
          [(inc x) y w h]
        )
      ]
    )

And in my doLogic method, I'll simply re-return the diretion, and the modified sequence of enemies.

All I need to do now is return a different direction if a certain condition is true

- Are we going right? Are any of the entities too far to the right?
- Are we going left? Are any of the entities too far to the left?

Okay, how about

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
    
We'll ask another function what the next direction is going to be, if the current direction is '1', we'll inc x, anx if the current direction is not 1, we'll dec x.

I can implement getNextDirection like so, taking in current and enemies

    (defn getNextDirection [current enemies]
      (if (= current 1)
        (let [right (apply max (map (fn [[x y w h] e] x) enemies))]
          (if(> right 600) -1 1)
        )
        (let [left (apply min (map (fn [[x y w h] e] x) enemies))]
          (if(< left 0) 1 -1)
        )
      )
    )


I can say, if we're going right, check out what the max x position is, and change direction if it's greater than 600, and vice versa if it's not.

This all works, my squares go from left to right and back again and keep bouncing around.

<img src="/img/yellow_squares.png" alt="A load of yellow squares" title="Space invaders">

I have another question at this point, which is "how many times is this list being iterated, should I be optimising this some how?"

