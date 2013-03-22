Now we have some enemies, let's add our player and see how we might achieve the handling of keyboard events to move him.

- [Missing statement](/entries/learn-functional-programming-with-me---a-mission-statement.html)
- [Drawing a square](/entries/learn-functional-programming-with-me---drawing-a-square.html)
- [Moving the square](/entries/learn-functional-programming-with-me---moving-the-square.html)
- [Attributes and vectors](/entries/learn-functional-programming-with-me---attributes-and-vectors.html)
- [Improving my workflow](/entries/learn-functional-programming-with-me---improving-my-workflow.html)
- [Creating lots of state](/entries/learn-functional-programming-with-me---adding-lots-more-state.html)
- [Mutating lots of state](/entries/learn-functional-programming-with-me---mutating-lots-of-state.html)
- [Improving our data structure with maps](/entries/learn-functional-programming-with-me---improving-our-data-structure-with-maps.html)

Well, first off let's create the player


     :player (initPlayer 0 430 20 20)

Where

    (defn initPlayer [x y w h]
     {
      :x x
      :y y
      :w w
      :h h
     }
    )

There are some similarities here between the player and the enemy, and perhaps I'll address that when I'm a position to tell how.

Now, I'll want to render that player, and I'm actually going to take the bold step of pulling out a render function so my main loop now looks like this:

    (defn tick [ctx state]
      (clearScreen ctx) 
      (renderScene ctx state)
      (js/setTimeout (fn []
        (tick ctx (doLogic state))
      ) 33  )
    )

Just keeping things tidy!

    (defn renderScene [ctx state]
      (enemiesRender ctx state)
    )

    (defn enemiesRender [ctx state]
      (let [enemies (:enemies state)]
        (doseq [enemy enemies] 
          (let [{:keys [x y w h]} enemy]
            (drawSquare ctx x y w h)
          )
        )
      )
    )

Right, so now I have a player, I may as well render it


    (defn renderScene [ctx state]
      (enemiesRender ctx state)
      (playerRender ctx state)
    )

    (defn playerRender [ctx state]
      (let [player (:player state)]
        (let [{:keys [x y w h]} player]
          (drawSquare ctx x y w h)
        )
      )
    )

And I'll need to copy it across to the new state each iteration

    (defn doLogic [state]
      {
        :direction (directionLogic state)
        :enemies (enemiesLogic state)
        :player (:player state)
      }
    )

Nothing new here, this'll just give me an additional yellow square on my canvas...

<img src="/img/player_added.png" alt="An additional yellow square on a canvas" title="The player has been added">


**Handling those input events**

Now I want to move it though, and this is where things are going to get fun, from what I can *imagine* off the top of my head (I'm currently sat on a plane at 10,000ft with no internet so I can't look anything up), my options are:

- Attempt to mutate player state directly as input events happen
- Keep some mutable state around to indicate whether certain keys are currently down, and apply these modifiers as part of logic

I think I'm going to opt for the second one and see how that plays out. It kinda makes sense to me in ignorant-land that isolating areas of mutable state from my (currently quite) clean program will be helpful.

This is pretty much what the structure will look like:

    tick (state) =>
      inputs = getCurrentInputs()
      tick applyLogic(state, inputs)

I don't have to care that behind that getCurrentInputs is some horrible interop with the browser and piles of mutability, and if I work out how to do it better later it should limit the area of change needing to be applied.
    










