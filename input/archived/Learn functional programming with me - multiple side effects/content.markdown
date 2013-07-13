Next up, what I want to do with my Space Invaders game is to have other side effects taking place as a result of shooting enemies.

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


Side effects are an interesting one, we have an activity which is the collision of a bullet with an enemy. There are several side effects as a result of this.

- Destruction of the enemy
- Destruction of the bullet
- Increase of the score
- Potential restart of the level
- Creation of an explosion where the enemy used to be

Now, we actually have 3/5 of these already, although they're not being explicitly modelled as side effects of a collision (they're just reading the state to work out whether stuff needs to happen)
