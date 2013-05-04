I'm writing up my notes on Kotlin and this is the second entry

- [Looking at Kotlin - getting started](/entries/looking-at-kotlin---a-few-notes-made.html)
- Looking at Kotkin - Classes and Functions

What better place to start investigating Kotlin than classes and functions, the basic constructs in any Javariffic language.


*Taking the 'ction' out of function*

What I immediately like about Kotlin that the hello world example (beyond the laborious exercise of getting it compiled) is really simple.

    fun main(args : Array<String>) {
      println("Hello world")
    }

There are no import statements, I haven't had to create a containing static class with a static method etc, I've not had to tell it where println is coming from and I haven't had to write 'function' which makes it immediately 60% better than JavaScript and 260% better than Java.

I'm not sure how modules work in Kotlin yet, or how you'd share these functions between projects (That means getting into some Javariffic cruft I imagine), but I'm able to create other files in the project with more functions like so:

*secondary.kt*

    fun doSomething() {
      println("I'm doing something")
    }

and call them from my other files ala

*main.kt*

    fun main(args : Array<String>) {
      doSomething()
    }

Without dancing through any hoops so that's cool.

In JavaScript I'd be using require statements for this, which is great because dependencies are made explicit and I can follow them manually. 

In a tooling-oriented language like Kotlin we can rely on the intellisense to tell us where stuff came from so this makes sense. (Although it makes it less ideal for vimming it up)


*Class action Kotlin*

Classes are interesting in Kotlin, they're rather reminiscent of constructor functions in JavaScript

    class Person(age: Int, name: String) {

    }

and to create it (notice the lack of a new key-word)

    var person = Person(27, "Rob")

  
However, we start to encounter differences almost immediately - let's try adding a function

    class Person(age: Int, name: String) {
      fun sayHello() {
        println("Hi, I am $name, and I am $age years old")
      }
    }

Ignoring the string interpolation at this point (any good sane language has this good feature these days) age and name don't actually exist at this point, we have to declare them as fields in the class and to do this we do:
    
    class Person(age: Int, name: String) {
      var age = age
      var name = name

      fun sayHello() {
        println("Hi, I am $name, and I am $age years old")
      }
    }

Or short-hand:

    class Person(var age: Int, var name: String) {

      fun sayHello() {
        println("Hi, I am $name, and I am $age years old")
      }
    }


This feels a bit weird and unnecessary (if I understand it correctly), I'm all for having to explicitly declare those properties if we want to access them externally but I don't see why we'd be passing state in if we didn't want to use it.

Now we've declared those as vars, we can access them externally too, because those vars are public and write-only by default.

    fun main(args: Array<string>) {
      var person = Person(27, "Rob")
      println("I can access ${person.name} from here")
    }

We actually have an immutable key-word too, which is 'val', we can make read-only properties therefore with:

    class Person(val age: Int, val name: String) {

      fun sayHello() {
        println("Hi, I am $name, and I am $age years old")
      }
    }

*Access control in classes*

I'm not a huge fan of public state in classes, so how do we make these things private?

    class Person(private var age: Int, private var name: String) {

      fun sayHello() {
        println("Hi, I am $name, and I am $age years old")
      }
    }

Yeah, they're public by default, this is a weird design decision - if we were looking to correct the sins of badly written Java programs this is not how I'd start.

In fact, the notion of data-only objects is baked right into Kotlin, this is a data-bag or struct in this language:

    data class Person(val age: Int, val name: String)

Notice the data key-word in front of class, theoretically this means that Kotlin will generate a toString and equals check for you (although this didn't work on my machine).

**What I'd do differently**

There is a fundamental difference between data bags and objects, and I'm fine with both of them existing. I don't think we even need all those key-words or constructs to make it happen. 

*Want private state in a behavioural object?*

    class Person(age: Int, name: String) {
      fun someBehaviour() {

      }
    }

That should be the default as far as I'm concerned.

*Want to expose that state?*

Make it explicit,

    class Person(val age: Int, val name: String) {
      fun someBehaviour() {

      }
    }

*Want to expose that state as mutable?*

    class Person(mutval age: Int, mutval name: String) {
      fun someBehaviour() {

      }
    }

*Want immutable value data bags?*

    class Person(val age: Int, val name: String)

No behaviour = data bag. In fact, I'd tolerate the data attribute in this case too although I feel that the more key-words a language has the less likely I am to want to use it.

*I don't like lots of key-words*

In fact, you know what? What's the difference between a function and an object? We'll cover some of this later but I don't really see why we need the class key-word at all, the above would be fine as

    fun Person(age: Int, name: String) {
      fun someBehaviour() {

      }
    }

One of the reasons I quite like Clojure is that everything is just a function, and looking up how to do something is *always* a case of looking up an appropriate function. Simplicityt comes in many forms.
    
Reduce towards simplicity and have some opinions, that's all I'm saying, I'm a big fan of JavaScript's lack of features, I'm actually a fan of Java's lack of features too, if we could just fix some problems in those languages then we'd be great.

I feel with just what I've shown here, we've just made it even *easier* for Java developers to carry on *doing it wrong*, when there is actually a good opportunity to encourage them to go in a better direction with the code. 

A default towards private and immutable would have been a logical choice and I don't know why this isn't the case.


