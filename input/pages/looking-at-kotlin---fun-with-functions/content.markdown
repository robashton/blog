Onto some of the interesting things you can do with functions in Kotlin...

- [Looking at Kotlin - getting started](/entries/looking-at-kotlin---a-few-notes-made.html)
- [Looking at Kotkin - Classes and Functions](/entries/looking-at-kotlin---classes-and-functions.html)
- [Looking at Kotlin - Inheritance](/entries/looking-at-kotlin---kicking-the-java-inheritance-addiction.html)
- Looking at Kotlin - Fun with functions


You can have some real fun with functions in Kotlin, let's do a really basic one first:

*Extension methods*

Extension methods were exciting a few years ago when they came out in C# because they offered the possibility of creating new and innovative APIs. There is little harm in them being in Kotlin then!

An extension method on the StringBuilder class which when invoked adds a new line to the builder:

    fun StringBuilder.newLine() {
        this.append("\n")
    }


Simple enough.

**Function literals**

Any sane language has a way of declaring functions as variables. In JavaScript functions are objects just like anything else so this *just works*, in C# this ability was added as an afterthought so it is a bit ugly and Java well... it's Java.

A function that takes in nothing and returns nothing in Kotlin looks like:

    var function : () -> Unit

If we wanted to invoke this, we could do so very easily

    function()

Of course, we'd have to define it first and that looks like this

    var function : () -> Unit = {
      println("Hello world")
    }

We can be implicit if we're combining declaration and definition so..

    var function = {
      println("Hello world")
    }

I've got very few comments about this, it's pretty simples and self explanatory.

A few examples to help with the next bits

*A function take takes in a string and returns nothing*

    var print : (arg: String) -> Unit = { (arg: String) ->
      println(arg)
    }

or implicitly

    var print = { (arg: String) ->
      println(arg)
    }

*A function that takes in two numbers and returns their sum*

    var sum = { (one: Int, two: Int) -> one + two }

All of this is pretty nice, although it takes quite a bit of practice remembering what the various forms are for declaring these functions, what threw me (if I understand) is the difference between these function literals and declared functions.

With the function literals, it *seems* as though the returned value is the last statement in the function

    var doubleUp = { (val: Int) -> 
      println("do a thing")
      val * 2 
    }

where as in the declared functions it looks like this

    fun doubleUp(val: Int) : Int {
      println("do a thing")
      return val * 2
    }

I'm not sure I understand this, two different ways of defining functions (plus all the various options we have around being implicit/explicit about the various aspects of the function) made it quite confusing to pick this up and I haven't even gone through some of the examples I saw on the Kotlin website.
 
**Passing functions around**

Given the following two methods, I want to invoke applyAndPrint with an array and the doubleUp function

    fun doubleUp(value: Int) : Int {
      return value * 2
    }

    fun applyAndPrint(values: Array<Int>, modifier: (value: Int) -> Int) {
      for(i in values)
        println(modifier(i))
    }

My first thought would be 

    applyAndPrint(array(1,3,4,5), doubleUp)

But this doesn't actually work, which I find counter-intuitive - in fact I couldn't find a good way to do this other than
    
    applyAndPrint(array(1,3,4,5), {(x) -> doubleUp(x)})

I'm definitely missing something here but the docs didn't really help me out.

**Setting the context of these passed functions**

Okay, this is where things get a bit cooler in theory - what if we had some sort of context (a builder) with some state in it and we want to invoke these functions transparently on top of these state.

Understand that? Well done, I don't - but here's an example

    class Builder (val multiplier: Int) {

        fun invokeStuff(action: (Builder.() -> Unit)) {
            this.action()
        }

        fun multiply(value: Int) : Int {
            return value * multiplier
        }
    }

The important bit here is the way we've declared the type of 'action'

    action: (Builder.() -> Unit)

This is a function that returns nothing, takes in nothing but is invoked on an object of type "Builder".

This means when we use this builder like so

    var builder = Builder(10)
    builder.invokeStuff({
      var result = multiply(1)
      println(result)
    })

The context of 'this' has been set to the builder object and we can invoke functions declared within the builder.

Thing is, this context can be played with - consider that we can also create extension methods on types, let's say the Int type.

    class Builder (val multiplier: Int) {

        fun invokeStuff(action: (Builder.() -> Unit)) {
            this.action()
        }

        fun multiply(value: Int) : Int {
            return value * multiplier
        }

        fun Int.doStuff(value: Int) : Int {
          return value * multiplier
        }
  
    }


I can use this with

    var builder = Builder(10)
    builder.invokeStuff({
      var result = 1.doStuff(50)
      println(result)
    })


*The scoping of extension functions*

This is invalid code

    fun someFunction() {
        fun String.someOtherFunction() {

        }
    }

    fun someStuff() {
        "".someOtherFunction()
    }


The extension method isn't visible because it was declared inside a different function.

This is the valid version:

    fun someFunction() {
        fun String.someOtherFunction() {

        }
        "".someOtherFunction()
    }

The same goes for classes too, this is also invalid

    class someClass() {
        fun String.someOtherFunction() {

        }
    }

    fun someStuff() {
        "".someOtherFunction()
    }

The scope is shared outwards when we set the context of those function calls, and this was clearly thought about.

*Infix calls*

And here is another feature, we have infix calls for functions with only one argument so..

    var builder = Builder(10)
    builder.invokeStuff({
      var result = 1 doStuff 50
      println(result)
    })

**My feedback**

There are so many things we can do with these functions and the way we do this seems to change with context (which is also something we control - haha, see what I did there?)

There is also power locked up in this state of affairs, I just wish it wasn't so confusing.

I don't see why there is a difference between function literals and declared functions (and if there isn't, I don't know why my examples work or why I'm so confused about it).

Functions are functions are functions, or at least they should be. I should be able to just do

    var doSomething = fun(x: Int) : Int {

    }

and

    map(doSomething)

Being able to pass in/set the context of calls is quite interesting for a specific use case (builders), and I kinda like it - although it feels a bit like something we could abuse horribly (and in fact, in the workshop we did - this is valid Kotlin code)

    json {
        "name" .. "Rob Ashton"
        "age" .. 5
        "address" .. {
            "number" .. 444
            "line one" .. "never you mind"
        }
        "family".array(
            {
                "name" .. "Gareth Ashton"
                "relation" .. "brother"
            },
            {
                "name" .. "Suzanne Ashton"
                "relation" .. "sister"
            })
    }


What I don't know is all the angles that are trying to be covered, so it's hard to tell why we've ended up in a situation where there is so much to learn about functions.

If you need a page on "[this ambiguiguity](http://confluence.jetbrains.com/display/Kotlin/This+expressions#Thisexpressions-Qualified)", then probably you've gone too far.

Like the other language features so far, it feels a bit like there were some cool features thought of, and then things got a little bit of hand as the edge cases were discovered.

- I like infix notation
- I like being able to pass functions around
- I like being able to make function literals
- I like extension methods
- I like the scoping of extension methods
- I like having type safety in the above
 
I'd just make an effort to prune it back and make things a bit more cohesive. I'd definitely take a look at context and try to avoid the need for pages explaining how to manage it. 
