I'm nearly done with my brief foray into Kotlin, but I've a few more things to cover

- [Looking at Kotlin - getting started](/entries/looking-at-kotlin---a-few-notes-made.html)
- [Looking at Kotkin - Classes and Functions](/entries/looking-at-kotlin---classes-and-functions.html)
- [Looking at Kotlin - Inheritance](/entries/looking-at-kotlin---kicking-the-java-inheritance-addiction.html)
- [Looking at Kotlin - Fun with functions](/entries/looking-at-kotlin---fun-with-functions.html)
- Looking at Kotlin - Wrapping it up

**Inner classes**

So, inner classes, a feature that both Java and C# have and each with their own set of issues.

    class Outer() { 
      private val bar : Int = 1 
        class Nested() { 
          fun foo() = 2 
        } 
      } 
           
   val demo = Outer.Inner().foo() // == 2

No biggy here, except in Java there is a problem with nested class as explained in [this blog entry](http://blog.jetbrains.com/kotlin/2013/02/kotlin-m5-is-out/) 

  <blockquote>
    An inner class is a non-static nested class, i.e. it holds a reference to an instance of its outer. In Java nested classes are inner by default, and if you don’t want a reference to the outer, you make your class static. Sometimes it leads to memory leaks, when someone is holding a reference to an instance of an inner class without knowing that it also holds an outer instance.
  </blockquote>

Kotlin's solution is to say that you can't do this

    class Outer() { 
      private val bar : Int = 1 
        class Nested() { 
          fun doSomething() {
            println(bar)
          }
        } 
      } 
           
Instead you have to mark the inner class with 'inner' to make this an explicit decision

    class Outer() { 
      private val bar : Int = 1 
        inner class Nested() { 
          fun doSomething() {
            println(bar)
          }
        } 
      } 

I have a better solution, *get rid of inner classes*, they're crap anyway and don't just cause problems in software, but apparently cause problems with language design too. Problem solved.

**null and elvis**

Kotlin operates on the basis of things not being nullable by default, which is something I wholeheartedly support.

    var derpy : Pony = null

This isn't allowed.

We're allowed to say we *want* a nullable object, by adding a question mark to things.

    var derpy : Pony? = null

This is sensible, and can't be faulted.

Most legacy java APIs will return things that *can* be nullable, and in fact a lot of Java APIs do this in places where they really shouldn't. The compiler will complain about this though, so where I do


*Compiler error*

     var fileList = files.listFiles()
     var size = fileList.size
     println(size)

*Compiler success*

     var fileList = files.listFiles()
     var size = fileList?.size
     println(size)

The elvis operator gets compiled down into

     var fileList = files.listFiles()
     if(fileList != null) {
       var size = fileList.size
       println(size)
     }

This is a case where Kotlin "fixes" Java well, so I'm not complaining.


**Ranges**

Syntastical sugar, but always a fan of this sort of thing

    for (i in (1..4).reversed() step 2) print(i) // prints "42" 

I like this sort of thing, although it gets out of hand in languages like Ruby so I'd watch this stuff cautiously.

**Pattern matching**

    when (x) { 
      1 -> print("x == 1") 
      2 -> print("x == 2") 
      else -> { // Note the block 
        print("x is neither 1 nor 2") 
      } 
    }

or

    when (tuple) { 
      is #(1, 2) -> ... 
      is #(val a, 3) -> print(a) // binding a to the first element of the tuple 
      !is #(*, 1100) -> ... 
      else -> ... 
    }

I'm a sucker for pattern matching constructs and I like this implementation because of how easy it is to declare those function literals.

Also, it does intelligent casting for you as well, cutting own on some of the cruft you'd usually get with this sort of thing

    function strangeFunction(a : Any) : Any {
      when(a) {
        is Int ->
          return a + 5
        is String ->
          return a + "5"
      }
    }

Nice

**Round-up**

The stated goals of Kotlin of "fixing Java" don't appear to have been met, there are holdovers from the language such as 

- The same old Java build cruft
- Over-complicated inheritance system
- Inner class madness

And then some new ones

- confusing function constructs
- keyword heavy solutions

It does 'fix'

- Null

And it does compile to JavaScript which means some interesting library targeting opportunities.

If I were "fixing a problem like Java", it would be by removing features that people shot themselves in the foot with often and by adding a few pieces of sugar to make things more readable in places.

It wouldn't be by adding more key-words and more options for shooting yourself in the foot with, and it wouldn't be by defaulting to mutable and public :)

That's pretty much all I have to say from my day of Kotlin, there are probably more things in there but I've got other stuff to do and not on the JVM!





