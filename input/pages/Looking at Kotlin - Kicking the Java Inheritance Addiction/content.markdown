I'm still looking at Kotlin and this is where I'm at so far..

- [Looking at Kotlin - getting started](/entries/looking-at-kotlin---a-few-notes-made.html)
- [Looking at Kotkin - Classes and Functions](/entries/looking-at-kotlin---classes-and-functions.html)
- Looking at Kotlin - Inheritance

We've established (at least in my brief foray), that Kotlin doesn't really fix one of the biggest problems in Java (and C#), which is the misuse of classes as mutable data bags being passed around by scripts.

One of the other big problems in Java is that a common way to fix problems with code you don't own is simply "inherit from all the things and override all the things".

This is their version of monkey patching, but in practise ends up being even worse because there is a tendency to use this power for the re-use of re-usable things which need re-using because there are a few lines of code we could re-use if we squinted hard enough at it.

I'm not really a big fan of inheritance systems in any language; I *am* all for the notion of bringing in behaviours from other systems though. I prefer to do it via composition or in JavaScript just copying the methods over with the constructor function state.

**What does inheritance look like in Kotlin?**

Well, to begin with it looks just like it does anywhere else, although it allows for multiple inheritance too (which is an inevitable quagmire of complexity, let's see...)

    open class Pony(val name: String) {
    
    }

    class Unicorn(name: String) : Pony(name) {
        
    }

One of the good decisions made here is that at least classes are closed by default, which I know annoys a lot of people but I think if you're making APIs or framework code in this sort of language a closed-by-default approach makes sense.

I'm going to ignore this for now because it's the same old crap you'd expect from any inheritance system.

One of the reasons we sometimes wants to "inherit", isn't necessarily because we want state *and*behaviour, and in fact the multiple inheritance in Kotlin is only allowed in cases where we only want behaviour.

For this, we have the "trait" keyword, which defines an class which only has behaviour, and in this inheritance system you can inherit from one class and as many traits as you like.

    trait Speaker {
      fun SayHi() {
        println("This is the best day ever")
      }
    }

    class Unicorn(name: String) : Pony(name), Speaker {
        
    }


I can't actually think of many cases where this would be something I'd want to do, you can't give anything to that trait so it's basically a static utility bag at this point.

It gets interesting when we have the ability to create a composition system out of these traits

**Composable traits**

How about we a trait called Speaker like the above, and ponies and unicorns aren't derived from each other, let's make it look like this for now:


    trait Speaker {
      fun SayHi() {
        println("This is the best day ever")
      }
    }

    class Pony(val name: String) : Speaker {

    }

    class Unicorn(val name: String) : Speaker {

    }

My ponies and unicorns now have a method called "SayHi" and I can invoke these like so

    var pony = Pony("Pinkie Pie")
    pony.SayHi()


This is great, except actually ponies and unicorns don't say things in quite the same way, and actually they all have different sayings.

Well how about creating a speaker which is pretty much an interface, and an implementation which says stuff based on some state:

    trait Speaker {
        open fun SayHi() {
            println("herp derp")
        }
    }

    class ConstSpeaker(val saying: String) : Speaker {
        override fun SayHi() {
            println(saying)
        }
    }


We can actually pass these in in the constructor of our ponies and unicorns like so

    class Pony(val name: String, voice: Speaker) : Speaker  by voice {

    }

    class Unicorn(val name: String, voice : Speaker) : Speaker by voice {

    }

Which means we can do

    var dash = Pony("Rainbow dash", ConstSpeaker("I run faster than you"))
    var celestia = Unicorn("Princess Celestia", ConstSpeaker("I'm the boss of all the ponies"))


**Good news everyone**

  <img src="/img/farnsworth.jpg" style="float: left;">

Well, we've found something that in principle I really like about Kotlin.

This is one of the pleasant surprises I had when exploring Kotlin at the workshop, although it caused all of the people in my immediate vicinity (including myself) undergo confusion the first time we came to try and use it.

If we got rid of the crappy legacy inheritance system and forced this composition based approach my *word*, we'd solve problems, we'd solve a lot of problems.

  <div style="float: none; clear: both;">

Make the above a little less verbose and we could be onto a winner.

It feels as if the language designers haven't really designed at this point, they've just thrown a load of stuff at the wall because inheritance is something that all Java developers want and given us a few bad ways of doing something as well as something that has the potential to make us write better Java.

**My feedback**

If I was taking tackling this whole "improve Java" problem, I'd be looking at some of the awful Java being written out there and saying "Hey, inheritance based approaches usually suck, can we make them better?", and I'd still not have come up with that traits system.

The traits system is great, run with it - get rid of the others and you remove a crap load of complexity and show an opinionated way of building composable software.

No doubt there are some problems not immediately obvious with it, but I'm not being paid to be a language designer, the JB guys are so... what are you waiting for? :)






 
    



