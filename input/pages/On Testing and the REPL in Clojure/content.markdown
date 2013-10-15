I'm currently at [Euroclojure](http://euroclojure.com/2013/) where I have the pleasure to have been meeting and chatting to people about Clojure and related ponies for the past day or so - and I've learned a few things and got a much better feel for what the Clojure community is about.

One theme that keeps coming up is the ugly one of testing, and its association with the obsession over tests in other communities, whether it be TDD or some other variant of making yourself feel good through automatic code exercise.

Another theme that keeps coming up is that the JVM (particularly under Clojure) has a heavy start-up time of quite a few seconds, so that practising a strict TDD feedback loop as you change your code is impractical without installing hacky workarounds like drip or associated (pre-warming the JVM for example). This is reminiscent of what the Rails folk do with Spork/related and comes with a whole suite of interesting problems.

Another more concerning theme is the derision I've been feeling from a lot of corners as a result of the first theme on tests themselves - some of it is joking around, and some of it is almost describing tests as a set of crutches to get around the fact that some code is hard to manipulate in the REPL for example.

As I've been writing [CravenDB](https://github.com/robashton/cravendb), and during my time at MastodonC I have been adjusting my patterns and attitudes around this subject and I'd like to describe where I'm currently at with testing, tdd, the repl and Clojure.

### The REPL *IS* TDD

The REPL has taken over as my primary way of exploring a new problem or a new feature, and this seems to be due to the following reasons:

- I quite like manipulating a bit of code once I already have state in memory and repeating a small bit of code over and over until the algorithm is just right
- Running tiny tests around that sort of thing would be time consuming, because JVM/Clojure start-up time
- Most code written in Clojure seems to be very honest in that it's just about transforming state to get some sort of result, and this is usually as a result of composing little functions in a chain (often through experimentation) to get the result we ewant.

If I was in another platform where the REPL isn't quite as good (so anything else really), I think I'd still be writing lots of small tests to build up my understanding around a problem. If I was in another platform where the process of finding a result wasn't so... functional, I'd probably have a lot more code and I'd have to hide it behind classes which require setup/teardown and tests are a good way of repeating that process in a consistent way.

The separation of state from behaviour in a functional language like Clojure combined with the dynamic nature of Clojure makes the practice of repeatedly editing little bits of that behaviour then sending state through that behaviour a stonkingly easy thing to do, so it's of course a happy path.

Once I've done this, I clear the REPL and my job is done because...

### I would only throw those tests away anyway

If I'm doing TDD (which I rarely do because most enterprise code is so stupidly simple), I build up quite a suite of tests around  tiny pieces of understanding and can end up with a very feel-good number of tests that do very little but get in my way once written.

What generally happens is that I'll end up with very much an outside-in set of tests around the actual features/functionality and I'll delete all the tiny tests because their only purpose was to help build my understanding and my more functional tests are more about testing the final result.

I've found that while writing my [database in Clojure](https://github.com/robashton/cravendb), that once I've finished building up a set of functions around some state to get ma new version of that state, that writing a couple of tests around that whole set of functions that test maybe a positive and a negative case is a reasonable thing to do because I end up leaning on that test in the future when adding new features to prevent regressions against my overall functionality.

What also seems to be the case, is I can often just transplant the code I've built up in the REPL into 

- The code base
- The tests

And get that regression suite almost for free.

- I don't spend a lot of time fixing the regression suite because of internal structure changes as they're more dependent on the overall results than internal set-up.
- They do save me a lot of time in regressions because I'm discovering better ways to write my internals all the time and I'm not always capable of keeping the whole system in my head when practising those better ways.
- They *do* seem quite high in volume, but I've not found that the line count in my tests is quite as indicative of bad code as it would be in the main code base (most of it is just descriptions of what the tests do rather than big chunks of code that I'll have to maintain.
- Sometimes fixing an issue is just a matter of turning debug logging on and running a test, I like that as a result of *log all the things*


### I do have to be careful though

It is difficult sometimes when I break functionality to not start falling into the trap of repeatedly running the tests and "fixing" things until the tests pass. I've set a hard rule for myself for the moment that if I can't get the tests to pass with-in a couple of iterations that I have to drop back to the REPL and start exploring the problem from the inside-out.

This process helps ensure:

- The internals are still easy enough to use within a REPL
- I still understand the internals (as a developer new to the project, this would be how they learned I guess)
- I don't spend time shotgun debugging issues

### Fin

That's my process anyway, I've enjoyed telling people I have 1500 lines of code in my tests and hearing that sharp in-take of breath at this conference, but of this project I have to say they're the bit I'm least ashamed of. Some of my loops on the other hand....
