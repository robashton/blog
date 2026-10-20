**Open closed is dead, long live Open closed**

I'm blogging about [SOLID](/entries/my-relationship-with-solid---starting-with-s.html) for some reason, and now we're onto the beast that set me off:

**OCP**

Yikes

  <blockquote>
    They are “Open For Extension”. This means that the behavior of the module can be extended. That we can make the module behave in new and different ways as the requirements of the application change, or to meet the needs of new applications.
  </blockquote>

 and

  <blockquote>
    They are “Closed for Modiﬁcation”. The source code of such a module is inviolate. No one is allowed to make source code changes to it.
  </blockquote>


Thanks [Uncle Bob](https://docs.google.com/file/d/0BwhCYaYDn8EgN2M5MTkwM2EtNWFkZC00ZTI3LWFjZTUtNTFhZGZiYmUzODc1/edit?hl=en), you're right, this *is* [over-stated](http://blog.8thlight.com/uncle-bob/2013/03/08/AnOpenAndClosedCase.html), and because it's so over stated, I believe it to be the cause of so many of the over-designed pieces of crap I've had to deal with in my career :-)

This is the conversation I imagine developers having with themselves when writing this stuff, I don't have to imagine too hard because I've been there too:

  <blockquote>
    What if somebody at some point wants to change this so they can have another behaviour for this value, I'd better use the strategy pattern here instead of this switch statement, but oh my word now I've done that what if somebody wants to use this code from some other system than this one, I'd better stick a command system in front of this and use double dispatch for handling them - but wait, what if other code needs to react from this and do something else, I'd better raise a pile of events, but what if those aren't enough I'd better make sure I split all this behaviours out into their own interfaces so they can be overridden and...
  </blockquote>

And on it goes until suddenly what was a couple of classes behind a controller or presenter blow up into a mess of a hundred classes that all do the square root of diddly squat, but together manage to cause a lot of headaches for anybody coming across the code in the future.

Now, I'm sure this wasn't the intent behind these statements, and it sure isn't now - but you know what?

- I don't really care what the original sentiment was behind Uncle Bob's statement.
- I don't really care what it is actually supposed to mean. 
- I *do* care that code that I come into contact with doesn't get in my way when I want to add a feature

Here is my current thinking on the Big O. Let's make it stand for "Open", and remove the CP.

  <blockquote>
    Good code is code that another developer can change the behaviour of easily and clearly see the consequences of that change.
  </blockquote>

The decisions made when designing a language can have implications on how we satiate this need.

We can look to [@jonskeet](http://twitter.com/jonskeet)'s perfect language where "all classes are sealed by default, all methods are sealed by default, all extensibility points are explicitly defined", or we can look at any of the no-holds barred dynamic languages that let us get away with pretty much anything.

**Let's take option one**

Let's say we *do* make that call (because Jon is where all of this started, so taking his perspective will help see where he is coming from), then surely if we're going to follow OCP then we have to from the very beginning bake in these extension points on a *just in case* basis. 

Woah! No!!! Stop right there. This is how we end up with the kind of code where we use the strategy pattern everywhere and have a million and one interfaces to describe the act of doing absolutely nothing of consequence at all.

*The best code is code that can be changed easily*, code that is easy to read, code that keeps state close to the behaviour, code that that doesn't attempt magic tricks, code that anybody can read - this is the code meets this standard. 

**Let's take option two**

Now we're in the magical happy land of dynamic languages, and we can just screw over any object by fiddling with its prototype, the rules have gone out of the window. This is the *land of possibility* people, and we have the *power to change things*.

Does this mean we haven't got to worry about OCP? Nay - this is not so. Having the ability to change anything is *fantastic*, people *don't* know everything when putting together a module, and having everything open by default means that while you wait for the project you're using to have an appropriate extension point you can hack around it and get on building your product.

However, explicitly defined extension points have clearly defined behaviours and are predictable - so are clearly desireable.

**Wait a second**

Getting off the subject of how languages can affect our decisions, we can look at how our programming culture can have an impact on it. 

I find it intensely irritating that the languages that lean towards the "closed by default" design also seem to live in the environment where the code itself is also "closed by default", which means that either the framework authors have to build in extension points for everything imaginable or the users of that framework code have to suffer for it.

I find it intensely amusing that the languages that lean towards the "open by default" design also live in the environment where the code itself is open by default (this is the age of Github), which means that the people with the problem can come in, make the desired change and move on with their projects with barely any thought to it at all.

And this is where I go back on what I said in all the statements above, *this* is where OCP is now, times have changed since the original sentiment was uttered, we have a lot more open source now and the ideal is:

- Building small disposable modules that live in package systems and on Github/etc
- Primarily building products out of these open source building blocks
- These building blocks are either replacable in a few hours work, or easily extended via a pull request
- These building blocks can be forked for your project and changes merged from upstream with little effort

This changes the very face of things, this changes how we build software - and it means that a strict adherence to OCP becomes largely a thing restricted to stodgy enterprise environments that are slow moving, uncompetitive and slow to get things done (and they're welcome to it)

The true future of OCP is in building these open source little packages that are easily changed or forked, and in that we can find an elegance and simplicity that I find quite pleasing.

If we're forking a module and sticking a new version on it, we're saying that it is no longer the same as the old module, it is new code and the old code still exists too.

**Where OCP makes sense**

- An example of where this sort of thinking has relevance, is in technical patterns like event sourcing, where once an event becomes committed, it becomes preferable to create new versions of that event rather than modify the original (because events can't be changed, they already happened - etc)

- An example of where this sort of thinking has relevance, is where you have code that starts being modified in the same way repeatedly (such as adding further conditions to an if statement), and this modification has cost and side-effects. Refactoring towards simplicity usually has the effect of removing this sort of repetitive action and indeed that code will become closed to modification over time as it matures.

- Repetition actually has the key, if you're constantly touching  the same piece of code again and again over time, then that piece of code is probably, as [@gregyoung](http://twitter.com/gregyoung) puts it, a "bug hive". If you have this sort of pattern in your source control then that code is likely brittle because it hasn't been refactored over time towards making it easier to extend without breaking stuff.

- Coupled with a practical application of the single responsibility principle, we can say that if we're changing a piece of code too much either our requirements are just changing a lot (at which point you're not changing code you're throwing old code away and replacing it), or our code is trying to do too much.

**Summary**

So, in the age of tiny disposable modules that do just one thing, OCP is dead (*wink*) - who'd have thunk it. */dramatic oversimplification*
