I'm blogging about [SOLID](/entries/my-relationship-with-solid---starting-with-s.html) for some reason, and now we're onto the beast that set me off

**OCP**

Yikes

  <blockquote>
    They are “Open For Extension”. This means that the behavior of the module can be extended. That we can make the module behave in new and different ways as the requirements of the application change, or to meet the needs of new applications.
    They are “Closed for Modiﬁcation”. The source code of such a module is inviolate. No one is allowed to make source code changes to it.
  </blockquote>


Thanks [Uncle Bob](https://docs.google.com/file/d/0BwhCYaYDn8EgN2M5MTkwM2EtNWFkZC00ZTI3LWFjZTUtNTFhZGZiYmUzODc1/edit?hl=en), you're right, this *is* [over-stated](http://blog.8thlight.com/uncle-bob/2013/03/08/AnOpenAndClosedCase.html), and the cause of so many of the over-designed pieces of crap I've had to deal with so far in my career...

This is the conversation I imagine developers having with themselves when writing this done, I don't have to imagine too hard because I've been there too:

  <blockquote>
    What if somebody at some point wants to change this so they can have another behaviour for this value, I'd better use the strategy pattern here instead of this switch statement, but oh my word now I've done that what if somebody wants to use this code from some other system than this one, I'd better stick a command system in front of this and use double dispatch for handling them - but wait, what if other code needs to react from this and do something else, I'd better raise a pile of events, but what if those aren't enough I'd better make sure I split all this behaviours out into their own interfaces so they can be overridden and...
  </blockquote>

And on it goes until suddenly what was a couple of classes behind a controller or presenter blow up into a mess of a hundred classes that all do the square root of diddly squat but together do manage to cause a lot of headaches in anybody coming across the code so that's okay.

Now, I don't *care* what the original sentiment was behind Uncle Bob's writing, and I don't *care* what it is actually supposed to mean - what I do *care* about is the results of this and what I do *care* about is what I consider to be code open to extension but closed to modification.

  <blockquote>
    Good code is code that another developer can change the behaviour of easily and easily see the consequences of that change.
  </blockquote>

At this point in the road, we can go in one of two directions - that is to say, agree with [@jonskeet](http://twitter.com/jonskeet)'s well publicised opinion that all code should be immutable by default (in that classes should all be sealed by default, methods should all be sealed by default etc), or go in the other direction and say that the monkey patching ability in Ruby and JS help us do this just fine.

I hate to say this, but I lean towards [@jonskeet](http://twitter.com)'s opinion at this point, which is to say that any extension points built in to your code should be an explicit decision that is obvious to the consumer, and protects them against the dangers of screwing with state they don't understand.

**Let's make call one**

Let's say we do make that call (because Jon is where all of this started, so taking his perspective will help see where he is coming from), then surely if we're going to follow OCP then we have to from the very beginning bake in these extension points on a *just in case* basis. 

Woah no, no no. Let's say we take this stance, then you know what? Code that has easily understandable behaviour will follow the single responsibility principle sensibly so we can see exactly what an object does and how changing it might impact its internal behaviour.  (Of course, we'll have tests around the usage of this so we'll have tests around the external behaviour too)

Creating spaghetti code is contrary to this goal and should be avoided. Create code that is easy to understand and sure - hedge your bets so if something comes up the future, the developer having to make the changes doesn't have to fight the layers you've put in his way.

**Let's make call two**

We're in JS land now, and we can just screw over any object by fiddling with its prototype, the rules have gone out of the window. This is the *land of possibility* people, and we have the power to change things.

Does this mean we haven't got to worry about OCP? Nay, although granted you'll see in the code being delivered in this space a complete disregard for it up until the point a feature is requested at which point they'll might be granted access but in the meantime, *workaround* it and make sure you test to make sure you haven't screwed things up.


**Option three**

Wait, I didn't mention an option three - but you see, there is one. In the NodeJS world, we have tiny modules doing tiny things, I'm currently writing a game engine which cumulately is about ~8000 LOC
