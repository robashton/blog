I saw a tweet by [@jonskeet](http://twitter.com/jonskeet) the other day which caught my eye:


  <blockquote class="twitter-tweet"><p>(I know that doubting things like OCP is pretty close to heresy, but it's just *never* made sense to me.)</p>&mdash; Jon Skeet (@jonskeet) <a href="https://twitter.com/jonskeet/status/309911260701552640">March 8, 2013</a></blockquote>
  <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>


*Well okay then*

Well, obviously I kinda agree if you go and hit up the ["Immature" 43 year old Uncle Bob's](http://blog.8thlight.com/uncle-bob/2013/03/08/AnOpenAndClosedCase.html) statement which is as written:

  <blockquote>
    They are “Closed for Modiﬁcation”. The source code of such a module is inviolate. No one is allowed to make source code changes to it.
  </blockquote>

But this got me thinking more widely on my relationship with SOLID as a whole and how that has changed over the years, and how many times (like the GoF patterns) I've seen an over-zealous and misunderstanding of these concepts wreak havoc in codebases.

I've been able to quote these word for word for the past half decade quite easily, and my relationship and understanding of how these seemingly innocuous statements impact my code has changed over time, much like [my relationship and understanding of TDD](/entries/uncle-bobs-viewpoint-considered-harmful.html)

So, for the next 5 days, I jot down my current relationship with SOLID without too much thought or proof-reading...

**[The single responsibility principle](http://en.wikipedia.org/wiki/Single_responsibility_principle)**

  <blockquote>
    In object-oriented programming, the single responsibility principle states that every class should have a single responsibility, and that responsibility should be entirely encapsulated by the class. 
  </blockquote>

This is the kind of statement that leads to [this sort of mess](http://ayende.com/blog/154177/limit-your-abstractions-so-what-is-the-whole-big-deal-about), 

  <blockquote>
    "but omg my controller should only be routing and my service should only be servicing and my data layer should only be data-ing."
  </blockquote>

And this is unfair, and yet the confusion continues because people see several parts to the whole idea that

  <blockquote>
    a class should only have one reason to change <br/>
    a  class should only have one responsibility
  </blockquote>

The problem is, that most of the time, the abstractions people come up with to limit a classes responsibility are *horizontal* separations, when the true power of single responsibility lies in the vertical.

Perhaps this is because they're easier to conceptualise, and easy to write frameworks and patterns for so we can feel productive in our day job - but this is really not as it should be.

Frameworks like ASP.NET MVC don't help us with this and their by-default grouping of horizontal concerns across a project, it makes it too easy to muddle the verticals and separate the horizontals.

*Your relationship with state*

A lot of the time it boils down to state, OO and state have a bit of a confusing relationship, and most of our efforts should be devoted to minimising the amount of mutable state that is exposed to concerns that aren't directly related to each other.

Funnily enough, despite the confusion this is actually pretty easy to conceptualise via tooling and metrics, if your classes are cohesive, most of the methods on that class will touch most of the state in that object. 

- If half of the methods touch half of the state, and the other half of the methods touch the other half of the state then there are clearly two classes.
- If you're constantly having to pass visitors around the place to get at private state, or expose state through getters, then you should probably be looking at merging code because you've spread responsibility too thin
- If you're constantly passing state around in bags to be mutated by several other things, then you likely have the responsibilties spread out over several layers and you should likely be deleting those layers and putting the code in an object that looks to protect access to that state.
- If you haven't got a lot of directly mutable state, then something somewhere probably is being mutated (such as in the database) and following that chain to the source will yield in answers similar to the above.
- If you have to jump through more than a couple of classes to find the state that your behaviour is trying to modify, you've gone too far - keeping your mutable state close to the actual behaviour being enacted on it is the road to success

Having a horizontal layer whose responsibility is "modifying state in the database" is nonsensical and vague.

Having several objects whose responsibility is looking after the state for a particular feature and then persisting it (perhaps via another facillitiating object) has a lot more sense in terms of understandability and traceability.

*A note note on orthoganal concerns*

State based data persistence is not (usually) an orthogonal concern, neither is the workflow/routing sat in front of your MVC application - logging on the other hand can be, and authentication/authorisation can be too. 

Clearly, you shouldn't be constantly modifying these vertical slices because of a change to your authentication scheme or logging system. Trying to classify too many things as being orthogonal to your core functionality however is what leads to layer soup, and care should always be taken not to do this.

You can discover these as you go, there is nothing wrong with littering your code with these concerns to begin with, and then as things get repetitive, pulling them out to make life easier. Premature attempts at trying to isolate these concerns is often the path to layer soup.
 

*Upwards from the class level*

Trying to make classes whose concerns are limited, whose reason to change are limited is all very well and good, but it's no good looking at things under a microscope and saying *Hey, things look okay*, when you sit back and have to go *what is this crap?* 

Let me invoke the [NodeConf drinking game](http://codeofrob.com/entries/lots-of-small-things.html), a lot of the time it is much more valuable to think of your codebase as a collection of modules which are independently versioned and have clear boundaries set up between them.

Any of these small modules can start off by being a complete and utter mess, and if further work is required in that area you can either re-write the module in a few hours, or refactor it as you go (did you say *spike and stabilise* again Rob? I think I did)

*That* is where single responsibility really means something, and you can save a lot of time in development by building these disposable building blocks when you're rapidly iterating on a product.


*Summary*

I seem to have started with the least controversial and one of the most harmful of rules, oh well...

Thus ends my brain dump on responsibility and the many routes to layer soup. Tomorrow I'll go the heart of the matter on OCP, and then wind down the rest of the week through the rest of the set.

