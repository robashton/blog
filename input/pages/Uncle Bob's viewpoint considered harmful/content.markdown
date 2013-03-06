**oof**

I just felt a disturbance in the force, it's as if a thousand angry geeks just vented their nerdrage on Twitter because of a [blog post by Uncle Bob](http://blog.8thlight.com/uncle-bob/2013/03/05/TheStartUpTrap.html) and a response by [@Nate Kohari](http://news.ycombinator.com/item?id=5328721).

<blockquote class="twitter-tweet"><p>It looks like I've pissed off everyone at 8th Light. Fortunately I can't hear the whining over the sound of me shipping code.</p>&mdash; Nate Kohari (@nkohari) <a href="https://twitter.com/nkohari/status/309028034755825665">March 5, 2013</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Ouch.


**So what kind of tests do you write Rob?**

Meh

**When do you write tests Rob?**

Meh

**Do you write tests Rob?**

Meh

**What**

Ok, so I write tests - I usually can't be bothered having this conversation though, because years of having "TDD" forced down everybody's throats has left a bitter divide between developers who crave tests with every ounce of their being, and developers who really don't like tests and wading between them is opening myself up for another few hours of argument when I'd prefer to just be building stuff.

*I'm currently wandering around though, and have the time for this sort of thing.*

Classic "*make no assumptions*" TDD is dead, that ship has sailed - it is over.  The stance it put forth however had the desired effect; which was to get everybody thinking about the code they wrote before they wrote it and write some tests around that code at some point in that code's lifetime so that iterating on that code in the future wouldn't be painful.

*painful*

That's the word. **pain**, let's say it again - **pain**, if it hurts - do something about it. 

- Do you have too many tests that you are regularly throwing away with the failed feature? Stop writing so many tests around your product - you're likely experimenting and you're best off adopting a "spike and stabilise" approach to development.

- Do you find it hard to make changes because lots of things break due to harmful coupling? Write some more tests around that part of the system, practise a test-first approach on new code and feel your way around those coupling issues.

- Are you performing a song and dance around a feature because you want to do fine-grained unit testing and it means putting reams of code where your direct controller CRUD would suffice? Stop, stick with the end-to-end tests and perhaps use an in-memory repo to keep them fast enough.

Listening to the pain is the only sane way to do software development, blindly listening to far-right viewpoints like Uncle Bobs will get you so far, and listening to far-left viewpoints which advocate throwing caution to the wind will probably lead you to failure. The idea that tests are a waste of time is a dangerous one if given time to grow in an echo chamber, but the idea that TDD is the absolute is just as dangerous.

Most of the time going with *make no assumptions* TDD is a waste of your client's money, and most of the time writing no tests at all will come back to bite you in the future - *got it*.

**So what kind of tests *do* you write Rob?**

- I usually spike and stabilise on new features which are likely to be canned, this means not making undue effort early on on code likely to be thrown away but allows the feature to get out as quickly as possible for feedback
- I usually start off with end-to-end tests with an in-memory representation of anything that is slow (the database is an implementation detail etc)
- I drop down to unit tests if I have complicated logic (surprisingly, most systems just have crud, so this is not necessary)
- Workflows are usually tested from the UI as part of the end-to-end suite as it's what the user sees
- Any combination of the above

**Summary**

There are two types of pragmatist in this world when it comes to testing, those that use pragmatism as an excuse not to do the right thing, and me.
