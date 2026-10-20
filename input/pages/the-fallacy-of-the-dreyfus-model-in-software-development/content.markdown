Not [another](/entries/uncle-bobs-viewpoint-considered-harmful.html) [post](http://blog.markrendle.net/2013/03/09/dont-unit-test-trivial-code/) [about](http://blog.ploeh.dk/2013/03/08/test-trivial-code/) [TDD](http://blog.8thlight.com/uncle-bob/2013/03/05/TheStartUpTrap.html) Rob, please no!

Okay, so I'll reply on a tangent because I've had enough of talking about TDD and want to talk about software development as a learned "craft" and why pimping out magic software development strategies to "newbies" is harmful to the overall quality of software development.

First off, I want to make an opening statement so you know where I'm coming from. This is based on various jobs and contracts I have been a part of and various code bases I have been asked to look at over the years.

  <blockquote>
    Badly applied software patterns and methodologies often cause bigger problems than not applying them at all
  </blockquote>

This is obvious to most people (I hope), we *know* this - so why am I saying it again?

I read [this response](http://programmers.stackexchange.com/questions/185719/how-should-you-tdd-a-yahtzee-game/188188#188188) to a question on TDD on StackExchange and the sentiment being expressed is:

  <blockquote>
    Flexibility isn't for novices
  </blockquote>

This hearkens back to the [Dreyfus model of skill acquisition](http://en.wikipedia.org/wiki/Dreyfus_model_of_skill_acquisition) and the idea that beginners and novices shouldn't be left alone to fend for themselves; that they should be strictly adhering to the recognised rules during this development phase.
 
Let me start off by saying that this premise is something that I agree with across any industry where experience is more important than qualifications or badges next to your name. 

**However...**

Software development is definitely one of these industries, so what's my beef/horse with applying this model in reality?

  <blockquote>
    The statement that beginners should be strictly following the rules makes the assumption that they have access to somebody who isn't a beginner and can tell them that they're doing it wrong.
  </blockquote>

The problem is that software development is *still* an immature and confusing trade. Access to these experts is limited to 1-2 week consultancy stints where they come in, spout a pile of "truths" and then leave the team to fend for themselves (if this even happens at all).

What we actually have out here in the field is thousands of software teams with no experience of these things. They also have no real access to anybody with these skills.

When they hear advice from the so-called Master Craftsmen in the field that:

  <blockquote>
    You should be testing all trivial code
  </blockquote>

They take that as a gospel and get to work creating literally thousands of brittle unit tests (it's likely they're not even doing TDD "properly"). The end result is that their code is worse off then had they just felt their way through the problem with direct solutions and applied evidence-based solutions to the pain points.

The same problem happened 10 years ago with all that rubbish N-Tier advice that the Enterprise Consultants were doling out.

  <blockquote>
    The problem is that we're encouraging thousands of new developers to follow the rules and not think for themselves.
  </blockquote>

*All of this has happened before and will happen again.*

**The ideal world**

In the ideal world where we all have access to mentors with vast experience in this "more professional world" that our master craftsmen [aspire to create](http://blog.8thlight.com/uncle-bob/2013/03/05/TheStartUpTrap.html) then we can afford to spend time bringing up our "novices" with:

- rigid adherence to taught rules or plans
- no exercise of discretionary judgement

This sounds absolutely wonderful and when we have a few hundred thousand people with 20 years of experience trying TDD and failing at it. When those few hundred thousand people have found the balance and come to their form of pragmatism these people will make *wonderful* mentors.

**The reality**

In reality, the mentors that our novices have access to are the ones who still think that SQL Server is the only safe place to put data. You know - the ones who still think that every app needs a [BLL, DAL, BOLL,ETC](/entries/cqrs-is-too-complicated.html), and that if you write your code properly the first time you shouldn't need tests.

In reality, a lot of these developers have decided that TDD is the greatest thing without really trying it out, or that every piece of code should have a Single Responsibility, and Never Be Touched Once Written (it's an interpretation I've seen). This is  because that is *how it's written on the internet* and because of this they're "engineering" *monsters* which while financially great for consultants like me are costing the companies that own those monsters dear.

The reality is that new software developers aren't going to spend their first two years being spoon-fed "the correct way of doing things" (whatever that is, because we're still not sure). 

The reality is that they might get a couple of months of "help" when starting software development at their first company (where the help is "this is how you use Enterprise Framework X").

The "people on the internet" spouting TDD and associated methods like sermons do not count as mentors to these people. You cannot apply the Dreyfus model to this sort of learning relationship. These people are not there for these novices and trying to reason about software craftmanship as if they are is what is causing the *new* problems.

The reality is that to every question the answer is *it depends* and we should be arming our "novices" with the tools to work out what the real answer is instead of handing them a single hammer with a bag of nails. (That's a metaphor for "writing blog entries which state that *you're doing it wrong unless...*")

**Getting a grip**

Spouting dogma at people on the internet and at conferences is self-defeating, (Unless you make your living off consultancy in these matters) it either puts people who know better into defensive mode because they recognise the dogma and rightfully fear it, or encourages people to follow your rules without looking at the wider context which causes substantial problems in software products.

Quit it.
