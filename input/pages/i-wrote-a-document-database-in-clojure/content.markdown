A couple of years ago I met [@bodil](http://twitter.com/bodil) at a conference and asked her to teach me the functional programming so I could function better with my programming. She told me all about [Clojure](http://clojure.org/) which was on the JVM and totally hip.

I then tried writing Space Invaders a bunch of times in a variety of different ways to varying degrees of success but in the end I decided it was a bit of a reach for a first project and that writing something in a domain that was more familiar to me would be a good idea.

[So I wrote a document database in Clojure](http://robashton.github.io/cravendb/).

The goal was *not* to write something that people would use, which is good because I stopped working on it months ago and nobody is using it.

- The goal was to pretty much do what [RavenDB](http://ravendb.net/) does by using some sort of trusted persistence, and indexing into Lucene for query purposes using map/reduce/etc.
- The goal was to make a tidy client API in Clojure that would be really easy to understand and use
- The goal was to learn how to write Clojure by writing a lot of Clojure

### What I ended up with

I ended up with a few thousand lines of Clojure across a total of 34 files, half of those lines were just code testing the other code, and the other half is written in a variety of styles as I wrote more Clojure and realised how everything I'd done in the past few weeks was wrong over and over again. During the development of this database I also ended up in the top 100 OSS committers across Github  - presumably becuase I spent so long committing fixes to those mistakes.

It took me on a journey through interop with legacy Java, interop with native code, adventures in core.async, building a RESTful API in Clojure, building a HTTP client in Clojure and a ton more stuff that is probably good to know if you want to be a Clojure developer.

I am not a Clojure developer, although I have done some work in Clojure. I consider this work to have been a key moment in turning me at least into a borderline reasonable functional programmer and netted me my current job of writing code in Erlang from coffee shops over Europe. Yay indeed.

### I am going to go through this stuff

I'm going to do a few talks this year on my experiences with Clojure and on learning via writing this database in particular. For those lucky enough to avoid these events I am going to write about some of the areas of this codebase, some of the decisions I made when writing them and some of the obvious mistakes that perhaps I shouldn't have made.

This will be a very practical and possibly/probably even wrong information sharing exercise but hopefully it means the hours I put into building that database can be used to help others too.

Ready? Well you'll have to wait, the posts are queud up :-)





