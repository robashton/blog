Do you like *anything* Rob? You said you were going to be nice and all you've done is complain about [.NET](/entries/the-ashton-disinterest-curve---c.html) and [JS](/entries/the-ashton-disinterest-curve---javascript-and-node.html) so far. Point taken - let's talk about Clojure.

An ode to the lisp
===

*(parens (love (i)))*. I love s-expressions; code written in Clojure tends to be beautifully expressive thanks to the terseness and minimal syntax provided the humble s-expr. When you go on to add easy composition and a rich library of "All the basic things you might want to do to a list or a key-value structure" the magic starts to happen. The focus on data-oriented code that gave birth to that heavily re-usable core library and an emphasis on referential transparency means that things generally do just what you expect them to and you can usually just focus on the functionality that led you to open up an editor in the first place.

Speaking of editors, once you have your editor of choice set up with REPL integration, Paredit (if you want to edit expressions and not lines) and you've downloaded the internet with Maven, the low syntastical burden of the language makes it ideal for hacking around in even for people who are new to the language. Spending evenings at the London Clojure Dojo was one of my favouite ways to use time as every week there would be new people to play with in whatever bizarre challenge had been set by the group.

That community was instrumental in keeping me going with the journey as I wrote [CravenDB](http://robashton.github.io/cravendb/) and did my best to learn through that action. Over the year I wrote that I entered the top 100 committers on Github and entered the top five committers for Clojure itself. Being able to spend some time at [MastodonC](/entries/the-use-of-clojure-in-the-cdec-open-health-data-platform.html) working on some unrelated OSS in Clojure was also an amazing experience and cemented my love of the humble paren and the people who wielded them.

Clojure is therefore a great gateway language in that it makes functional programming accessible to anybody with the JVM installed.

Concurrency
===

Clojure had a goal of making concurrency on the JVM easy with its built in constructs for utilising STM. Atoms, agents and simple support for transactions meant that managing access to shared data structures was very simple indeed. Then core.async came along and pretty much took over every library and application I tried writing with it. Core.async definitely made the experience of writing Clojurescript more elegant given its hosting environment and the forced asynchronisity of the JavaScript world. Setting up your entire application as a series of communicating sequential processes around managable chunks of state is definitely something to be celebrated in that ecosystem.

This is where the love affair starts to unravel however as there is some amount of pain this world.

The pain
===

*Stack traces and errors*; Ever seen a Java stack trace? Now add a few more pages of scrolling for all the Java written to make Clojure possible (shudder) - now make those errors occur inside a core.async block and be amazed if anything useful gets  dumped out as your application ceases to work.

*Start-up time*; build a real application, now it takes 30 seconds to start-up on my MBA before hitting any actual code. The ardent Clojurites therefore commit a great deal of time and energy avoiding ever having to bounce the REPL - indeed designing their entire systems around development from a REPL standpoint. That isn't such an awful thing because I really enjoyed the feedback loop that REPL-first development provides and keeping small parts of your system bootstrappable makes it easy to write tests when needed but having this start-up time on my pet database and having to re-engineer it around not wanting to bounce the REPL just felt awkward.

*The real world*: Occasionally when programming it becomes useful to do things like read and write from files/sockets/etc. In Haskell we talk about Laziness requiring Purity, in most other places we're Strict and Impure (Unless you count the 100s of gloriously awful things done in the name of LINQ/C#). Clojure is both Lazy and Impure with no real control over where those side effects take place. You can call a seemingly pure function that calls a dozen other pure fuctions and at the bottom somebody is holding onto an atom just to spite you. That's a made up problem but replace that atom with a file handle and we start to have issues.

*Types*: Or lack of; we're lazy and impure and we have no type system, combine that with a complete lack of decent error handling constructs and giant stack traces and say hello to wasting hours debugging problems if they're so cruel as to slip through your REPL driven development process. Yes there is core.typed which is a wonderful project but it makes Erlang's spec notation looks beautiful and completely ruins the elegance of the original code. A big bucket of nope.

*JVM*: 'nuff said.

Rather than just repeat myself, there is a [10 minute video](https://skillsmatter.com/skillscasts/6040-resource-management-in-clojure) and [slides](http://slides.com/robashton/resource-management-in-clojure) of me trolling a Clojure conference with a talk on this very subject.

The path towards Erlang
===

So okay; simplifying things a lot - you either need to pass file handles (or something that represents them, so handle handles) up to the users of your library for short lived access or wrap up long lived resources in core.async blocks to manage concurrent access to them.

This ends up looking like this (keeping most of the code out of the core.async block so it can be tested in the REPL easily)

    (defn go-index-head [_ {:keys [command-channel] :as engine}]
      (debug "being asked to start")
      (go
        (loop [state (initial-state engine)]
        (if-let [{:keys [cmd data]} (<! command-channel)]
         (do
          (debug "handling index loop command" cmd)
           (recur (case cmd
             :schedule-indexing (main-indexing-process state)
             :notify-finished-indexing (main-indexing-process-ended state)
             :new-index (add-chaser state data)
             :chaser-finished (finish-chaser state data)
             :storage-request (storage-request state data))))
          (do
            (debug "being asked to shut down")
            (wait-for-main-indexing state)
            (wait-for-chasers state)
            (close-open-indexes state))))))

Basically re-invent actors but without decent error handling, supervision trees, named processes... So yeah - Clojure, glorious syntax, clever libraries, great data structures, but it's not Erlang for getting things done in the kind of projects I'm working on at the moment.

Where is it then?
===

I can't see myself using Clojure as a backend language on its own anytime soon. If I'm forced to do something on the JVM (legacy integration, cross my fingers I haven't got to do that for a while) I can see myself using Clojure to integrate with the legacy syste and export data using the wonderful library Liberator.

I *can* see myself using Clojurescript rather than the mess that is JS if I have to do any higher value front-end code. (Some of the React wrappers look *amazing*) and it's a lot more professional than trying to bodge it together in a broken language. There is a project coming up next year that represents value on the front-end and I suspect it'll make an appearance there if I can persuade our editors to do sensible CLJS/REPL integration and I can persuade my colleagues to adopt a REPL driven development method on the front-end.

*Position on the curve:* Still interested, I just don't have use for it at the moment.


