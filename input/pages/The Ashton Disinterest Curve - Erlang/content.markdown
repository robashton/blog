I've complained about [.NET](/entries/the-ashton-disinterest-curve---c.html) (nothing has changed by the way) and [JS](/entries/the-ashton-disinterest-curve---javascript-and-node.html) (or here) and I've been pretty nice about [Clojure](/entries/the-ashton-disinterest-curve---clojure.html) (or so I thought) and I've managed to delay doing anything about Erlang at all because reasons.

Erlang - mundane brilliance
===

I'm not passionate about Erlang; I've never been passionate about Erlang and I've never hated it either. Perhaps this explains why I'm over a year into working with it professionally and I still don't feel the need to talk loudly about it one way or another. (That and I have a great team of colleagues, work remotely and build pretty awesome products that are actually being used by people - okay then).

Let's get something out of the way then, I'm not an Erlang evangelist - I was even asked in a talk the other day what my three sentence elevator pitch for Erlang would be and I haven't got one. If you want to put in the effort to work in the Erlang ecosystem then the rewards will become apparent fast enough but trying to explain how exactly such an ugly language contributes positively in any way to our product quality/stability in a tidy soundbite just isn't something I'm interested in doing.

Erlang is Ugly
===

Contrived bit of code I just wrote in the airport:

```erlang

    -module(greeter).

    -export([hello/2]).

    -record(state, {
        targets :: list(target_config())
      })

    init() ->
      spawn_link(fun chat_loop/0).

    hello(Pid, Str, Names)
      case Names of
        [] ->  ok;
        [ Head | Tail ] ->
          Pid ! { hello, Str, Head },
          hello(Pid, Str, Names)
      end.

    chat_loop(State) ->
      NewState = receive
        { hello, Str, Name } ->
          Target = lookup_name(Name),
          Target:write(Str),
          State,
        { register, Name, Cfg } ->
          register_name(State, Name, Cfg)
        after 1000
          housekeeping(State)
      end,
      chat_loop(NewState).

    register_name(State = #state { targets = Targets }, Name, Cfg) ->
      State#state { targets = [ { Name, Cfg } | Targets ] }.
````


Or whatever. This is all fairly standard code and certainly there is nothing unusual in here that you'd not expect to see in any old module from around the place. This is probably about 5% of the syntax of Erlang and you can start to see why a lot of people start to pick up Erlang via the medium of "Learn Erlang for Great Good" or something similar and after a week of syntax lessons throw their arms in frustration and exclaim "What on earth is the point?" and either switch to Elixir or something even worse (okay it's relative) like NodeJs.

No *real* type system, no polymorphism, hilariously inconsistent APIs for processing lists, queues, dictionaries (even within the list API itself it's not consistent with the argument orders etc), no real way of doing function composition or guaranteeing purity in functions (which means guard clauses can only use built-ins) and what you really have is a way of writing locally "pure" code across imperative operations on networks and filesystems.

Erlang has moving parts
===

How do you do releases? How do you build your application? How do you test it? How do you run it? What do you need for all of this to work? A quick scan of an "empty" project built from scratch just now on my machine contains:

  - Makefile (for building stuff)
  - Relx (for releasing stuff)
  - sys.config (for configuring stuff)
  - relx.config (for configuring the release)
  - foo_app.src (a manifest for the application)
  - foo_app.erl (the application entry point)
  - foo_web.erl (a gen server for spinning up the web process hierarchy)
  - foo_sup.erl (a supervisor for managing the process hierarchy)
  - foo_config.erl (a little wrapper for accessing sys.config)
  - a 'deps' folder containing "gproc (process utils), lager (logging), cowboy (web)

And a few other bits. This is astonishingly daunting and no amount of "just use rebar" will ever make that easy. I'm generally against scaffolding because it allows awkward design to survive but you almost *need* to scaffold the basic Erlang application somehow because of all the bits you need to just spin it up. Contrast that to a simple node project with its app.js. node_modules and ... oh wait you can pretty much write something useful from this point and *discover as you go* while still building something tangible that somebody might want to use.


Oh, and unlike Clojure there is no real REPL (stop saying erl is a REPL or like a REPL because it really is not a REPL and it offers none of the same experiences you'd expect from a REPL and nobody does applications which can be bootstrapped easily via a REPL and there is very little tooling support for talking to this from your editor so really it's nothing like Clojure omg stop you guys)

At this point the folk in the Erlang community reading this will be thinking "but it's not quite like that - once you have a workflow it's all sensible enough" or "You don't need most of those things for an application really" or "why would you want to do things in a REPL anyway?" or all sorts of self-deluding statements because they've been blinded by some of the better things in Erlang and things like working and profitable products for years and that's sort of okay but in a way also really frustrating because it seems that after a few years of Erlang development Erlangers seem to forget just how awful some of the tools they're working with actually are.

Erlang's OSS is moving sands
===

Every few months we decide we need to make http calls from our applications and we have to do the dance of working out which library is still being maintained or compiles in the current version of Erlang, ibrowse caused us issues until we switched to lhttpc but unicode and R17 really pissed us off and then lhttpc was deprecated so we looked at gun (which uses maps) but actually shotgun is a better wrapper but there is also fusco except that's not ready yet yes it's 2015 and we don't have a consensus on a http client. (Actually Gun is pretty neat and there are reasons to be re-writing http clients with the advent of http2 etc but okay this is a little frustrating and a small example of the ecosystem).

The language itself is still evolving and some libraries just stop working between releases, there aren't too many active users of most Erlang libs once you get out of the basic world of http and databases and you will find bugs and you'll have to fix those bugs and you'll have to invest time in doing that if you want to be effective in this world - so you'll need to be a competent Erlang developer to build an Erlang application but is that such a bad thing given that 90% of developers working in their chosen language are incompetent and at least we have this as a safeguard to keep that sort of person out? I really don't know I'm just thinking out loud).

Oh - and every month I find another Erlang library that hasn't written their manifest properly so doesn't work in the releases that we build using relx and we're relying on dangling forks or commits of quite a few projects because nobody seems to be around to take our call for fixing them or merging our fixes.

It's not brilliant but...

Erlang does seem to work
===

It doesn't just work, it excels. Once you've fought and made peace with all of the above (and more) and you're using the bash scripts and makefiles that the rest of your company is using for automating the build/test/release process and you've spent the time learning how all of it fits together then you can spend some time looking at supervision trees and process ownership and you realise that it's pretty hard to crash an Erlang application and leave anything open or dangling or in a weird state if you reasoned about your supervision structure at all.

OTP is pure wonder (gen_server, supervisors, applications etc) and the libraries that ship wth Erlang using the process model and underlying abstractions are battle-tested, stable and well thought out.

Consider that I can write the following code anywhere in any old gen server and if it fails (IE, write doesn't return 'ok', the entire process tree crashes and subject to the rules in the supervisor either will be restarted, kill siblings or parents or pass the error up automatically to the next supervision level and we'll have a log about all of this including the current state of the offending process and there won't be a dangling open handle to the file and a whole bunch of other useful "none-artifacts" that you'd easily overlook if you hadn't put in the time to avoid them.

    ok = file:write(Handle, Bytes).

Not having to dance around exceptional error cases in most file or networking scenarios and writing your code on top of these built-ins means you can spend more time writing the code you need to write for the feature itself (Okay admittedly if you've gotten around the syntax issues). This philosophy and things like it hold true across the various libraries we use from across the ecosystem and mean we don't tend to get too many support calls at 2am because web servers have disappeared because something is locked, crashed, down permanently, corrupt or whatever.

Easy inter process communication means it is easy to build self-contained little workers around little balls of state and not worry about concurrency (most of the time) because everything is safe if you're following the happy path. Generally it also means that shifting cpu heavy stuff around our stack is easy because we can always take one of these processes and spin it up somewhere else. I mentioned that in Clojure we ended up with Actors in core.async but without error handling or safe handle management and here is the answer neatly packaged for us in a platform that has been around and battle-tested for 10x as long. Neat.

But what about Elixir
===

If I mention Erlang, this is always the first thing that comes up and it sorta annoys me. I have a few issues with Elixir and they're not really to do with the language itself because y'know, I couldn't care less about syntax in general but I do care about philosophy/focus. It's irrational but here goes:

1) I've worked in Ruby, the Ruby community might be friendly but they're mostly godawful developers and their tendency to overload operators with magic, or monkey patch internals or generally do any sort of meta-programming because ahahaaha lols made me almost quit programming altogether in frustration when I stared into that abyss for the duration of that job

2) Elixir comes from that background, with that sort of developer and now with ADDED MACROS (oh man no don't give these kids more metaprogramming tools)

You see - Clojure is allowed to have Macros because the rules of Macro club are clearly written down and stated as thus:

- Don't use Macros
- Don't use Macros
- Don't use Macros
- Okay, re-write those rules - we're going to use Macros because our name is Rich Hickey

This general sensibility means that you don't generally bring in libraries in Clojure and then have to wonder why everything in your application no longer works because somebody decided to re-define basic mathematical operators (for those not doing Ruby *yes this is actually a thing I don't know what even*).

Erlang doesn't cost us any money because we can't pretend records/maps are objects, we don't need that functionality. Erlang doesn't cost us money because we can't do meta-programming (actually we sorta can but sssh don't tell the Ruby devs) - and while the syntax might be ugly it isn't something that is a fundamental issue beyond the initial learning curve because you should be learning OTP, not the language. Putting the focus on that shiny language takes a lot of focus away from the things in Erlang that are actually *useful* as in the ode above. This is further chronicled by the sheer number of blog entries of "Ruby vs Elixir", "Node vs Elixir" etc as if somehow the language is at all anything you should be interested in. I find it incredibly hard to trust Elixir, anything written *in* Elixir or anything written by the Elixir devs (see Ruby metaprogramming above).

The new and shiny detracts from the old and gnarly without really adding that much in tangible benefits and bringing in the hipster brigade who all totally missed the point when it came to the bit in the chapter "What do you mean you can't re-assign variables".

If you're a great dev then you can probably be more effective in Elixir (probably), but bear in mind they're re-building all the shitty infrastructure around packaging management, build tools, scaffolding etc that it has been quite a relief to stay away from in this last year of doing Erlang (I'll cover this in the upcoming series). So thanks but no thanks - you are all incorrect in your thinking and I'll see you in a couple of years when you work that out for yourself.

LFE
===

An interesting idea, and we're looking at trialling it in our low value webby bits side by side with the Erlang (because it's not trying to do anything magic beyond offering a fairly simple syntax change - you know I love a good LISP). At the moment there are a pile of things that we'd want in it (I found some issues the first six hours I jumped in and while Robert Virding is very fast at fixing them we'd need to keep it in low value code while we helped grow the project).

Bonus: It's not being ran by Ruby script kiddies, so that's a Good Thing (tm).

State on the disinterest curve
===

Still not really that interested in Erlang, simply building things in it and enjoying it - this process is pretty mundane and perhaps that's what I've been looking for all this time. I'm going to throw in some more blog entries after this one covering how *we* build things in Erlang because we do things *our* way and I've been asked about this a few times now. This should be fun.



