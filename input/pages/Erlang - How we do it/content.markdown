Having [established](/entries/the-ashton-disinterest-curve---c.html) that I [apparently](/entries/the-ashton-disinterest-curve---javascript-and-node.html) [hate](/entries/the-ashton-disinterest-curve---clojure.html) [everything](/entries/the-ashton-disinterest-curve---erlang.html), let's get a bit more constructive and go over how at the company I work at, we do Erlang and do Erlang effectively.

Let's start off with a little history first though to build context for this series... I'll use "we" a lot, but what I really mean is "me and my current understanding of our way".

Where we are
==

There were a couple of .NET developers, and they had need to build a distributed system and they looked at .NET and went "hell no, let's do that little bit in Erlang" and then a little bit later "Oh sod it, let's do everyting in Erlang this is actually great". Lots of mistakes were made over the next few years and they were learned from as they were made. Eventually a developer called Rob (me) joined the team and most of the important mistakes had already been made and a lot of decisions had been made about how best to be effective at Erlang development.

We've been through our own custom build systems/runners, to using [Rebar](https://github.com/rebar/rebar), to using fairly [poorly written](https://github.com/ninenines/erlang.mk) Makefiles to using [reasonably written](https://github.com/fenollp/erl-mk) Makefiles to just forking the best one and making it [our own](https://github.com/id3as/erl-mk). We've been through the "let's make everything a gen server" to "why do we need gen servers anyway" to "okay, here is the happy medium". We've had bash scripts lying around a bunch of projects for automation purposes that have diverged from each other, converged again and been merged into a "[single tool to rule them all](https://github.com/robashton/vir)". We have sensible ways to build up APIs between gen servers, between running applications and we have a common swiss army knife of a common library that every time I go to write Erlang without for personal stuff I wonder how on earth I ever got on without.

It's a pretty mature stack and because we own most of it outside of our basic dependencies we're not subject to the whims of third parties changing their minds about how projects should be written and managed. Leaning on a large platform like Erlang is relaxing because it has been around for a few decades and generally doesn't undergo massive shifts every year just because some hipster somewhere decides they don't like *that sort* of paren or semi-colon.

We're pretty much against package managers, binary dependencies or other opaque tooling that we can't understand and simply make our own. We've learned the hard way that sometimes the best tools are the ones that we've all been using since the dawn of time (Bash and Make) and slowly over time these ideas have converged and become crystalised as a standard set of tools we all understand and are happy with.

A series then
===

So we have these bash scripts and makefiles and our standard application structure and while we've been using them for years in various forms we've never really publicised that they're actually OSS on Github. We've never talked about how to use them to build Erlang applications (new starters? Pair for half a year and you'll have it all down like second nature anyway).

Now that most of this has stabilised (for now), it's quite a good time to write a blog series about our workflow, how to bootstrap a new application, how to do builds and releases and best practises around gen servers and processes and APIs and inter-process communication. We're also heavy Docker users (for now) so I'll cover our loose workflow around how we use that for development etc.

On board? The next entry will be about bootstrapping an empty application.
