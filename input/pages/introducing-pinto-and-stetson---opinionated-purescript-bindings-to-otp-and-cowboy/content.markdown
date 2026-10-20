If you're reading this, you've either been given the link to the posts ahead of time cos you've asked to see what is going on, or I've hit the publish button in which case hooray. Either way, this is a little series of posts going through some of the Purescript/Purerl code that we've quietly open sourced on Github under the Apache 2.0 license. Hopefully betwen these posts, the published markdown docs and the sample application there will be enough to get started with.

Over the last year or so, we've been gradually building out our capacity to create applications end-to-end in Purescript, compiled to JS on the front-end and compiled to Erlang on the back, building on top of both OTP and our existing libraries from nearly a decade of company history doing business on top of the Erlang stack.

The repositories we're looking at are:

- [Purerl](https://github.com/purerl/) itself
- The [purerl-package-sets](https://github.com/purerl/package-sets)
- [Stetson](https://github.com/id3as/purescript-erl-stetson) (Opinionated Cowboy bindings)
- [Pinto](git@github.com:id3as/purescript-erl-pinto.git) (Opinionated OTP bindings)
- [demo-ps](https://github.com/id3as/demo-ps) (end-to-end sample code)

The best place to start if you want to dive right in, is probably the demo-ps project as it demonstrates the usage of most of the above, and that is indeed where we'll be starting in this series.

Purerl
==

The Purerl organisation contains the core sets of bindings to much of Erlang's base libraries, as well as the fork of the Purescript compiler that can generate Erlang as a backend. 

Purerl-package-sets
==

Essentially a pile of Dhall that generates a package.json containing a list of versions of the various Purerl libraries that work together, you'll not need to touch this directly unless you end up using Purerl internally in an organisation and you want to fork it and add your own internal/private Purerl dependencies.

Stetson
==

[Cowboy](https://github.com/ninenines/cowboy) is the de-facto webserver in the Erlang world, and [direct bindings](https://github.com/purerl/purescript-erl-cowboy) exist for the project already, however when it came time to start building applications on top of this, it was clear that there was little gain to be had by directly using them over simply writing Erlang in the first place. Stetson was my attempt to mirror the experience I've had in other functional languages using libraries such as [Compojure](https://github.com/weavejester/compojure) and [Scotty](https://github.com/scotty-web/scotty). It isn't by any means complete, and merely serves as a statement of intent around the kind of interaction I'd personally like to have around routing/etc in a Purerl world. I fully hope/expect that somebody will write a native http server in time rather than simply wrapping Cowboy as I have done here.

Pinto
==
There have been a [few examples](https://github.com/purerl/purerl_otp_sandbox) written demonstrating how to interact with OTP from Purerl, but again at the point of building a real application, direct bindings don't offer a good user experience once you start building out functionality and repeating yourself a whole ton. I cheated a lot when putting together Pinto and skipped the direct bindings step, going straight to the "desired usage" step and doing a pile of cheats around types and such. It seeks to largely mirror the existing OTP interactions, but in a more functional manner. Much like with Stetson, I fully expect/hope that in time somebody (maybe even us) will want a more idiomatic Purescript experience and choose to build something even more opinionated outside the familiar comfort of the OTP vocabulary. For now, we have Pinto.. :)

Demo-ps
==
This is a completely pointless web app that uses [purescript-erl-stetson](https://github.com/id3as/purescript-erl-stetson), [purescript-erl-pinto](https://github.com/id3as/purescript-erl-pinto), [purescript-simple-json](https://github.com/purerl/purescript-simple-json) and [purescript-halogen](https://github.com/slamdata/purescript-halogen) to store data in Redis using some FFI and display it in a single page application, sharing the view models between the server and client components. It seeks to demonstrate rough usages of all of these without cluttering up the interactions with "real code" (read: business logic).

Next post, we'll look at the structure of the demo-ps project, as understanding this is essential if you wish to build your own.
