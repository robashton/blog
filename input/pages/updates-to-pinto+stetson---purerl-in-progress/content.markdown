Whew, a lot of things have happened over the last year and nearly all of it in private Github repos for work as we carry on doing "everything" in Purerl.

These posts are a reminder of where we were last time we poked our heads over the parapet, although some of their content is now a tad out of date the demo code they link to is thankfully updated (I've been busy).

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)
- [The structure of an end-to-end purescript OTP project](/entries/the-structure-of-an-end-to-end-purescript-otp-project.html)
- [Building on top of OTP with Purescript with Pinto](/entries/building-on-top-of-otp-with-purescript-with-pinto.html)
- [Building a Purescript web server with Stetson and Pinto](/entries/building-a-purescript-web-server-with-stetson-and-pinto.html)
- [Shared code twixt Purescript server and client](/entries/shared-code-twixt-purescript-server-and-client.html)
- [Purescript interop with native Erlang, interaction with Redis](/entries/purescript-interop-with-native-erlang---interacting-with-redis.html)

So what has changed?  

- Nix overlays for Purerl/etc
- Typed routing for Stetson
- Emitter based messages for handle\_info in Gen Server
- Monitors for arbitrary pids from Gen servers + Stetson handlers
- WebSocket handlers in Stetson
- Streaming handlers in Stetson
- MessageRouting in Pinto to easily bind to legacy code that sends us messages

Some of these things existed in one form or another last year, indeed it is hard to write a substantial amount of Erlang without requiring them, however as we started building more and more critical functionality on top of Purerl, the warts in the existing implementations started becoming apparent and eventually needed dealing with.

The advantage to eating our own dog food is that we have a lot of mouths to feed and the dog food needs to taste good if we're to keep on going with it.

For those that are super keen, the updated demo code can be found [here](https://github.com/id3as/demo-ps) for perusal, for those who want an explanation, the following blog posts will cover them, for now  I'll quickly cover the various nix packages that are available for us about the place and the tools we are currently using to do our builds.

# Nix Packages + Development Stack

The demo-ps project has been 'nixified', with a [nix-shell](https://nixos.org/download.html) and [direnv](https://direnv.net/) or similar, a fully operational development environment will appear (and yes it works on MacOS).

Our entry point can be found [here](https://github.com/id3as/demo-ps/blob/master/env/common/shell.nix), dumped below for convenience.


```
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.4-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
    };

  pursPackages =
    builtins.fetchGit {
      name = "purerl-packages";
      url = "git@github.com:purerl/nixpkgs-purerl.git";
      rev = "5da0a433bcefe607e0bd182b79b220af980a4c78";
    };


  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import pursPackages)
        (import ./.)
      ];
    };

  inherit (nixpkgs.stdenv.lib) optionals;
  inherit (nixpkgs)stdenv;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    nixerl.erlang-22-3.erlang
    nixerl.erlang-22-3.rebar3

    purerl.purerl-0-0-5

    demo_ps.purescript-0-13-6
    demo_ps.spago-0-12-1-0
    demo_ps.dhall-json-1-5-0
   ];
}
```

[nixerl/nixpkgs-nixerl](https://github.com/nixerl/nixpkgs-nixerl) is maintained  by [@philipstears](http://twitter.com/philipstears), [purerl/nixpkgs-purerl](https://github.com/purerl/nixpkgs-purerl) is maintained by the purerl org ([nick](http://twitter.com/nwolverson)) - the demo-ps project itself  provides some  overlays for purescript/spago/dhall because we want specific versions of them.

I strongly recommend investigating Nix for development workflows based on the last year or so of using it in earnest as a team, we have built our own internal packages for native/shared dependencies across our projects and have started  looking into using it to aid in our deployment as well. It is not without its pain points (it's a lot easier if you just run Nixos like half the team), but it has made managing our dependencies a lot easier. As a way of getting a sensible Purerl development environment up and running it's probably the easiest avenue.

We've pretty much set ourselves on using

- dhall-json for our package sets
- Spago for building
- Purescript is the main compiler
- Purerl is the backend for the compiler
- Rebar3 is the build engine for our Erlang projects
- Erlang is the compiler/VM for... well, Erlang

Getting all of these installed is probably a journey that is going to be differ wildly depending on the host OS and Nix at least gets that solved for us.

# Dhall

There are two dhall files per Purescript project at the moment, there is packages.dhall which describes the package set we are using to pull packages, and then spago.dhall which describes which packaages from that package set we want to use.


At the time of writing, our package set looks like this

```
let upstream = https://github.com/purerl/package-sets/releases/download/erl-0.13.6-20200402/packages.dhall sha256:5442e50aa76c20bd60b2770ab41c68bae80f6ec96f2df1cfaea310673de567d1

let overrides =
      { erl-cowboy =
          { dependencies = [ "erl-modules" ]
          , repo = "https://github.com/id3as/purescript-erl-cowboy.git"
          , version = "4ee391f0349c00d92f68e4331425174eb8bdff9e"
          },

      erl-pinto =
          { dependencies = [ "erl-process" ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version = "59fd04bb0215f532b984909b3cd52bbaf1c10e6a"
          },

      erl-stetson =
          { dependencies = ["erl-atom" , "erl-binary" , "erl-lists" , "erl-maps" , "erl-tuples" , "erl-modules" , "foreign" , "maybe" , "prelude" , "transformers" , "routing-duplex"]
          , repo = "ssh://git@github.com/id3as/purescript-erl-stetson.git"
          , version = "2244181d4905c16f7a62ead62a12a2056eb0c975"
          }
      }

let extras = {
       erl-simplebus =
          { dependencies =
              [ "erl-process"
              , "effect"
              ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-simplebus.git"
          , version = "14b5bd5971e7f2eeba99b51f1c6d4f43761b5376"
          }
  }

in  upstream ⫽ overrides⫽ extras

```

[erl-simplebus](https://github.com/id3as/purescript-erl-simplebus) is a new package not yet added to the [purerl package-sets](https://github.com/purerl/package-sets) so that's been defined as an 'extra', and our cowboy/pinto/stetson repos are a moving target so we're using git commit hashes for them as the versions in the package set are out of date. In general our packages.dhall breath a little as we work on core libraries, stabilise changes and then get official releases pushed into the official package set.

With all of this defined, we can define our build package spago.dhall

```
{-
-}
{ name = "demo"
, dependencies =
    [ "console"
    , "effect"
    , "erl-cowboy"
    , "erl-pinto"
    , "erl-stetson"
    , "psci-support"
    , "simple-json"
    , "erl-simplebus"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
```

This allows us to use spago build on the CLI, which will pull down all of our packages and build them and our Erlang. Note the presence of 'backend' for compiling to Erlang rather than JavaScript - this is a fairly recent change to the Purescript env and definitely differs from my previous blog posts on the subject (We've done away with psc-package for starters).

# Anyway

Look into Nix or install all the dependencies manually, I know which I'd pick these days. Presumably it could be done in a docker container too, but docker is old hat, don't be old hat.
