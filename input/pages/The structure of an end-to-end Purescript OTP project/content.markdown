All the posts so far..

- [Introduction to Pinto/Stetson - Opinionated Bindings to OTP/Cowboy](/entries/introducing-pinto-and-stetson---opinionated-purescript-bindings-to-otp-and-cowboy.html)

Useful links

- [demo-ps](https://github.com/id3as/demo-ps) The demo codebase we're talking about here
- [erl-pinto](https://github.com/id3as/purescript-erl-pinto) (the opinionated bindings to OTP we're using)
- [erl-stetson](https://github.com/id3as/purescript-erl-stetson) (the opinionated bindings to Cowbou we're using)


The structure of an end-to-end Purescript OTP project
==

Our [demo-ps](https://github.com/id3as/demo-ps) can be viewed as two separate chunks of code, the base layer is just a plain old Erlang application built using rebar3 and such, and then on top of that we have a pile of Purescript that compiles into Erlang that can then be compiled and used by the usual development workflow.

# The Erlangy bits
- *release-files*: Assets to be shipped during the release process
- *src*: This is usually where the Erlang code lives, but there is no Erlang code
    - *demo_ps.app.src*: The entry point, just points at a Purescript module, we'll talk about that
- *rebar.config*: Erlang dependencies and such
- *priv*: Assets/files we want access to from code (static html/js/etc is covered here)

# The purescript bits
- *server*: The Purescript that we want to compile into Erlang lives here
- *client*: The Purescript we want to compile into JS lives here
- *Makefile*: Turns the Purescript into JS/Erlang
- *shared*: Contains Purescript we'll share between JS/Erlang

In an ideal world, we'd just have a single Purescript entry point and forego our interaction with the Erlang world, but this would involve building out a lot more tooling - the result of this, is that sometimes you will be bringing Purescript dependencies down that require Erlang dependencies and then adding these to rebar.config and the entry point will be your responsibility.

The purescript dependencies can be found in in *psc-package.json* inside the server and client directories, and the Erlang dependencies can be found in rebar.config at the top level.

As a team already familiar with the Erlang ecosystem, this doesn't represent a hardship for us; but this definitely represents an area which could be improved by an enterprising developer or two, probably a plugin to the Purescript stack that stashes the rebar assets/etc in another build folder and allows us to just write PS/Erlang in the right place. (But this would then also involve modifying our editor plugins to know about this new structure, and as you can already see, it's a lot of work when we have something that is already functional..)

# That entry point then

```erlang
{application, demo_ps,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { bookApp@ps, []}},
  {applications,
   [kernel,
    stdlib,
    lager,
    gproc,
    recon,
    cowboy,
    jsx,
    eredis
   ]},
  {env,[]},
  {modules, []},
  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
```

One of the key things to note here, is that we have cowboy as a dependency, this is to support (as mentioned), the Purescript libraries that binds to it ([stetson](https://github.com/id3as/purescript-erl-stetson) and [erl-cowboy](https://github.com/purerl/purescript-erl-cowboy). 

The other big note, is that entry point module is *'bookApp@ps'* - that module can be found in server/src/BookApp.purs, which defines a module *'BookApp'* - the Purescript compiler will compile Purescript modules into *<moduleName>@ps*, as this is unlikely to clash with anything else in the global application namespace. Beyond this entry point there is no Erlang code in the application itself - it's Purescript all the way down...

The Makefile in *server/Makefile* does the work of compiling this Purescript into Erlang that can then be compiled by the usual rebar3 toolchain. The gist of the below Makefile being that we take all the .purs files lying around in the 'server' folder, and compile them into .erl files that end up in ../../src/compiled_ps.

We'll go into detail on the Purescript stuff in the next post, as that's the key; we put a pile of Erlang supporting files in the right location, and then write PS in the other location and everything "just kinda works".
