We don't tread the same path as most Erlangers, as [mentioned](/entries/the-ashton-disinterest-curve---erlang.html) having been through more than a few of the standard build systems in that ecosystem we've settled on our own (the original hard work done by somebody else). All on top of a [pile of bash](http://github.com/robashton/vir) that organically came from real world production use of Erlang.

So why Make?
==

Well firstly we already know it; our three languages at work are C, Erlang and (*spit*) JavaScript. Secondly if you look at an Erlang project and compilation of that Erlang project you'll see that we have a bunch of files that need compiling into another format (mostly independent to each other), let's have a wee look at that.

    src/%.erl       ->    ebin/%.beam
    src/%.app.src   ->    ebin/%.app
    src/%.xrl       ->    ebin/%.beam
    src/%.yrl       ->    ebin/%.beam
    priv/mibs/%.bin ->    mins/%.mib

Etc.

If only there was a tool which allowed you to declaratively wildcard a bunch of inputs to a bunch of outputs and use timestamps to determine whether individual files needed re-compiling again. Hmmmmm.


So yeah, this is our fork of [erl-mk](http://github.com/id3as/erl-mk.git), although in a few days this will be renamed to [id3as.mk](http://github.com/id3as/id3as.mk) so check which link doesn't 404 and this will see you right.

Conventions
==

There is a standard structure to an Erlang project, and it looks like this

    relx.config
    src/%.erl
    src/%.app.src
    release-files/sys.config
    release-files/*.anything.else
    include/%.hrl

We can only get away with using a standard one-size-fits-all Makefile if you conform to some convention and given that there is already a convention to Erlang projects this is the one that we are using. Additionally you can also have

    apps/<app-name>/<the same as above>

To have multiple apps in the same project, and

    deps/<dep-name>/<the same as above>

To rely on other Erlang projects and their source code - more on that in a little bit.

Using id3as.mk
==

To configure and use id3as.mk, we use an entry point Makefile to set up some variables and download id3as.mk - this would usually be called "Makefile" or "makefile" and sit in the top level of the project (vir will generate this if you're using it).


    DEPS_DIR = $(addsuffix /deps, $(realpath .))
    ERL_LIBS = $(DEPS_DIR):$(realpath apps)

    export DEPS_DIR
    export ERL_LIBS

    export ERLCFLAGS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +'{lager_truncation_size, 10240}'
    export ERLMIBFLAGS =

    DEPS = lager cowboy gproc jsx

    dep_lager = git://github.com/basho/lager.git 2.0.1
    dep_cowboy = git@github.com:extend/cowboy.git master
    dep_gproc = git://github.com/esl/gproc.git 0.2.12
    dep_jsx = git://github.com/talentdeficit/jsx.git master

    id3as.mk:
      @wget --no-cache -nv -O $@ 'https://raw.github.com/id3as/id3as.mk/master/id3as.mk' || rm -f $@

    -include id3as.mk

I'm not a huge fan of using this for dependency downloads (I'd prefer a bash script) but it's just a single operation at the start to download all dependencies to the DEPS_DIR and then build is just standard Make. You'd still need to specify which dependencies you had because the Makefile uses this to build up a dependency tree and only build each dependency once (in the right order).

Anyway, it's self explanatory - you'll see that in Erlang we haven't got a package manager (although some misguided but well meaning folk are trying to change that), and we just download source into a deps folder and build that ourselves. change that), and we just download source into a deps folder and build that ourselves. change that), and we just download source into a deps folder and build that ourselves. change that), and we just download source into a deps folder and build that ourselves. (No, they're not submodules, *ew*)

For each dep, id3as.mk checks if there is a Makefile present (in which case it'll run that), checks if there is a rebar.config present (in which case it'll run rebar) and falls back to re-executing itself in the dep dir. For all the id3as.mk based dependencies it'll honour timestamps all the way down. Rebar is a little more dumb and once you're in the world of rebar it can be a little slow as it insists on recursing over deps multiple times during a single build (boo, hiss).

I'm a big fan of having dep source available - it means if you build up a tags file for your editor you can jump into the source code of even the third party dependencies you're using and see how they work (what better documentation than the actual code okay just joking devs we should all be writing better documentation).

Make commands
==

- "make get-deps" - initial download of deps
- "make" - build *everything*
- "make apps" - just build the apps
- "make apps/<foo>" - just build the foo app
- "make deps" - just build the deps
- "make deps/<foo>" - just build that dep (useful if you're debugging a third party dep)
- "make rel" - make a release

Neato. Everything is just timestamp checking and then for bonus points

- make -j <anything from above>   - DO IT IN PARALLEL

Seeing as most of these steps and most of the erl/beams are independent of each other a build is much faster if you run it in parallel.

On package managers
==

I often get a few eyebrows raised when I say we don't need one of these - so the next entry will write about why they're unnecessary (in any of the incarnations so far) and why we should do without.
