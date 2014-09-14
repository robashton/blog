I somehow ended up doing a talk at #jsconfeu this weekend on "make" of all things, submitted after a night of partying with all the wrong people who thought it would be a hilarious idea.

Anyway, it happened and the slides at demos are available online. ([http://slides.com/robashton/got-make](Slides) // [https://github.com/robashton/jsconfeu2014](Demos) ). Here is the blog post version of that talk because it is unlikely I'll ever do it again.

Why Make
===

The long and short of this is that I work on a team of cross-platform developers on a project comprised mostly of Erlang and C. Make is the natural choice for this and indeed we have a common make file across our projects found at [https://github.com/id3as/erl-mk](github/id3as/erl-mk).

Our core codebase is shiny diamonds, polished diamonds; It's stable and relatively robust, and we string together a lot of it with the programmer equivalent of duct tape (bash scripts). Make is for building, Bash is for task execution and this separation works very well for us.

Bringing in "yet another build system" just for JS would wind everybody up something chronic, especially when the build systems available in the JS world are confused about whether they are task runners or build scripts and the heinous mess of either JSON or JS streaming code found in this environment is pretty off-putting.

So yeah, we use Make because it's there and because it's good at building things; We use Bash because it's there and it's good at running things.

Also Make is just plain fun, it's such a quirky thing to work with how could you not enjoy it? (If you're working in JS you already know what it's like to work with something quirky and the two are therefore a match made in heaven)

All the examples shown in this entry assume you're in the directory with a file called "Makefile"

Make is not a procedural language
===

So lesson number one in the land of Make, is what happens in the following file when we run "make"?

```

one=$(two)
two=$(three)
three=hello world

all:
	@echo $(one)

```

Well obviously we print out "hello world" to the console, why is that? Because anything assigned with '=' is a recursive variable, and they'll be recursively expanded at the point of use. (So in this case "one" isn't used until we echo it, at which point both two and three have values and we get a good result).

This is important because it impacts how we use expressions and functions later on in the Makefile.

Targets, Pre-Requisites and Recipes
===

A makefile is comprised of variables (as we see above) and recipes (which are a combination of targets, pre-requisites and some instructions to generate those targets)

For example

```
out/file.txt: in/file.txt
  cp in/file.txt out/file.txt
```

The above is not idiomatic, but serves to highlight that a target is *usually* a file or directory and the pre-req is often a file/directory to a file/directory too. The instructions in the recipe simply instruct Make how to generate that target from that input.

Because we've stated our targets and pre-reqs in terms of files, Make can check the timestamps of these artifacts and only run the command if the pre-req is newer than the target.

DRY with Pattern Rules
===

Given this Makefile, what stands out?

```

all: out/pinkie.txt out/rainbow.txt
	@echo > /dev/null

out/pinkie.txt: in/pinkie.txt out
	cp in/pinkie.txt out/pinkie.txt

out/rainbow.txt: in/rainbow.txt out
	cp in/rainbow.txt out/rainbow.txt

out:
	mkdir -p out

```

Well we're repeating ourselves a lot for what is a simple file copy. Make gives us "pattern rules" to help with this. Targets can be defined in terms of "patterns" and then we match on the pattern rather than an exact.

```
all: out/pinkie.txt out/rainbow.txt
	@echo > /dev/null

out/%.txt: in/%.txt out
	cp $< $@

out:
	mkdir -p out

```

Woah, what is going on here? Well, the percentage symbol is the pattern we're matching on, and we're still invoking that target for both out/pinkie.txt and out/rainbow.txt. Make then gives us *automatic variables* to work with; There are a good dozen of these available, but in the above example we're using

- **$<** (The *first* pre-requisite that triggered the execution of this recipe - in/pony.txt in this case
- **$@** (The full name of the target being matched, in this case out/pony.txt)

Of course, we're still manually entering out/pinkie.txt and in/pinkie.txt and that's sub-optimal so let's sort that out too.

Using built-ins to generate targets
===

Various functions are available to us in Makefiles, and what we want to do is generate a list of *outputs* to invoke our targets with.

The only thing we have to work with is a directory full of inputs, so let's go and find all of those first

```
INPUTS := $(wildcard in/*.txt)
```

When the value of INPUTS is expanded, it'll contain a list of files matching the pattern, in this case that's the value "in/pinkie.txt in/rainbow.txt"

Our desired outputs actually have the same name at these except they're in a different directory, that's okay because we can call another function to replace all the 'in's n the INPUTS with 'out's

```
OUTPUTS := $(patsubst in/%,out/%, $(INPUTS))
```

Putting all of this together, our Makefile now looks like this

```
INPUTS := $(wildcard in/*.txt)
OUTPUTS := $(patsubst in/%,out/%, $(INPUTS))

all: $(OUTPUTS)
	@echo > /dev/null

out/%.txt: in/%.txt out
	cp $< $@

out:
	mkdir -p out
```

This is much tidier and this forms the basis of many a Makefile.

- Scan for the inputs
- Generate the names of the outputs from this list of inputs
- Invoke targets with lists of outputs, with pre-requisites as the list of inputs

A borderline real-world example
===

Referring to the example found on Github here: [https://github.com/robashton/jsconfeu2014/tree/master/src/simplewidgets](robashton/jsconf2014)

We have a folder layout that looks like this

    /
    site/
      index.html
    widgets/
      pinkie/
        img/
          various-images.png
        index.styl
        index.coffee
      rainbow/
        img/
          various-images.png
        index.styl
        index.coffee
      celestia/
        img/
          various-images.png
        index.styl
        index.coffee

What we want to do is generate

    site/widgets.js
    site/widgets.css
    img/*

From the widget folders, and we want to do so as efficiently as possible and in a way which means that Make has a good chance of not doing repeat work.

I like to start with my desired outputs and work out what I need to get there, in this case, ignoring the images this means

    widgets.js <- cat widgets/*/*.js <- coffee -c widgets/*/*.coffee

and

    widgets.css <- cat widgets/*/*.css <- stylus widgets/*/*.styl

(This does mean that requires won't work in Styl, but it doesn't require a lot of imagination to stretch this makefile to only run on index.styl and declare other styl files as dependencies of it)

So, first we need to bundle up our inputs

```
COFFEEFILES := $(wildcard widgets/*/*.coffee)
STYLUSFILES := $(wildcard widgets/*/*.styl)
```

And generate our lists of outputs

```
OUTPUTJSFILES := $(patsubst %.coffee,%.js, $(COFFEEFILES))
OUTPUTCSSFILES := $(patsubst %.styl,%.css, $(STYLUSFILES))
```

Then our code path is quite simply

```
site/widgets.js: $(OUTPUTJSFILES)
	cat $^ > site/widgets.js

widgets/%.js: widgets/%.coffee
	coffee -c $<
```

And our CSS path is quite simple too

```
site/widgets.css: $(OUTPUTCSSFILES)
	cat $^ > site/widgets.css

widgets/%.css: widgets/%.styl $(STYLUSFILES)
	stylus $<
```

Images are a little more complicated because we're cheating and flattening the structure (everything from each /img folder goes directly in the output /img folder, so name clashes could happen. In the real world we'd probably stick things in a per widget folder or more likely generate a sprite map.)


```
SOURCEIMAGES := $(wildcard widgets/*/img/*.png)
COPIEDIMAGES := $(addsuffix  _lastcopied, $(SOURCEIMAGES))
```

What's this _lastcopied business? We'll see


```
widgets/%_lastcopied: widgets/% | site/img/
	cp widgets/$* site/img/
	@touch $@
```

We use a dummy file which we "touch" every time we copy an image, this is because in this example we lose the relationship between the input and output file. (This isn't necessary, it's just a demo to show you could do this). Make can then compare timestamps between that _lastcopied file and the image itself to determine whether it needs copying.

Our make all instruction now simply looks like this

```
all: site/widgets.js site/widgets.css $(COPIEDIMAGES)
	@echo > /dev/null
```

For bonus points, we can run make with

    make -j

**And all of this can be done in parallel!!**. Most of the time it'll only be compiling a couple of coffee files anyway and will be super fast because Make doesn't like to do extra work.

Makefile re-use
===

So we've written an awesome makefile that generates spritemaps, compiles various languages into JS (coffee/JSX/whatever), and then executes browserify against this and perhaps even scans node_modules for templates/stylesheets/images/etc (Well why not eh?)

It'd be a shame not to use these conventions across our projects, but copying a Makefile into all those projects seems like an awful idea because you don't want all the projects to be the same but "slightly different".

How about using wget?

Our makefile can look something like this

```bash
export WIDGET_DIR=baubles

common.mk:
	@wget -nv -O $@ 'https://raw.github.com/robashton/jsconfeu2014/master/src/_assets/common.mk' || rm -f $@

-include common.mk
```

Note the "export", we can see how this is used in common.mk over here

```
WIDGET_DIR ?= widgets
.DEFAULT_GOAL=all
```

Nice - we can configure the makefile if we decide our widgets should be called baubles, for bonus points we then execute "all" by default, so just typing "Make" will result in this file being downloaded and then the project being compiled. Mega wins.

*Note: While it is seemingly nice to use a package manager, for widget type designs like this, it's enough to just stick them into Github and recursively download the repos and either execute their make files directly or if they haven't got one, re-execute our make file in the context of that directory. (That's what we do in Erlang instead of using Rebar, but projects that have rebar or their own makefile work fine then too because we're just calling Make anyway).*

I have no opinion on whether your team should do it one way or another.

The difference between task runners and build systems
===

I think most of the Grunt/Gulp whatevers and examples of usages of those I've seen in the wild get it pretty hilariously wrong because they tread a weird line between acting as task runners and build systems. What's even worse is when you see Makefiles that look simply like this

```
all: build-js build-css

build-js:
  browserify -t brfs src/app.js > site/app.js

build-css:
  stylus src/style.styl > site/style.css

etc:
  blah


.PHONY build-js build-css etc
```

For plain old task running, just stick things in npm scripts, for build processes use a build tool. In our projects this means Bash or Make but whatever works. Make's power is in generating dependency trees and only evaluating targets that are out of date (and then parallelising them). If this isn't being used then it's a bit pointless to be using Make.

It goes the other way, if we're spending a pile of time trying to set up dependency structures in whatever task runner is the current hotness, perhaps looking backwards to proven technology might be a sensible notion.

And that's it
===

I hope this was useful if you've not considered make before or have considered make and were put off, it's a fine piece of software full of magic incantations and hilariously clearly added afterthoughts.




