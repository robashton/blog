Previous entries..

- [Intro](/entries/functional-erlang---purescript-on-the-beam---intro.html)

I guess the first thing we need to do is get some sort of environment up and running on my machine - seeing as this is a [fork of Purescript](https://github.com/purerl/purescript) with an Erlang backend and it's reasonably likely at some point I'm going to have to make a pull request or two, I settled with cloning the repo and running 

    stack build 
    stack install

Haven't got stack? Well this isn't a tutorial, so you can Google that if you need to.

I then did the same for [psc-package](https://github.com/purescript/psc-package) simply because I couldn't be bothered working out a different way, if it ain't broke then don't fix it - that's what I always say, my definition of broke is probably more lax than others though so YMMV.

Armed with a purescript compiler and a package manager, the next step is to obviously get something building.

I decided to base my meanderings off of [pureerl_otp_sandbox](https://github.com/purerl/purerl_otp_sandbox) at least as far as basic structure goes, although I did modify the makefile a tad so it looks a bit like this (I'm sure it's changed by the time anybody will read this, but close enough)

    .PHONY: all clean
    
    PS_SRC = ps_src
    COMPILED_PS = src/compiled_ps
    OUTPUT = output
    
    all: $(COMPILED_PS)
    
    $(COMPILED_PS): output
    	mkdir -p $(COMPILED_PS)
    	cp -pu $(OUTPUT)/*/*.erl $(COMPILED_PS)/
    	touch $(COMPILED_PS)
    
    output: $(PS_SRC)/**/*.purs $(PS_SRC)/*.purs .psc-package
    	psc-package sources | xargs purs compile '$(PS_SRC)/**/*.purs'
    	touch output
    
    .psc-package: psc-package.json
    	psc-package install
    	touch .psc-package
    
    clean:
    	rm -rf $(OUTPUT)/*
    	rm -f $(COMPILED_PS)/*
    
This is added to a hook in the rebar.config so it happens automatically on rebar3 compile

    {pre_hooks,
      [
       {"(linux|darwin|solaris|win32)", compile, "make"},
       {"(linux|darwin|solaris|win32)", clean, "clean"}
      ]}.

Essentially, I can write an Erlang app as if ever ordinarily did, and as long as I have some .purs files in a folder called "ps_src" they'll get compiled into beam and everything will "just work".

My psc-package.json is about as stock as it gets at this point and just contains a pile of packages from the [pureerl package set](https://github.com/purerl/package-sets).

    {
      "name": "untitled",
      "set": "erl-0.12.0-20180730",
      "source": "https://github.com/purerl/package-sets.git",
      "depends": [
        "console",
        "erl-atom",
        "erl-binary",
        "erl-lists",
        "erl-tuples",
        "erl-jsone",
        "maybe",
        "prelude"
      ]
    }

This is probably already too long for most people to read when sat on the loo, so I'll leave the writing of any code at all to the next entry..
