I've been asked a few times why I've stopped using AMD as I move around and folk see my use of Browserify, here-in I try to write down my thoughts on why AMD doesn't make any sense to me any more.

-----

When I wrote a blog entry [about 9 months ago](http://codebetter.com/robashton/2012/09/03/keeping-js-sane/), I made the statement:

  <blockquote>
    The synchronous manner in which files are included in CommonJS-ish systems doesn’t lend itself to the web very well.
  </blockquote>

I was wrong. I changed my mind, and as I've said before I am now a [Browserify Convert](/entries/lots-of-small-things.html).

What I didn't explain was what turned me off [AMD](https://github.com/amdjs/amdjs-api/wiki/AMD) and in particular, [RequireJS](http://requirejs.org/).

**The list of things I want my module loader to help me with**

- I don't want to write all my code in a single file
- I want to write my code across many files
- I want to write my code across many modules
- I want to be able to easily debug these once in the browser (it's unusual I need to debug, but when I do...)
- I want the time between editing a file and seeing feedback in the application to be -- >< -- this big

In other words, yes - I do want the moon on a stick.


**RequireJS**

Seemed to help with these things, I didn't use the hideous ceremony-ridden version of AMD that looks like somebody vomitted in my Javascripts (that alone would have been enough to put me off) 

*The ceremonial way*

    define([
      '../foo', 
      './lib/bar', 
      'boo'], 
      function(foo, bar, boo) {

    })

*The less-ceremony way*

    define(function(require) {
      var foo = require('../foo')
        , bar = require('./lib/bar')
        , boo = require('boo')
    })

So it was tolerable for a while because I didn't have to, however I would then run into the following road-blocks:

- What about Library X, does it support AMD?
- If it doesn't support AMD, I have to add a shim? Should I patch the library?
- What if it supports AMD, I'll want to load it from a libs directory
- Where is the root of my application? What about sharing code client/server
- What if I want to use CoffeeScript (or more recently for me, OMeta)

**For every question, RequireJS had an answer**

And AMD's answer for *nearly every one of them* is to *add some configuration directives* or *write a r.js plug-in*

And this is what put me off.

In no time at all, every project would have a configuration file many lines long of obscure directives (and I mean obscure directives, have you [read the documentation?](http://requirejs.org/docs/api.html) - this is not intuitive and requires a hella lot of investment if we are to make effective use of it.

Then we'd want to write tests against the code, and we'd have to either attempt to re-use this configuration or duplicate it over into our tests directory. 
Then we'd run into problems with that configuration, and waste hours trying to work out a compromise to keep RequireJS happy.

**Most of those questions were the wrong question**

First off, using [actual modules](/entries/stop-using-relative-paths-in-your-javascripts.html) has massive advantages over relative paths anyway, and while the build step of RequireJS will support these, this isn't going to work if we're using the A of AMD in development.

Secondly, if we want to compile our .coffee to JS, we should be using the coffeescript compiler to do this. If we want to compile our Typescript to JS likewise, if we want to import static files for templates then this should be part of our build process.

Trying to do everything as a long sequence of plug-ins meant to support the A/MD actually makes things a lot *slower* in my experience, because we're not trying to do it only when specific files change but instead trying to do it as part of the request pipeline (unless we go to lengths to work around the r.js plug-in system)

**The future of JS modules is not async anyway**

If yo look at the specifications and where they're going, (while last I checked they weren't perfect yet), the module system that is coming for JS isn't going to be asynchronous - and this is because our program *can't run until it's loaded anyway* - it actually makes little sense to add the overhead of asynchronous management to this process.

Building up an entire codebase around tooling that isn't compatible with how the future-web is going to work doesn't make an awful lot of sense, building up an entire tool-chain around this tooling makes even less sense.

**I switched to Browserify**

As I've said, I'm not particularly sold on using node\_modules for  client-side code, but the module system in node *does* work, and *does* encourage us to package things up in a neat re-usable manner (I'm not sold on component.js yet either).

However, the code we write doesn't care what module system is being used if we're just doing CommonJS - it just knows that they come from *somewhere*. The tools we use to convert CS into JS don't care that we're using Browserify to package the end-result. The tools we use to embed templates in the downloadables don't care that we're using Browserify to do this. 

In a nut-shell, Browserify is allowing me to hedge my bets by not coupling my workflow too closely to it. It doesn't come with pages of obsuse documentation and every time I pump out a new module into my little eco-system I am ever so thankful for this.

It has enabled me to be liberal with my module creation and not care how people are going to actually consume these packages (apart from 'through npm' somehow), and I've not had to debug or diagnose issues with it in the whole time I've been using it.

Let's look at the list of things I want:

- I don't want to write all my code in a single file
- I want to write my code across many files
- I want to write my code across many modules
- I want to be able to easily debug these once in the browser (it's unusual I need to debug, but when I do...)
- I want the time between editing a file and seeing feedback in the application to be -- >< -- this big

I'm able to do all of these - yes I need a build step now, but you know what? We need a build-step anyway if we're going to take advantage of the module system properly - even in RequireJS so this isn't a big deal. 

We get debugging support through source maps (and this even tells us which module we're debugging) and as I said yesterday, I no longer really [use relative paths](/entries/stop-using-relative-paths-in-your-javascripts.html) so I'm a great deal happier about my JS.

Happiness and productivity, good reasons for doing most things really.
