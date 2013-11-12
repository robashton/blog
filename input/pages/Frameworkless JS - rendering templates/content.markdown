Continuing then with the ["look no ma no frameworks"](https://github.com/robashton/look-ma-no-frameworks/) series...

- [Look ma no Frameworks](/entries/look-ma,-no-frameworks.html)
- [An example](/entries/frameworkless-js---an-example.html)
- [Getting started](/entries/starting-the-frameworkless-js-project.html)

Now, I'm [no real fan of pull-based templating systems](/entries/anti-templating-languages.html), but I lost the war on this a while ago and I'm not going to impose this one on this system today. (Remember, this was a walkthrough for a client with specific questions and not about me trying to impose my own opinions on a team I wasn't going to stay with)

So we picked a templating engine at random, and went with Mustache.. because no reason.

    npm install mustache --save


Great! There were celebrations in the street as we wrote the following code...

    var mustache = require('mustache')
      , domReady = require('domready')

    var template = "<p>Hello {{name}}</p>"

    domReady(function() {
      var container = document.getElementById('container')
      container.innerHtml = mustache.render(template, { name: "Bob" })
    })


*Waaait a minute, what is going on here - why have you just stuck stuff in a string that is cheating Rob Ashton how dare you.*


Guilty as charged, clearly this isn't going to scale well over time (although it's probably going to be better than building up strings of html using the "+" operator).


What we need here clearly is something that can give us a template from an external source and allow us to use it from there now if only such a thing existed.

The temptation is there to download these things as needed from the server - and in some cases this is certainly an option (although in those cases a server-side rendering approach might not be a bad idea either).

Instead, how about writing code like this?

    var mustache = require('mustache')
      , domReady = require('domready')
      , fs = require('fs')

    var template = fs.readFileSync(__dirname + "/myfunkytemplate.html")

    domReady(function() {
      var container = document.getElementById('container')
      container.innerHtml = mustache.render(template, { name: "Bob" })
    })


What on earth? What is this even? Magic? *fs* is a module you didn't see me install because normally this is a server-side module in node.js - and right now the example above does absolutely nothing.

You will recall our process for building the output file looked like this:

    browserify app.js -o public/app.js

Now, if only there was something smart enough to see that *readFileSync* call and replace that with inline content from our template file...

    npm install brfs --save

Boom, headshot. This is a transformer for browserify, something that can take the output of browserify and do something with it. If we use it like so

    browserify -t brfs app.js -o public/app.js

Then just like magic, the un-optimised output will look like this

    var mustache = require('mustache')
      , domReady = require('domready')

    var template = "<p>Hello {{name}}</p>"

    domReady(function() {
      var container = document.getElementById('container')
      container.innerHtml = mustache.render(template, { name: "Bob" })
    })

Which was just like the initial example where we started. Neat huh?

