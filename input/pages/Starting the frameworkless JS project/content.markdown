- [Look ma no Frameworks](/entries/look-ma,-no-frameworks.html)
- [An example](/entries/frameworkless-js---an-example.html)


# So you have an empty folder

*Set up the initial project structure*

    npm init
    git init
    echo "node_modules" > .gitignore

    mkdir public
    touch public/index.html
    touch app.js

    git commit -am "Initial commit"
    
*Install a standalone http server to serve our static files*

    npm install -g http-server
    cd public
    http-server

    (server now listening on http://localhost:8080)

*Install browserify so we can build our application*

    npm install -g browserify

*Write our first application*

    echo "console.log('hello world')" > app.js

*Build our application*

    browserify app.js -o public/app.js

*This will create a wonderful file which can be loaded by our index.html*

    <script type="text/javascript" src="app.js"></script>

*The output actually looks like this:*

    ;(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
    console.log('hello world')

    },{}]},{},[1])
    ;

*This is a bit useless for debugging in the browser, so instead we pass the 'd' option to browserify and it will generate source maps so in the browser can debug our original file which looks like this*

    console.log('hello world')

*To generate the source maps, that's*
  
    browserify -d app.js -o public/app.js

# Installing our first module

I want to set the content of a div to "Hello world", and in order to do this for the sake of argument I want to wait for the dom to be loaded. 

In jQuery this would be the equivalent of the 

    $(function() { // do stuff here })

The equivalent to this that I've found in NPM is a module called "domready", so I install it with

    npm install domready --save

Save modifies my package.json to contain the module I've installed, so other developers can install it on cloning this repo. Now in my app.js I can write some code to use this

    var domReady = require('domready')
    domReady(function() {
      var container = document.getElementById('container')
      container.innerHTML = "<p>Hello world</p>"
    })

Building this with

    browserify -d app.js -o public/app.js

Gives me the desired results of seeing "Hello world on the page"


# Recap

All we've done is

- Installed some bits on our machine
- Written an app.js that uses a module
- Ran a command to build that into the static file directory on my site

Next we'll look at an option for doing some templating...
