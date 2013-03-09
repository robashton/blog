[Zombify](http://github.com/robashton/zombify)? What is that?

Okay I'll admit, this is one of my projects that I've used in a few projects quite successfully (the only limitation being Zombie itself in heavy JS scenarios)


- Zombify is a bundle in NuGet
- Zombify bundles Zombie - a headless browser written in JS
- Zombify bundles XSP, Mono's server which can be torn up/down easily
- Zombify enables RPC for whitebox testing between JS and .NET
- Zombify encourages functional tests to be written in JS/Coffeescript against the application output


That's Zombify in a nutshell, it has been on [NuGet](http://nuget.org/packages/Zombify) for a few months now and people are already using it without me talking about it at all (honest)


    Install-Package Zombify


It's also in NPM if you're already a NodeJS user and don't want to faff with NuGet (This is how I choose to use it)


    npm install zombify


Anyway, I'll cover how we start using this in my next blog entry, setting up the new and empty project!

**Why Coffeescript?**

I like [Coffeescript](http://coffeescript.org/) for writing functional tests along with [Mocha](http://visionmedia.github.com/mocha/) because it's incredibly readable and thus possesses some of the qualities of using [Cucumber](http://cukes.info/) for this purpose (in that your boss can usually read both the tests and the output) but without the disadvantages of having that horrible extra layer between your tests and what is actually going on.

I was a sceptic to begin with, but I changed my mind once I got going with it and this is how I do most of my functional testing these days.

**Why Zombie**

[Zombie](http://zombie.labnotes.org/) is *fast*, and when you're writing an application from a classic HTML/Forms perspective you haven't got to worry too much about the extreme JS edge cases it isn't going to work for (at which point Phantom becomes more useful).

Yes, I start with HTML and forms, if I want to do any single page stuff off the back of this I'll be consuming that HTML and those forms as my API - which means I'll have an app that work without JS and with JS and on mobile phones etc. This is how I like to build apps even today in this world of JS everywhere.


**Why Whitebox testing?**

There are some things you don't want to expose by conventional means to the outside world, but are useful during testing - an example of this is when I was working on one of my last games, I wrote RPC calls so I could tell the backend "This round is now over" without waiting for the timer to run down. 


**Why JS for automation**

Having JS means you're working with the actual DOM in the language built for it. There is no faffing with trying to manipulate or build up abstractions around your HTML/JS built UI in a language not built for it.

The advantage of using a headless browser written in JS is that you have direct access to the event loop and know *deterministically* when things have finished - you don't get timing issues in your UI tests and that is really important to me and anybody who has had to suffer their way through UI driven tests before.


