The very first thing I usually want to do with a project, is to do something with the dom.

  - [Look ma, no frameworks](/entries/look-ma,-no-frameworks.html)
  - [Grunt+browserify+npm+application=success](/entries/grunt+browserify+npm+application=success.html)

*What is jQuery?*

jQuery isn't a framework, it doesn't take over your entry point, doesn't force any sort of structure on you and doesn't have any of the other smells associated with frameworks in general; it is a general purpose glue library.

Most people will by default pull it down and start using it on their page without giving it a second thought, most people shove the majority their code into the jQuery "Dom ready" callback, use the selectors to find their elements and start doing things without worrying about what is going on under the hood. 

A lot of projects will also do this *all over the place* without thinking about it because it's so easy, and that's part of my problem *with* jQuery, it makes it too easy for developers to just zombie along without thinking about what we're doing.

*jQuery - it's not you, it's me*

It's not that I don't like jQuery, it solved many problems back in the day before JS was cool, and probably contributed in part to the rise of JS as the weapon of choice for most dev work.

It's just, it solves *many* problems and has gotten quite confused over the years, [look at how many ways we've ended up with to listen to events](http://api.jquery.com/category/events/event-handler-attachment/) for example. 

A lot of these complaints are being dealt with as the project moves forward slowly but that's half of my conflict with it. The size and scope of the project and the number of people using it means that any change or improvement has to come gradually. The speed at which with small libraries I can either fork/patch/modify is pretty cool.

*Every line of code should be justified*

When you pull in a large library like jQuery, you're pulling in hundreds of methods to do *stuff*, and for me personally that's a bad way of working. In my perfect world, every method should have a reason to exist in my application.

In short, jQuery is a 100kb swiss army knife and it's not my personal preference to work with such tools.

*A tangible example*

Okay, so first things first - how do I wait for the dom to be ready so I can do stuff with it?

    <html>
      <body>
        <div id="content"></div>
        <script src="type="text/javascript">
          // Do stuff with "content"
        </script>
      </body>

*Trololol*, actually, you could read/write about this subject until the cows come home and enough people have, but I just wanted to make the point that sometimes you don't have to write any code and thinking about stuff before doing:

    $(function() {
      // My Application
    })

might hold some benefits.

# Avoiding jQuery

Anyway, waiting for the DOM to be ready is still [potentially complicated ](http://stackoverflow.com/questions/6902280/cross-browser-dom-ready) and managing this stuff ourselves doesn't seem very worthwhile.

# NPM to the rescue

```bash
npm install domready --save-dev
vim client/app.js
```

```javascript
var domready = require('domready')

domready(function() {
  // Application code goes here
})

```

What happened here, is I decided for the entry point to my application I needed a cross browser dom ready function, so I pulled one down from NPM and used it.

This is the preferred mode of working in this frameworkless environment.

- Start writing a feature
- Realise we need code that isn't directly related to that feature
- Find a library that does that specific thing
- Install it
- Carry on

# It's not re-inventing the wheel

In the last blog entry, I was accused of ["re-inventing the wheel"](/entries/look-ma,-no-frameworks.html). This is as about as far from the truth as it could be - the best thing we can ever do when writing a feature, is to avoid writing code.

However, building our feature on top of something that doesn't directly solve a problem we're facing is where the conflict lies. By limiting ourselves to single shot libraries such as domReady we should be able to avoid a lot of the disconnect we face when pulling in larger libraries or frameworks.

# Discovering these small libraries in the first place

*First, try searching on [npmjs.org](https://search.npmjs.org/)*

If we have a specific keyword to look for, this is usually good enough.  Searching for *domready* on this page will give us a collection of libraries to choose from.

*Next, try [google.com](http://google.com)*

Google is great at fuzzy searches - and generally if you include "NPM" in the search we'll get better results than using the NPM search itself if we're not too sure what we're looking for.

# Choosing the library

There are a dozen domready libraries (and for most simple problems this is the case) - this is *great*, we just need to be able to pick which one to use. I like to ask the following questions

- How many libraries are using this one? (More is mostly better)
- How many libraries does it consume? (Less is mostly better)
- Does the documentation fit in a single README?
- Is there a link to Github?
- How many people watch  the project on Github?
- Is there a lot of code churn? (I don't want to see a lot of activity on the Github repo)
- Are there many outstanding issues?

This might seem like a lot of work, but making that third party code justify itself in this way means that it'll be right for the project and not just some ad-hoc file in a directory somewhere.

# Next steps

Next up, we'll have a look at some of the patterns I use when building a dom-intensive JS application.




