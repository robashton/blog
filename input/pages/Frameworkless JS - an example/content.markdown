I was at a client last week helping with some [RavenDB](http://ravendb.net/) stuff and helping out with some of their JS while I was at it.

What I am going to do is walk through a bit of code I wrote with a couple of the developers from the client to explore a particular set of questions that they had, and demonstrate some of the concepts around frameworkless JS and why it doesn't mean "re-inventing the wheel" as such.

The repo is actually [here](http://github.com/robashton/look-ma-no-frameworks) if you want to skip my blathering and have a look without any context - but be aware that the code written is very focused on what questions were being asked by the developers at the client site.

I feel the conversation often gets too focused on the negative aspects of frameworks, rather than the positive experience to be had developing without one and for that reason I'm really glad to have a tangible example to walk through to demonstrate the fun to be had over at this side of the fence.

### What was I asked for?

The developers were working on a project which didn't have a lot of data-binding (but had some), and involved the coordination between several actors on page. They were wondering how I dealt with such scenarios and whether I would be open to using libraries like Knockout within a frameworkless context. They were also curious about how to handle multiple "pages/views" and how to re-use widgets/code across multiple applications and avoid building a monolith. 

### What did we build?

It's not much when you look at it, we demonstrate 

- How we can build feature/widget folders that could potentially become npm modules in their own right
- How to compose our application's functionality around the dom
- How we could manage event listeners when moving around the application
- How this approach doesn't necessary preclude us from using more bulky libs like Knockout
- How you can have multiple "pages" with back/forward navigationo
- Some rules to keep things sane as the application grows over time

A few different techniques were used across the code example for templating/content, this was simply to demonstrate that we had *options*, we'll discuss as part of this blog series what the standard approach might look like.

### The features

Again not much, we have a list of customers who belong to various banks, and we want to filter those customers by their bank as well as dive into the customer's details. There is no "write" functionality, although this would just be a matter of presenting the customer within a form and posting it so not a stretch to extend the example.

### 

I'll go over the code from the beginning and talk about the decisions/conversations had while doing this. It'll be fun.
