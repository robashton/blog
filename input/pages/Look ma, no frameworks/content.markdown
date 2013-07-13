Having a break from the FP series, I've made a few statements in the past and had more than a few discussions with people about how I build JS applications and the most common question I get asked is

  <blockquote>How on earth can you build JS applications without a framework?</blockquote>

My favourite quote on this subject so far is...

  <blockquote>Typical cowboy bullshit</blockquote>

Which came from the [Reddit thread](http://www.reddit.com/r/programming/comments/1hqmyj/working_for_free_and_what_it_taught_me/cawyy87) about [Working for free](/entries/working-for-free-and-what-it-taught-me.html), and is a a typical reaction when I'm waving my arms around about how awesome not using frameworks is without really backing it up with info.

So, here's the deal, I'm going to go through a few of the libraries, patterns and techniques that I use over the next few entries to show my workflow/thinking/etc around this to supplement some of this arm waving with useful information.

# First up, the arm-waving - why?

*I don't like being told what to do*

I admit it, I'm a bit of an anti-authority type. If I'm told to do something by somebody and they've not yet earned my respect, I'm probably going to be having words. Admittedly this has gotten me into trouble in the past but hey ho... 

The same goes for frameworks, I hate being told what to do and where to put things from the get-go. Without discovering reasons for those decisions in the applications I'm building. I'm not saying I don't re-use patterns and ways of doing things that have served me well in previous applications (this is what this series is going to be about after all). 

The thing is that over time I've come to the conclusion that a lot of the ways we do things in frameworks are *wrong* anyway.

That brings me onto...

*I don't like forced abstractions*

Data binding? Logicless Templating? MVVM? MVC? Not my cup of tea most of the time, forcing your application into layers either on the server or the client before we even know what our requirements are takes away too much control and leaves little room for the more natural abstractions that a more vanilla approach will provide.

It's hubris for a framework to suggest that it will somehow magically enable us to write "Application Scale JavaScript" (whatever this means). It is dangerous to think that by separating our application up in horizontal slices somehow yields in better, more manageable code.

Abstractions should be used because there is a pain that needs solving, whether that be because you're talking to third party code, or slow remote calls that need hiding during testing or because there is complexity that needs hiding. Putting abstractions in before we feel any of this pain just means more code to wade through when trying to get stuff done - no thanks.

*I like power*

If I want to bulldoze my way through a feature, then I want to bulldoze my way through a feature. If this means taking a shotgun to the DOM then that's what I want to do. If it means abusing cookies/local storage for nefarious purposes then that's what I want to do.  If there is a module/library around that can help me I want to pull it in and *use* it.

The power is mine to do these things. As soon as a framework starts imposing its will on me, this power often goes away and more importantly...

*I hate fighting*

I keep saying this - that Frameworks basically mean having to fight edge cases instead of actually writing feature code. 

I'm not saying that you can't be faster if all you do is glue things together, but spending hours reading StackOverflow to find out how to insert Knob A into Hole B is not my idea of fun at all. As much as I value getting things done - I start getting pretty miserable when that tiny little feature I wanted to develop turns into hours of trying to fit incompatible abstractions together.

*Focus*

The problem with frameworks is not that they don't solve problems beacuse for some people they do. My problem with them is generally that they try to solve more than one problem and because of this there is often a conflict between what I want to get done application development vs what is provided in the framework. 

Focus is the key here, when I'm pulling in a library that does something, I want to know that I can use it without its implementation getting in the way of the other things I want to do.

*I want control over my entry points*

I want an app.js which contains the code that sets up my application. (Frameworks don't necessarily prevent you doing this, Backbone is a good example). I want to be responsible for creating new objects/services and control their lifetimes from the outside in.

Doing so means that isolating code I want to test is trivial, that everything remains explicit and readable from that point in, and at any point it's possible to change how the application works without fighting configuration or convention. I long the days when either we're not writing code because we've hit the singularity or we've moved past this notion of "Drop some files in these arbitrary folders and honest we'll wire everything up for you"

# Onwards then...

I don't know where I'm going with this, but I'll try to start from the beginning and take it from there.



