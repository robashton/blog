We are in a bit of a pickle. We have a little bit of code which does things to the dom and has interactivity, but how do we organise that so we don't just have a pile of code in a file?

- [Look ma no Frameworks](/entries/look-ma,-no-frameworks.html)
- [An example](/entries/frameworkless-js---an-example.html)
- [Getting started](/entries/starting-the-frameworkless-js-project.html)
- [Templating stuff](/entries/frameworkless-js---rendering-templates.html)
- [Demanding Knockout](/entries/frameworkless-js---but-we-*really*-want-to-use-knockout-for-that-bit.html)
- [Encapsulation of views](/entries/frameworkless-js---encapsulation-of-views.html)

Well, a few of you are wanting me to *get to the point already* and I'mma let you finish but first I'mma tell you that jQuery is the greatest library of all time, ALL TIME.

### jQuery

In the beginning there were a pile of JS files included into a pile of html files and executed in whatever order we felt like including them. If we were lucky there might be a common.js or a utils.js to wrap up various inconsistencies between browsers and then everything else just used those to hijack bits of the dom and do all manners of wicked things in our application.

```javascript
my.org.has.a.bigger.namespace.than.you.utils.getElementById("foo")
```

Then jQuery was born and we suddenly had a magical dollar symbol which gave us a way to use the dom without going through all those headaches. We saw that it was good and there were joyous celebrations across the land as the word spread and jQuery allowed us to write more spaghetti code and even faster than ever before.

```javascript
$('#foo')
```

And lo, for the jQuery team did see that perhaps not everything should be part of jQuery itself and they created a plug-in system whereby you could attach functions to the almighty dollar symbol and a sprawling ecosystem was born whereby everybody and their dogs were writing jQuery plugins to perform magic on web pages across the intarwebs.

```javascript
$('#foo').magic()
```

There was no standard. There was no dependency management. There were very few rules. We had jQuery plug-ins to handle touch events on a dom element, to convert a list into a tabbed UI, to create image sliders on your marketing homepage, to create and track various animations. The only commonality was that 

- You included jQuery
- You then included jQuery plug-ins
- You then included your app which then used those plug-ins

Some jQuery plugins would let you hook events through their configuration set-up

```javascript
$('#foo').setupWidget({
  onActivated: function() {
  },
  onDeactivated: function() {

  },
  onMagic: function() {

  }
})
```

Some jQuery plug-ins would simply raise their events through the element itself once activated:


```javascript
$('#foo').widget().on('activated', function() {})
```

As a bonus to that lack of dependency management, jQuery plug-ins tended to be very standalone (in as much as they could be given their dependency on jQuery itself!), and they'd commonly be pretty single purpose (do one thing and do it well...). 

These plug-ins could

- Render a whole tree of HTML under the element(s) you selected
- Attach CSS to that HTML
- Expose semantic events over the top of that element

```javascript
$('ul.tabs').tabs() // my favourite
```

This sounds a lot like the desired encapsulation I've been talking about in the last couple of entries.

### Pushing it to its limit

And indeed, on a couple of projects I saw this pushed as far as it possibly could. The guidance being *If you like it then stick it in a plug-in*. The entire app therefore became a collection of jQuery plug-ins whose use was orchestrated by the entry point (app.js). Each widget would be responsible for managing the element it was given, all of the HTML below it and for pushing events back up which meant something in the app.

Each plug-in would have its own repository, its own documentation which was usually a single page, as more than that would mean the plug-in probably did too much. Using them meant you'd RTFM for that plug-in to see how to make it work its magic. This experience was to a lesser extent mirrored in the general ecosystem outside too - for a couple of years an approach to client-side web development would mean reaching automatically to see *is there a jQuery plug-in that does that?*

### This was pretty neat

Does it sound familiar? We managed to build some pretty big apps this way, the downsides being

- No dependency management meant either documenting *spit* dependencies or not having any at all
- No explicit path for knowing "where code came from"
- Callstacks would often involve jQuery which was just fubarred

*What's this got to do with view encapsulation in js using npm modules?*

I probably don't need to answer this question, but in essence I'd say quite a lot.  If instead of jQuery plug-ins we think about modules in commonjs (where a module is either a package in NPM or just a single file included via "require"), and for documentation we think readme files on Github then we're almost there.

If we want a bit of functionality and our approach is to *add more code to our application's main file* then we've already lost. If our approach is to write a standalone module which exposes a single documented function and use those to compose our application with then we've probably got more legs.

As a bonus over those jQuery times (which looking back actually weren't *that* great), we get

- dependency management meaning we can potentially create even smaller modules(!)
- an explicit dependency resolution path, code doesn't get invoked unless you invoke it
- Sensible callstacks that exist entirely in "ordinary" code

### Next steps

Now that we've had a brief history lesson, we'll look at emulating this inside an npm/browserify style application (for better or for worse).




