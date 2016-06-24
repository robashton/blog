I've been writing a bit of Elm lately for work as we have never really liked Javascript and with Elm 0.17.0 it looks like some sensible decisions have been made and it's going to be a viable replacement for our currect defacto choice of React + NPM.

Obviously I'm in favour of that, I'm a big fan of Haskell and Haskell type languages and the only reason I hadn't thrust down this direction already is my own reluctance to spend time learning an immature technology for application code that has generally been "good enough" (I'd prefer to spend the time on my guitar/coffee sorry not sorry).

Now I've written a couple of Elm applications (one of which will have gone into production by the time this post is published), I just want to make a few remarks about the 'state of things', knowing that I don't really have the full history of things regarding Elm and there may well be reasons behind some of the design decisions I'm having a hard time with.

This post is more of an overview of what things look like and what resources I used to learn Elm with, my other posts will deal with language and application structure separately so I don't just dump out a few thousand words that nobody will ever read.

The state of docs/samples
==

[0.17](https://elm-community.github.io/elm-faq/17.html) only came out fairly recently and most of the bigger examples available were written before this. This means that a lot of the sample code use concepts that don't exist any more ([Signals](http://elm-lang.org/blog/farewell-to-frp)) and there has been very little written of any consequence in the New Way. This means that composing larger applications is a bit of a stab in the dark and we've had to guess our way around this.

There are examples written by [evancz](https://github.com/evancz/) in 0.17, but like any other examples I can find they are limited to using a single top level model and command structure and don't really give any indication as to what something larger would look like. It's really good reading that the "Commands and Subscriptions help nest components infinitely" but there isn't really any indication as to what that actually looks like in the wild.

That said, the docs around the language itself and ["The Elm Architecture"](http://guide.elm-lang.org/architecture/) are very good and a lot more complete than a lot of "small" projects like this would have usually been. The absolute focus on "user oriented design" really shows itself in that Elm is doing its best to use simple language and hide anything that isn't data or 'plain old code' from the developer.

Learning from others
==

While I have not directly interacted with any community members, I have joined the [Slack](http://elmlang.slack.com) and browsed the [mailing list](https://groups.google.com/forum/#!forum/elm-discuss) in an attempt to find answers to the questions I have had and that other people have similarly had.

I get the feeling that most people doing Elm are either writing fun little games (It looks *excellent* for this, or they've not really attempted to write anything substantial in 0.17 yet. There are a lot of questions about composition and events and no real answers other than "it's bit awkward". It feels a bit head-in-sand at the moment and one of the reasons I'm doing these posts is to get some feedback over the way I've ended up doing things.

The people *in* the mailing list and Slack seem to of the resoundingly friendly variety and the people approaching the language appear to be in the camp of it being their first functional language and there are no shortage of people ready to answer their questions in the Slack channels.  Searching through the history of the Slack and mailing list answered most of my questions about 0.17 or at least left me at a level of understanding that there isn't an answer yet and I'd better just get on and do it my way for now.


My overall impression
==

Elm is in a place where I'm happily going to use it for most of our front-end work from now on; the up-front time putting together the types and calls to get data is easily regained when building the actual application because compilation errors massively outweigh the runtime errors once the data is in place. 

I'll probably do some more posts beyond these "notes" as I build larger things and have anything worth sharing; for now Elm seems to be a sensible option for those building applications for web browsers.
