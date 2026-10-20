I pulled the [EventStore](http://geteventstore.com/) out a while ago to play with, with the intention of making a cool Github InfoGraph type thing, it never quite materialised thanks to the rate limits imposed by Github, and then other stuff came up (like I've got a game engine I'm working on and I want to blog about!)

That said, I had some downtime this week in between engagements and decided to bring it up again and blog about some of the things I did with it and some of the questions we're able to ask with the projections feature (which is hitting a point of maturity now which it didn't have before).

Anyway, setting up the EventStore on my Debian install sorta looked like this

- Do a fresh build of Mono 3.0.5 (it won't work on the 2.10 that ships out of the box)
- git clone the event store
- checkout the projections branch (not out yet)
- xbuild EventStore.sln
- Build the v8 stuff
- Set up LD_LIBRARY_PATH so the v8 stuff can be loaded from the .NET exe

Coolio, so we'll assume that this is running throughout my little experiment and that I can access it on http://127.0.0.1:2113

Browsing to that URL, we can see a veritable playground of shinies, the most important for me are

- See All Streams
- Projections

Popping into the "See all Streams", we can see a big pile of json, what's cool about this is 

- It's a pile of links
- The links tell us what types are accepted
- I can request this in various types as well (see the ?format=json) in this URL

Basically, it's AtomPub, and AtomPub is used across the EventStore for interactions - which means no faffing around with custom formats or any of the crap associated with a lot of proprietary systems.

Anwyay, I haven't currently got any info in my event store so I guess I'll look at that next...
