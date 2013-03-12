**This post was written on Thursday**

It's day 4 out of 5 and [we pretty much have an end-to-end MVP](/entries/this-week,-lets-create-a-start-up---day-3.html) sorted out, (and a genuine surprise for me given the platform chosen!).


Today we decided we'd concentrate on some of the more fiddly aspects of the job, namely:

- Getting the workflow nailed for the desktop client
- Adding per-customer partitioning to the system
- Adding a HTML5 audio player to the public-facing system

*The desktop client*

Authentication proved to be a stumbling block, we're using forms auth across the site which is pretty unfriendly. I was pretty against creating a whole set of new endpoints just for a desktop client when we had perfectly good forms available for it already but I also didn't want to waste ages faffing with custom auth.

We had a look at creating a separate deployment for the desktop services using WebAPI but that would have meant doing a load of the infra work already achieved for ASP.NET MVC in WebAPI (yeah, they're kinda united and divided at hte same time). This would have been faffing, so we looked at...

Hosting WebAPI inside the ASP.NET MVC system and going for both Basic Auth and Forms Auth, we found a project on Github which federated these two with some custom providers - but that looked like it was going to be a rabbit hole too - and I'm against faffing so we looked at...

The original idea of using the original forms, and issuing tokens to the desktop client ala [last.fm](http://last.fm), this only took 20 minutes or so and proved to be the sane choice for rapid *moving on*

*Per customer partitioning*

We actually already had a conversation about this, do we do database-per-client or just field-on-the-document, this is a classic debate and it's not much different in the document database world.

We decided that database-per-client would have been a nightmare faff of session management (shared database as well) - not to mention fun-times if it came to a clouded instance of Raven so lumped our bets in with the database-field.

We looked at hooking the events exposed by the RavenDB client to add the field check to all queries, but unless we wanted to manipulate the final string query this was a dead end (and it didn't seem like there was an easy way to hook the load/save process either). 

This is either an omission on our parts, or RavenDB doesn't have those APIs yet, I really want those APIs, so I might add those APIs later, moving on however because we haven't got time for that...

I'm not particularly proud of what we did next, but it worked, everywhere the system asked for IDocumentSession, we changed it to ISecureDocumentSession and exposed the 5 methods on Session that we're actually using 

    - Load<Type>(string id)
    - Load<Type>(int id)
    - Load<Type>(string[] ids)
    - Query<Type>(string id)
    - Store(Object doc)

In each of these we check for typeof T === ISecured, and apply a check for the OrganisationId of the document.

This took all of 5 minutes, and was an effective use of time, perhaps to be re-visited in the future if it doesn't pan out.

*Complicated logic*

However, we now have quite a lot of complicated behaviour around 

- What organisations does the user have permission over
- What organisation is the user currently administering
- What organisation is being requested by the public facing system
- What organisation is being pushed to from the desktop client
- etc

Most of this logic exists in the infrastructure, and it needs pulling out and unit testing separately - just as a way of documenting the different functionalities exposed by these varients as changes are going to be very time consuing and error-prone otherwise.

This is an example of what [I was talking about in my testing post](/entries/uncle-bobs-viewpoint-considered-harmful.html) about diving in when things became too hard to juggle.

*The HTML5 Audio player*

"This'll be easy", I said, "We'll just download a widget" I said. We plumped for [jPlayer](http://www.jplayer.org/), which has a few dependencies including jQuery - so it'll have to be hosted inside an iframe so as not to annoy the site we're embedding the public data in.

It wasn't that easy though, first off - our data is in S3, and our public site is on somesubdomain.truthvine.com, cross site calls aren't really allowed...

Actually, they are - turns out that S3 has CORS support these days and on enabling that I was able to hear My Bloody Valentine blaring out of my speakers when visiting the player page in my Chromium install.

The same went for our IE10 instance and our IE9 instance but IE8...? Nope.

After faffing around trying to put the static files on S3 and going to-and-fro a tad, it turned out we'd just mis-typed the path to the SWF file (which will handle CORS for us in crappy browsers) and now we have a cross-browser and cross-platform audio player on the site.

This is the classic example of where faffing costs time, and what I was trying to avoid throughout the day with the other decisions we had.

Trying to build quick, and practising building quick gives you instincts on avoiding these and I'm glad that with the other challenges so far we've managed to avoid faffing. 

I guess browser compatability is always gonna be one of those things, but it only took an hour or so and leaving with all of the above achieved in a single day made us feel pretty good :)

Tomorrow time for some more features before I head back off to London and have a couple of days rest before heading to Greece for another coding adventure...




