Putting my keyboard where my... er keyboard is, I'm going to build an application until I get bored and I'm going to do it on top of the ASP.NET MVC stack using my common patterns and [personal brand of pragmatism](/entries/uncle-bobs-viewpoint-considered-harmful.html).

Why ASP.NET MVC? Because like it or lump it, it's what most companies expect you to be building .NET web apps with if you're in that space, and while it might be annoying to a small subset of people because using it is like forced to undergo death by a thousand cuts, it is possible to build things in it if you leave your opinions at the door, as seen in my week [helping Sam build his start-up with it.](/entries/this-week,-lets-create-a-start-up---day-5.html).

I was asked on Twitter by a few people what the compromises I felt I had made were, and I found it hard to articulate after the fact, I'm hoping by doing this I can document them as I go and and get feedback on my style as we do this.

So, the domain
----------------

I want to build a web app which allows small hotel owners to define their hotel, define what they have available, and then embed a booking form into their static website so that they don't have to be on the phone all the time or miss out of bookings.

I could do this commercially, but a) It has been done before b) There isn't a huge return on this sort of thing for the effort, so I may as well do it open source.


The technology
-----------------


I'll be using RavenDB for persistence, and Zombify for my functional testing (I'll cover the reasons for these choices in the next couple of blog entries)

I've made those decisions up front because this is the kind of project where I feel I can do that, it's going to be a rapid development project where I try to put the thing together quickly, and outline what I feel to be steps I've skipped in order to go faster and what their repercussions will be.





