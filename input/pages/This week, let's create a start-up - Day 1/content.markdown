I've finished my [first day](/entries/this-week,-lets-create-a-start-up.html) of the build a start-up in a week challenge, and now I know a little more about the project.

- Sam has already built a codebase that his local church uses to put sermons online
- His target audience is non-technical churches that have managed to get a website up there (even if it's just static code)
- He wants to be able to embed a single line of code in one of their pages and give them all the functionality
- The functionality isn't too complex, but it has to be dead easy

Okay, so what the heck are sermons and what's the deal about getting them online

- Churches have sermons
- A sermon is just an audio file (encoded to a variety of formats)
- Each sermon can belong to a series of sermons (perhaps on a topic of some sort)
- Sermons have meta data associated with them, as do the series
- Each church needs their own subset of this data
- Each church can have multiple users who can add sermons and series

Not that complicated, and my first suggestion is

*Can't we just use Wordpress/Drupal/Etc and have this done in a couple of hours?*

If I was building an MVP, that's pretty much what I'd do (and I still maintain that's what we should do), but Sam is pretty insistent that he wants it done in ASP.NET MVC (*shudder*) and RavenDB (*yay*), so I guess I need to get out my Windows laptop and once more work my way around Microsoft's attempt at an MVC framework.

Anyway, after trying to create a few "empty sites" in Visual Studio, I finally find a configuration that is "emptier" than the other ones and get to work, this is my first time on ASP.NET MVC since last I did .NET web development (Back in version 2 and beginning of 3)

Turns out it's not only "mediocre" these days, but I'm actually able to get most of the basic CRUD operations and workflow done in a few hours, highlights of this experience being:

- The Razor View Engine
- Html.EditorForModel
- Html.EditorFor
- Data Validation Attributes for the ViewModels (sorry guys, but they work quite well)
- Global action filters

The default model binding seems to work out of the box for everything, and RavenDB is being managed via global action filters so I only have to do

    this.Documents()

In any controller to get the document session (yay for extension methods), no I'm not bothering wiring up a container, the only objects in play are the input/view models and RavenDB and the state model that is being persisted in it, and it's unlikely to get much more complicated than that (so end-to-end tests will suffice with an in-memory RavenDB for now)

I even wrote a little bit of magic to do paging in a standard way across any view that needs them in RavenDB

    pagedResults = session.Query<Whatever>()
           .Where(x=> SomeCondition(x))
           .PerformPaging(inputModel)

Yay again for extension methods.

I also set up OAuth - I used DotNetOpenAuth, which worked once I'd written a pile of code in a controller action - it's *much* better than the previous incarnations I used last time I did .NET, but it's still not quite as good as say, passport in NodeJS (and I'll hazard a guess it doesn't quite meet the standards of whatever RoR provides for this either).

I guess that's because with node, we usually have control over the entry point and everything can be done with connect middleware in a standardised fashion, whereas ASP.NET MVC is an opinionated framework which doesn't know what its opinions are and still suffers from a sad-mix of xml configuration and confusion, still - I guess once you know about this stuff you can copy and paste these bits of infrastructure around so it's not too awful.

Anyway, today I achieved with ASP.NET MVC and RavenDB

- All the basic CRUD forms + workflow around sermons and series (about 10 dynamic pages in all)
- Basic paging/filtering controls for anything that needs them
- Audio upload (alhough not going to S3 yet)
- Authentication with credentials
- Authentication via OAuth
- Theming using the bootstrap theme Sam has already provided

Tomorrow I'll hook up the script to embed content in third party websites, and get the MVP finished - that'll leave three days to do all the value-adds, I'm fairly happy with this progress and feel that *this* is still how we build software, even if it's not in the technology I'd have chosen.
