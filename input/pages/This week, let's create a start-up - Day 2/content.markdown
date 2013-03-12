**This post was written on Tuesday**

Yesterday [I pretty much put together the admin side of the project](/entries/this-week,-lets-create-a-start-up---day-1.html), and today I decided to focus on the other side of the project - dumping content into a third party site as if it was on the site itelf.

Carrying on from yesterday, this was actually pretty simple

- Create a JS file for inclusion on the third party site
- When imported, it looks for !#/this-stuff-after-the-hash-bang
- It then loads the appropriate content from the main truthvine site based on the path after the hashbang

We have to use a hashbang because we can't rely on the site we're being embedded on to be able to do server-side redirects but we want back-forward buttons to work.

What we ended up doing was splitting up the system so we have a solution that looks like this:

- TruthVineAdmin (ASP.NET MVC)
- TruthVinePublic (ASP.NET MVC)
- TruthVine (RavenDB/Infrastructure)
- TestThirdPartyWebsite (Static files only with the script tag in them)

I'd normally not like to have a "common" assembly in a solution as the tendency of developers is to shuffle lots of needless 'shared' code to this place (where they'd be better off writing the code separately in the web projects), but I trust that Sam won't do this and the only things that go in this shared assembly are:

- RavenDB models
- Common ASP.NET MVC infrastructure (session-per-request)
- The paging code

There is little point trying to share view models or play around trying to re-use views and hide and show admin functionality on content pages (that stuff is always horrible to do unless you have time to build up some decent conventions), so this separation makes sense.

As for my happiness rating with ASP.NET MVC today, well - it stayed out of my way because I did everything the way it wanted me to - because of this I ended up building the third party JS content-embedding system to the point where it had pretty much reached feature parity with the system we were basing this off in the first place. (Hurrah)

I'm pretty much doing things the way I've [Described previously on CodeBetter](http://codebetter.com/robashton/2011/06/13/finding-a-balance-with-asp-net-mvc/) without any of the feature-based grouping (haven't got time to set it up)

Controller actions all pretty much look like this across the site

    [HttpGet]
    public ActionResult Edit(int id) {
       var doc = this.Documents().Load<ThatDocument>(id)
       if(doc == null) return new Error404Result()
       return View(new EditViewModel(doc))
    }

    [HttpPost]
    public ActionResult Edit(EditViewModel input) {
       var doc = this.Documents().Load<ThatDocument>(model.Id)
       if(doc == null) return new Error404Result()
       
       if(ModelState.IsValid) {
        input.SaveTo(doc);
        return RedirectToAction("View", new { Id = model.Id })
       }
       input.CopyFrom(doc);
       return View(input);
    }

Straight down to earth and simple. Coupled with all of those helper methods and Razor it's pretty easy to throw up new forms and use redirects to create a workflow over the site.

My happiness rating with RavenDB is as ever, it stays out the way and handled persistence for me - lovely.

With a working admin portal and a working content embedding system, that's pretty much the end-to-end product written in two days, leaving us three days to mop up the rest of the tasks. 

The good thing about all the work so far is that the domain has been very well understood and communicated (thanks to the previous incarnation of the system) so we've not had to spend too long discussing or debating things, I've been showing the work to Sam on a feature by feature basis to get feedback (he is my customer after all) and adopting his feedback every hour or so. 

Tomorrow we'll look at getting these three sites deployed on EC2, and throwing the audio files up to S3, because that'll put us in a good place.
