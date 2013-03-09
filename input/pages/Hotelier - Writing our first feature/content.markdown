
Now I've [got the RavenDBs everywhere](/entries/hotelier---setting-up-the-project.html), it is time to have a look at writing our first feature.

Let's start off with hotels, I want a new customer to be able to hit the front-page of the website and start creating their hotel with no registration or anything being required.

First off then, let's throw up the home page, I'm not going to bother with styling but I'll make sure to use a Razor Layout page so I can deal with that later (did somebody say Twitter Bootstrap?)

So that's

    View: /Views/Home/Index
    Controller: /Controllers/HomeController

This is my first compromise on behalf of ASP.NET MVC, I'd prefer to have my controller, view and any other associated information all grouped together in a folder called "Home" instead of this weird horizontal partition.

However, I know from past experience that while playing with this is possible, it's not a route I want to pursue if I want to get things done now. I can change this later when it gets *really* annoying and I've had the feedback from my client that the work I've seen is okay.

I'll also create

    View: /Views/Hotels/Create
    Controller: /Controllers/HotelsController

This is another compromise, I much prefer having controllers which aren't actually controllers ala NodeJS

    app.get('/hotel/create', function(req, res) { // controller code here }

And having the route as part of that set-up, separation of these concepts is clunky and weird and I don't like it. (Yes nancy boys, I know I can do this with your framework, not the point of this exercise)


Anyway, I'm gonna create a model for this in my app, which is to say


    public class Hotel { 
      public string Id { get; set; }
      public string Name { get; set; }
      public string Description { get; set; }
    }

I'm making a few assumptions ahead of time here and I've not written any tests, I've mapped this out in my head and the way it works is as follows:

- Customer hits front page
- Customer goes "get set up now!!" as he's excited by my exclamation marks
- Customer gets to create a hotel which looks like the above

You'll notice I use K+R style braces, I'm a convert and I've un-checked all boxes in Visual Studio that say "New line after.." - moar context on my screen yes please.

That's enough text for now, tomorrow I'll hook all these things together and write a test or two.

The code at this point can be found [here](https://github.com/robashton/hotelier/tree/235d96b0314c22ca81a9000ad628a2a7678c13e4).
