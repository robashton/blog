I've made a [Github repo](http://github.com/robashton/hotelier), into which I've placed the standard .gitignore for .NET and etc.

I'm using Visual Studio 2010, because I haven't got Visual Studio 2012 and it's unlikely I'm going to get it as most of my personal dev these days is done in vim and most of my professional dev is done on client machines.

I've cloned that to my local drive and next up imma create an empty ASP.NET MVC4 application using Razor views.

With that done, I'm going to use the version of RavenDB from the unstable builds because there is functionality in there that I want (I wrote it while I was in Israel at Hibernating Rhinos) and by the time most people read this that'll be old news. (Judging by past performance in Google Analytics)

I'll install Zombify via NuGet into an empty class library project which is part of the solution.

Thus, things now look like this:


<img src="/img/empty-hotelier.png">


You'll notice that Zombify has made some basic set-up stuffs for us which is nice of it, we'll ignore that for now though because we have other fish to fry.

I've added RavenDB to a naughty \_libs folder, and added it as references and that's me good to go.


**Hooking up RavenDB per request**


Well, I'm going to be a rebel and not bother with an IOC container, it's a big faff, every one of my controllers is going to need the IDocumentSession to begin with and I'm not planning on writing unit tests for my controllers anyway (they can instantiate stuff just fine thank-you).

Instead, this is what I'll do:


**RavenDBFilterAttribute**


    public class RavenPerRequestAttribute : ActionFilterAttribute
    {
        public override void OnActionExecuting(ActionExecutingContext filterContext)
        {
            if (filterContext.IsChildAction)
                return;
            PerRequestRavenControllerExtensions.BeginAction();
        }

        public override void OnResultExecuted(ResultExecutedContext filterContext)
        {
            if (filterContext.IsChildAction)
                return;
            PerRequestRavenControllerExtensions.EndAction();
        }
    }

**And the extensions**


    public static class PerRequestRavenControllerExtensions
    {
        private static DocumentStore _store;

        public static IDocumentSession Documents(this Controller controller)
        {
            return GetSecureDocuments();
        }


        public static void BeginAction()
        {
            HttpContext.Current.Items["DocumentSession"] = _store.OpenSession();
        }

        public static void EndAction()
        {
            var session = (IDocumentSession) HttpContext.Current.Items["DocumentSession"];
            session.SaveChanges();
            session.Dispose();
        }

        public static void Initialize(IDocumentStore store)
        {
          _store = store;
        }
    }

**Being direct**

This is a very direct way of doing per-request IDocumentSession and while it's non obvious that I need to call Initialize on those Extensions, it's part of the infrastructure and exists in a single place in my application so I'll live.

In my AppStart, which I'll designate my Application Root and almighty Controller Of All Things Regarding Setup (in the absence of a real entry point ala real-coding in NodeJS)


    var store = new DocumentStore()
    {
        DefaultDatabase = "Hotelier"
    };
    store.ParseConnectionString(ConfigurationManager.AppSettings["RavenConnectionString"]);
    store.Initialize();

    PerRequestRavenControllerExtensions.Initialize(store);
   

And that's me done with that. I now have access to RavenDB throughout my ASP.NET MVC controllers and that's a *good thing*. RavenDB is very much part of this application and there is no point hiding from it.


