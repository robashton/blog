My week of SOLID: so far:

- [S](/entries/my-relationship-with-solid---starting-with-s.html)ingle responsibility
- [O](/entries/my-relationship-with-solid---the-big-o.html)pen closed
- [L](/entries/my-relationship-with-solid---the-misunderstood-l.html)iskov substitution
- [I](/entries/my-relationship-with-solid---seeing-i-to-i.html)nterface segregation
- [D](#)ependency inversion

We've reached D, and that's where the wave we started with L finally hits the shore and materialises into something we can use.

  <blockquote>
A. High-level modules should not depend on low-level modules. Both should depend on abstractions.
  </blockquote>

  <blockquote>
B. Abstractions should not depend upon details. Details should depend upon abstractions.
  </blockquote>

We've been using that Stream example for the last couple of blog entries and we'll pull that out one last time as an example:

Stream is an example of a *low level module*, and we can consider IStream to be an abstraction of that, which *high level modules* can consume

    public interface IStream {
      void Read();
      void Write();
      void Seek();
    }

    public class Stream : IStream { etc }

However, this is still not really an abstraction, the abstraction is a lie because it is an exact mirroring of the actual Stream object. (this is okay sometimes as we'll see below)

**How I like to code sometimes**

I'm writing a high level module, let's say it's a Controller, and it needs to construct a view model for return to the outside world for rendering in a View of some sort. 

Let's mix it up a little bit and do it in NodeJS like I'm pretty much doing everywhere at the moment.

    app.get('/ponies', function(req, res) {

    })


I don't like to typically make decisions in a long-term project about persistence until I know more about my use cases, and indeed in this case I don't even want to care that persistence is even happening - instead, what I'll usually do is say to the outside world, *I need something that houses ponies*

    module.exports = function(app, stable) {
        app.get('/ponies', function(req, res) {
          var ponies = stable.availablePonies(req.params.count, req.params.pagesize, req.params.page)
          res.send(ponies)
        })
    }

What is stable? Well, stable was instantiated by the Application Root and I don't care what stable is, a basic implementation (and indeed the one I start off with is)

    stable = new InMemoryStable()

This allows me to write features quickly, allows me to write functional tests that are relatively fast and allows me to iterate on these things quickly and defer the decision about persistence until it becomes necessary to deal with it (if it does at all)

The important point here is that implicit interface being defined here (I'm in JS, we haven't got a real interface), let's call it "IHousePonies" has been dictated by the high level module and it doesn't care what the low level code behind that interface does.

That's an inversion of responsibility and it's a good one too because it means I'm unlikely to run into violations of [ISP](/entries/my-relationship-with-solid---seeing-i-to-i.html) because the high level module *requires* that the functionality on that interface be present on all implementations of that interface.

This is close-ish to what some people would describe as using [role interfaces](http://martinfowler.com/bliki/RoleInterface.html) which are worth reading up on. Certainly when I'm putting my software-engineering hat on and I'm working on a project where things are likely to be complex, there are likely to be quite a few moving parts and code quality is important I'll lean in this direction.

**How I like to code other times**

If I'm in .NET helping build the standard ASP.NET MVC application that clients tend to want, I'll pull out RavenDB which has an in-memory mode and use its interfaces directly across my application. They are an abstraction already and that abstraction hides

  - The use of RavenDB as an embedded database
  - The use of RavenDB as a remote HTTP database
  - The use of RavenDB as an in-memory database (for testing)

Sticking our own abstraction around this makes very little sense, although if we have complicated domain logic we might end up with some coordinators between the controller and RavenDB.

In most cases the effort of building up abstractions over time from our high level controllers won't really have much of a pay off.

Of course, if elsewhere in that project I have the need to do something that isn't CRUD, then the use of my own abstractions will come into things because hiding the low level details from the high level consumers is still important. These abstractions can be built up over time as needed, rather than defining them all up front.

*Insert any other similar technologies into the above scenarios and it's pretty much the same*

**How any of this can relate to testing**

Well, if we're describing our dependencies on other modules as interfaces that we own, we can create in memory representations of those dependencies in the form of mocks, stubs or full-blown in-memory implementations (the latter being my personal preference in most cases)

Having ownership of the interfaces that we rely on means we can dictate the desired behaviour via tests (or in some languages code contracts), and it means that the tests we write for our module can make assumptions about how the code behind that abstraction is going to work.

Coupling this design principal with techniques such as [Dependency Injection](http://en.wikipedia.org/wiki/Dependency_injection) means that we can make it very clear to the outside world from our module what abstractions we rely on, and allow the outside world to make decisions about what it is we are going to actually get.

**How it can all go wrong - attack of the killer interfaces**

What happens in a lot of projects is that we decide that the reason for DI is for testing, and isolation is really important so we need to have abstractions for everything, and almost everywhere we end up with

    public class Something
    public interface ISomething

Because every class needs an interface in order to be mockable - we forget to apply the inversion part of dependency inversion and instead we just focus on dependencies for our tests.

This isn't helped by most popular IOC frameworks and their default convention that it'll automatically bind instances like the above for us.

This is awful, when we think about inverting the relationship between our high level modules and low level modules, we should be thinking about it in terms of pay-off and not dancing around for the sake of writing pointless low level tests for code with little real behaviour.

We should be limiting our abstractions to the tune of rather than thinking about everything at the class level, thinking about things at the module level (which could be a collection of classes that talk to each other and require some external data/input)

**SOLID - where did it all go wrong?**

I could go on about this till the cows come home, but I don't want to because I've got some stuff to build, so I'll leave it here with a final note:

**ALL** of the SOLID principles are *great*, as a guideline for thinking about code as we write it, I'm not disagreeing with that at all. What I disagree with are statements that say "MUST" or "NOT ALLOWED" etc - because most developers are not master craftsmen (or whatever you call yourselves these days) and trying to make them write code as if they are is what leads to disaster.

Most code should be allowed to grow organically, and caution should be exercised in making sure that we don't end up with that big ball of mud that everybody fears - absolutely. Trying to avoid that big ball of mud by blindly following the SOLID principles leads to the creation of a big haystack where the actual functionality is a needle hidden somewhere underneath.

**fin**
