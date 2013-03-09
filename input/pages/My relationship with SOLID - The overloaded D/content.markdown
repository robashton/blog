My week of SOLID: so far:

- [S]()ingle responsibility
- [O]()Open closed
- [L]()iskov substitution
- [I]()nterface segregation
- [D](#)ependency inversion

We've reached D, and that's where the wave we started with L finally hits the shore and materialises into something we can use.

First off, a common misconception that I've heard in talks at conferences several times

  <blockquote>
    DI is for testing
  </blockquote>

**NO**, this is not what DI is for, DI is a means of inverting the relationship between a module and its consumers and vice versa.

  <blockquote>
A. High-level modules should not depend on low-level modules. Both should depend on abstractions.
  </blockquote>

  <blockquote>
B. Abstractions should not depend upon details. Details should depend upon abstractions.
  </blockquote>

We've been using that Stream example for the last couple of blog entries and we'll carry on doing that here as an example and evolution

Stream is an example of a *low level module*, and we can consider IStream to be an abstraction of that, which *high level modules* can consume

    public interface IStream {
      void Read();
      void Write();
      void Seek();
    }

    public class Stream : IStream { etc }

However, this is still not really an abstraction, the abstraction is a lie because it is an exact mirroring of the actual Stream object.

You want to talk dependency inversion? Let's talk dependency inversion, let's mix it up a little bit too and talk about inversion of control and not get too bogged down in the *goddamned definitions*

Let's talk about how I like to code instead

**How I like to code sometimes**

I'm writing a high level module, let's say it's a Controller, and it needs to construct a view model for return to the outside world for rendering in a View of some sort. 

Let's mix it up a little bit and do it in NodeJS like I'm pretty much doing everywhere at the moment.

    app.get('/ponies', function(req, res) {

    })


I don't like to typically make decisions in a long-term project about persistence until I know more about my use cases.

Instead, what I'll usually do is say to the outside world, *I need something that can give me some ponies*

    app.get('/ponies', function(req, res) {
      var ponies = stable.availablePonies(req.params.count, req.params.pagesize, req.params.page)
      res.send(ponies)
    })

What is stable? Well, stable was instantiated by the Application Root and I don't care what stable is, a basic implementation (and indeed the one I start off with is)

    stable = new InMemoryStable()

This allows me to write features quickly, allows me to write functional tests that are relatively fast and allows me to iterate on these things quickly and defer the decision about my database until it becomes necessary to deal with it.

The important point here is that implicit interface being defined here (I'm in JS, we haven't got a real interface), let's call it "IHousePonies" has been dictated by the high level module and it doesn't care what the low level code behind that interface does.

That's an inversion of responsibility and it's a good one too because it means I'm unlikely to run into violations of [ISP](/entries/my-relationship-with-solid---seeing-i-to-i.html) because the high level module *requires* that the functionality on that interface be present on all implementations of that interface.

This is close-ish to what some people would describe as using [role interfaces](http://martinfowler.com/bliki/RoleInterface.html) which are worth reading up on.

**How I like to code other times**

If I'm in .NET building the standard ASP.NET MVC application that clients tend to want, I'll pull out RavenDB which has an in-memory mode and use its interfaces directly across my application. They are an abstraction already and that abstraction hides

  - The use of RavenDB as an embedded database
  - The use of RavenDB as a remote HTTP database
  - The use of RavenDB as an in-memory database (for testing)


