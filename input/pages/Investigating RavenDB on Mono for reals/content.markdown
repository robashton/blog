I've been down [this road before](/entries/ravendb-on-mono.html) with a certain amount of success, and indeed I even did several talks entirely in linux about RavenDB complete with working demos (although some folk might doubt that story).

So what gives then? How come RavenDB doesn't work on Mono out of the box now a couple of years later? Well - the short answer is that it never did. 

When I got it working on Mono in the past it was on top of "Munin", a managed storage engine that [@ayende](http://twitter.com/ayende) and I hacked up in a hotel lobby in the small hours of the morning that "mostly works" but has never entirely been recommended for production.

That's not to say we haven't got a lot of mileage out of it, as indeed it has been used in pretty much everyone's test suites since its conception and has faithfully done as asked all that time; however, it is clear that for *true* cross-platform happiness, we could do with a replacement for Esent for platforms that don't support it, and re-inventing the wheel ourselves when so many other people have done work on this for us doesn't make a lot of sense.

The other problem is of course that RavenDB was developed on top of the .NET runtime, using the .NET compiler chain - and support for some of the features used by RavenDB make compilation on mono an hilariously frustrating process of bug fixes and pull requests (which then means it won't work OOB on versions of mono packaged with popular Linux distros). 

I'm not going near that with a barge pole, although I'm sure if somebody wants to sit there and get RavenDB simply *building* on Mono, it'll only take a few hours of workarounds which will be happily accepted as a pull request.

With that, this series of blog posts is **not** going to finish with an entry saying "It now works on Mono", because that's going to take a couple of weeks' work and I haven't got a couple of weeks. However, the breadcrumbs and work completed so far should be good enough for anybody with the spare time and inclination to finish that work and get it working for reals.

So, we've been doing some investigations, and in summary it looks like LevelDB is a good option for our usage patterns - I'm going to spend a couple of entries going over the little spikes I wrote to verify that, and then show the beginnings of the work inside RavenDB itself to kick this off with.

With a proper cross-platform persistence solution, and a few hours tinkering with the build - RavenDB will work on Mono, allow me to help lay some foundations...
