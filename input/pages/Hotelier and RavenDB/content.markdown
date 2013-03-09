I'm building a hotel booking system aimed towards small hotel/bnb owners who have static websites and want to embed a booking system onto those pages.

I made the assertion I was going to use RavenDB, this is why:

- I had a sample project written using RavenDB a couple of years ago, it's out dated, I want to bring it forwards
- I haven't written much about RavenDB since those two years, and people are complaining that the blog entries I wrote are out of date
- RavenDB is *fantastic* for this kind of project, I know from experience that it'll stay out of my way while I put things together
- I want to do functional end-to-end testing, but don't want to write in-memory persistence. RavenDB will do this for me.

That's about it. If you haven't used RavenDB yet then perhaps this will show you why you might choose to in a project of this nature.

As a reminder, I'm able to with RavenDB create basic POCOs and persist them without worrying about it like so


**Persisting a new hotel**


    using(var session = store.OpenSession()) {
      var hotel = new Hotel("The waterfall Inn");
      session.Store(hotel);
      session.SaveChanges();
    }



**Loading a hotel, changing something about it**


    using(var session = store.OpenSession()) {
      var hotel = session.Load<Hotel>(id);
      hotel.AddRoom(new Room(5));
      session.SaveChanges();
    }


Remember? It has auto change tracking, which means no faffing around remembering to call Save etc


**Searching for hotels**

    using(var session = store.OpenSession()) {
      var hotels = session.Query<Hotel>()
                          .Where(x=> x.Name == "Waterfall")
                          .ToList();

    }


Remember? Auto index creation and look-up meas you can very easily do queries etc, kinda neat.

**Basically**

RavenDB makes life easy and I want to focus a bit more on my method of building this type of application in general so it's ideal for that :)



