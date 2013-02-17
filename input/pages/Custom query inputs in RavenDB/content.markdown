Back on the list of things "added to RavenDB" [during my time at Hibernating Rhinos](/entries/working-at-hibernating-rhinos.html), ever wished you could pass custom arguments to the transformer when performing a query in RavenDB?

Well, this has been asked for a number of times and since [splitting out results-transformers](/entries/ravendb-resulttransformers---a-new-way-of-looking-at-things.html') into their own process, it has become much easier to add this functionality.

What does this look like?

Well, say we have a result transformer that takes Ponies and creates unicorns, only our database doesn't know about horns - let's see what we can do here.

    public class Unicorn {
      public string Name { get; set; }
      public string Colour { get; set; }
      public string CutieMark { get; set; }
      public int Hornsize { get; set; }
    }

    public class PoniesIntoUnicorns : AbstractTransformerCreationTask<Pony> {
      public PoniesIntoUnicorns() {
        Transform = ponies => from pony in ponies
                              select new {
                                pony.Name,
                                pony.Colour,
                                pony.CutieMark,
                                Hornsize = pony.Size * Query["hornscalefactor"]
                              }
      }
    }

Okay, contrived example but this feature isn't for me and my pony database, it's for you and your requirements, and you know it's you I'm talking about because you're looking at the above and going *finally, I've been waiting for this*.

How do we use the above?

    session.Query<Pony>()
           .Where(pony => pony.Colour === "purple")
           .TransformWith<PoniesIntoUnicorns, Unicorn>()
           .AddQueryInput("hornscalefactor", 0.1)
           .ToArray();

Pretty simple and effective, glad I could oblige :)


Oh yeah, it works for Load too

    var unicorn = session.Load<PoniesIntoUnicorns, Pony>(
                  x=> x.AddQueryInput("hornscalefactor", 0.1))

Not the tidiest API in the world, but I'm sure it will improve as people actually use it.

