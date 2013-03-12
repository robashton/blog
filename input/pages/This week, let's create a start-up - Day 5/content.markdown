**This post was written on Friday**

Final day of "[Build a start-up in a week](entries/this-week,-lets-create-a-start-up.html)", how did we do?

Well, we deployed all assets to Amazon and ported across Sam's first customer from his original single-tenant system and everything works as expected. This work included lots of tidy up and "making nice jobs", very little faffing :)

- Sticking jPlayer into an iFrame so Sam could skin it 
- Write a migration script to take data from the original system and create an org for it in the new system
- Setting up autoplay from the home page (so pressing play on a sermon would re-direct to the sermon page and play the sermon via jPlayer)
- Fulltext search functionality on both the public site and the admin site through sermons
- Adding the series info on the sermon viewing page
- Styling


Not very exciting, but all very trivial (even full text search)


*Migration script*

This just loaded the original data into memory as a string, de-serialized it into the old data types, copied it across into new data types, re-wrote ids across references and then called SaveChanges.

Didn't even bother using any of the bulk support in RavenDB as the amount of data was trivial, the dumb solution is sometimes the best- *next*

*Full text search*

YAY RAVENDB

        public class SermonSearchIndex : AbstractIndexCreationTask<Sermon>
        {
            public SermonSearchIndex()
            {
                Map = docs => from doc in docs
                              select new
                                  {
                                      doc.OrganisationId,
                                      doc.ServiceType,
                                      doc.Title,
                                      doc.SpeakerName,
                                      doc.BibleReference,
                                      doc.SermonDate,
                                      doc.IsPublished
                                  };

                Index(x => x.ServiceType, FieldIndexing.Analyzed);
                Index(x => x.Title, FieldIndexing.Analyzed);
                Index(x => x.SpeakerName, FieldIndexing.Analyzed);
                Index(x => x.BibleReference, FieldIndexing.Analyzed);
                
            }
        }
    }


    if (!string.IsNullOrEmpty(input.Search))
    {
        query = query.Where(x => x.Title == input.Search || x.SpeakerName == input.Search || x.BibleReference == input.Search || x.ServiceType == input.Search);
    }


Can't argue with how easy that was, and it still all works with that original paging stuff I wrote on the first day.

*Adding the series info for a sermon*

    var sermon = session.Load<Sermon>(id)
    // if null etc
    var series = session.Load<Series>(sermon.SeriesId)

Remember that ISecureDocumentSession [I wrote yesterday](/entries/this-week,-lets-create-a-start-up---day-4.html)? That made the Include stuff hard to cater for on a Load, so I just do two load calls instead - in a more evolved system we'd have to do this better somehow because it isn't going to scale across all the other usages that IDocumentSession can give us.

Instead I'd look at hooking into RavenDB properly to do this security (either using its server-side security bundle, or adding appropriate extension points for this sort of filtering job on the client)

For this sort of thing though, it's two remote calls rather than one on a fairly low traffic system so it should be okay for now.

*Deploying*

Took 10 minutes to get onto Amazon thanks to Sam's efforts earlier in the week, and 5 minutes to replace the content on the old system with the script tag to import data from the new Truthvine system. (That's what the customer would have to do to use it)

If that's not easy I don't know what is. 

*Summary*

ASP.NET MVC is surprisingly tolerable if you leave your opinions and a bit of brain-matter at the door on your way in, certainly it's pretty fine at throwing together a quick cruddy application on top of something simple like RavenDB.  Oh, and Razor is a thing of beauty - well done Microsoft for getting at least one thing right this past few years (Aww, just kidding, you know I love you really)

Mission accomplished and nothing in the solution is messy at all thanks to the no-crap atttitude of throwing things together that work in the simplest way possible.

I wish Sam luck on his start-up adventure and hope he finds enough clients to make the effort worthwhile, he's got a good project on his hands and I hope I've given him the boost he needed to get going.

