So, this is a niche case that is quite annoying for people writing transformers as part of a map or a map/reduce index - and now we have the ability to specify transformers separately to the index this problem only gets worse.

What is this problem?

Well, if I make a map that does this:

    public class Ponies : AbstractIndexCreationTask<Pony>
    {
       public Ponies() {
         Map = ponies =>
               from pony in ponies
               select new {
                 pony.Name,
                 pony.Colour
               }

         Store(pony => pony.CutieMark, FieldStorage.Yes);
       }
    }

Available to me *without even loading the document* after performing a query, is "Name", "Colour", and "CutieMark". Obviously we can store other things in there as well, and so some funky things in the map - but this is the essence of this functionality.

The same thing happens by default when you apply a reduce stage to an index, because you lose the original document entirely because of aggregation.

This can be a bit confusing, as sometimes you want the result from that store data, and sometimes you want the data from the document, and this has been an all-or-nothing thing.

Now we can [Use Load<>](/entries/result-transformers---not-just-for-querying.html) with a ResultTransformer, this is even more ambiguous.

So, from now on, whenever you access anything in a ResultTransformer, RavenDB will first look in the stored fields for the information, and if not found - will load the document associated with the current result and fetch it from that.

It's a small change, but one which will remove a lot of the confusion around this feature.



