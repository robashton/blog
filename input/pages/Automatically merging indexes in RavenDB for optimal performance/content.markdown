So, we can mark indexes as idle, we can automatically do this to auto indexes, and we delete auto indexes that aren't being used enough - that paves the way for a pretty useful feature - merging automatic indexes for fun and for profit.

What is that you ask? Well - when we do the following series of queries


    session.Query<Pony>()
        .Where(pony => pony.Name == "Rainbow Dash")
        .FirstOrDefault()


and

    session.Query<Pony>()
		    .Where(pony => pony.Colour == "Pink")
		    .ToList()
 

We will end up with the following indexes in RavenDB

*Pony/Name*
    
		from pony in ponies
		  select new {
		  	pony.Name
		  }
    
*Pony/Colour*
    
		from pony in ponies
		  select new {
		  	pony.Colour
		  }

Well, in reality there is no real reason why we'd have two indexes and incur the cost of iterating over documents every time they're added more often than we need.

One of the things that the query optimiser already does, is look for the 'widest' index - that is the index that touches the most fields.

It makes sense therefore, that rather than simply look for the widest index that matches the query, we should look for the most compatible index and create a new one based off of that if it doesn't match completely.

Consider the above, we should end up with two indexes

		from pony in ponies
		  select new {
		  	pony.Name
		  }

and

		from pony in ponies
		  select new {
		  	pony.Name,
		  	pony.Colour
		  }

Over time, the first index won't be used (as it's not as wide as the second index), and it will wither and die because of idling and auto-removal.

Over time, we'll actually end up with only one index per document type (providing there aren't any reasons why a query isn't compatible with an existing index, such as different indexing or sorting options)

This ensures that over time when using automatically generated queries, that RavenDB keeps itself as lean as possible and using as few resources as possible (and tidy!)

Now that covers what I did on in first couple of days at Hibernating Rhinos, next up I'll talk about what I'm helping do with result transformations.
