[Last entry](/entries/adding-idle-indexes-to-ravendb.html) I talked about the addition of a priority flag on indexes to save resources on un-important indexes.

This is great news for those indexes that are created and managed manually (once Raven Studio has the UI for it anyway), but a lot of customers are using auto indexes (a feature I'm proud to say I had a hand in [All those years ago](http://ayende.com/blog/4667/ravens-dynamic-queries)).

What does this therefore mean for this type of index? Well, the way RavenDB currently works for those unfamiliar with it, is if you make a query for all the ponies with rainbow in their name like so:

    session.Query<Pony>().Where(pony => pony.Name.Contains("Rainbow"))

There is a query optimiser which will try to find an appropriate index to use for this query, and if it fails it will create an index for you at the following URL:

    /Index/Temp/PonyWithName

Or something similar (I forget the exact conventions). After a period of time, the index will be deleted unless it is used within a certain threshold of activity.

On top of this, we now have idle indexes - so what does this mean? We can actually do a lot of this automatically now too, if we select the least-used indexes in the system *(for example, order all the auto-indexes by the last time they were queried, and if there is a big gap between the last index and the penultimate index, then demote the index automatically)*

If the index is queried against after this, we can promote it again - happy days, thus the following scenarios:

- Manual indexes get left alone unless managed in the studio
- Auto indexes get made idle if they're not queried very much
- Auto indexes get promoted if they're then queried

Having the database make these decisions for you keeps away any of the possible complexity of maintaining a well behaving RavenDB implementation.

I left off yesterday by talking about the Forced flag, the idea behind this is you can force an index to keep its current state by using the Studio and not have any of this fancy magic stuff happen  for you. I expect most people won't use it, but it's always good to give customers the ability to assert control.

Now, with this done - the next natural cause of action is obvious really, what would that be...? Stay tuned for my next entry to find out.
