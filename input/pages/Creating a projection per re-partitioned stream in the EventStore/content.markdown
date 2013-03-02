[Now I have a stream per pony](/entries/re-partitioning-streams-in-the-event-store-for-better-projections.html), I want to create a projection per pony, but how do we do this?

Well, so far we've seen these two methods to get the events for our projection

- fromAny: Give us the events from all the streams
- fromStream: Give us the event from a specific stream

Well, now is the time to introduct another method we have at our disposal

- fromCategory: Run this for each "category"

Well, what on earth IS a category? Turns out that the EventStore is quite clever and one of the default projections is hard at work looking at streams and categorising them.

I called my streams pony-PONYNAME for good reason, because this default projection will have gone "Hey, there is a dash i that stream, I'm going to  create a category called pony, and each entry in that category is going to be a PONYNAME"

That leaves us with

    fromCategory('pony')
      .foreachStream()
      .when({
        "$init": function(state, ev) {
          return { count: 0 }
        },
        "PonyJumped": function(state, ev) {
          state.count++
        }
      })

For each stream in the category "pony", please run this projection code!

**Better way**

There is actually a better way of doing this though, where rather than re-partition and then apply a partition per projection, we can do it all in one go

    fromStream('ponies')
      .partitionBy(function(ev) {
        if(ev.body && ev.body.PonyName)
          return ev.body.PonyName
      })
      .when({
        "$init": function(state, ev) {
          return { count: 0 }
        },
        "PonyJumped": function(state, ev) {
          state.count++
        }
      })

We can now look at the state per pony by visiting the /state and passing in the partition we care about in the query string

*/projection/jumpingponies2/state?partition=rainbowdash*

    {
      count: 2000
    }

*/projection/jumpingponies2/state?partition=pinkiepie*

    {
      count: 300
    }

*/projection/jumpingponies2/state?partition=derpy*

    {
      count: 10
    }

**NOTE**

It's at this point, people usually ask "How about giving me a list of ponies", this is *not* what you use the EventStore for, the list of ponies is something that should exist in your domain and be stored in a database (whether this be a document or relationaldatabase), and then used to look up values in the event store.

This could either be a fixed list in one of those stores, or you could run through the streams in the EventStore and build up that list as a read model in that external store. This is the only time I'm going to mention this in this series!!
