Doing [basic aggregations](/entries/evented-github-adventure---who-writes-the-sweariest-commit-messages.html) is fairly easy, but actually one of the great selling points of the projections system inside the EventStore is how easy it is to do temporal queries.

What is a temporal query? Well the example that [Greg Young](http://twitter.com/gregyoung) always uses is "look for all the events in Twitter where somebody mentions starbucks, coffee, and happy within five minutes of each other"

The clue there is "look for all the events", and we'll see why that is with this example.


**Git**

Git is great, it's great because it's a DCVS and we can do everything locally and push to a remote repo when we feel we're ready.

That said, some people don't do this, every commit is a push - maybe it's because they're not used to using a system where they can work offline, or maybe they're just paranoid because they're working on a laptop they don't trust.

Either way, this is a great example of a temporal query we could write.

**A query on Git**

*"Find the instances of pushes within X minutes of each other from the same user and same repo", is there a trend across different languages for this kind of usage?*

I would *not* enjoy doing this in a standard database, but that's neither here nor there, let's see what we have.

First off, the important question is "what is the unique combination we're looking for"

We care about *PushEvent*s for a unique *User* and *Repo*, so it makes sense that what we should do is *partition by UserAndRepo*  so we can easily keep state around per repo.

**Partition by uniqueness**

Here is how that might look:

    fromStream('github')
      .when({
        "PushEvent": function(state, ev) {
          var repo = ev.body.repo.full_name
          linkTo('pushesbyrepo-' + repo, ev)
        }
      })

So far so good, I now have a stream per repo called "pushesbyrepo-{reponame}" - now what?

Well, the key thing I mentioned above was that we're "looking for all the events where", this suggests that perhaps the thing we are looking for should be considered an event in its own right.

We can do this, we can for each of thse streams keep a note of when the last push was, and if we encounter another push within a certain time-frame, emit an event for this find of ours.

**Running a projection per generated stream**

How to run this for each stream? Enter fromCategory

    fromCategory('pushesbyrepo')
      foreachStream()
      .when({
        $init: // etc
      })

There is a built in projection in the EventStore that will automatically use the character '-' to sort a collection of streams into categories, and if we have

- pushesbyrepo-bob
- pushesbyrepo-alice
- pushesbyrepo-derpy

We'll get three categories to iterate through (bob, alice, and derpy)

**Emit the events we're looking for**

    fromCategory('pushesbyrepo')
      foreachStream()
      .when({
        $init: function(state, ev) {
          return {}
        },
        "PushEvent": function(state, ev) {
          if(state.lastPush) {
            var newDate = new Date(ev.body.created_at)
              , lastDate = new Date(state.lastPush.body.created_at)
              , difference = (newDate.getTime() - lastDate.getTime()) / 1000

            if(difference < 120) {
              emit('paranoidpushes', "ParanoidPush", {
                first: state.lastPush,
                next: ev
              })
            }
          }
          state.lastPush = ev
          return state
        }
      })

Seems legit, simply keep the last PushEvent around at all times, when we get a new one, check the difference and emit an event if it's less than X minutes. NICE

Running this, I now have a stream at 

*/streams/paranoidpushes* 

So, how many paranoid pushes have I had in the time period I've been sampling over? (roughly three hours at time of writing)


    fromStream('paranoidpushes')
      .when({
        "$init": function(state, ev) {
          state.count = 0
        },
        "ParanoidPush": function(state, ev) {
          state.count++
        }
      })

And the result?

    { count: 1504 }

Voila, that's a temporal query that just works - and it's pretty easy too. Now we have this, perhaps we can look at combining this data we have to work out how this strange use of git plays out across different language developers.


