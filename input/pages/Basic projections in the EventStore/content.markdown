Being able to [shove events in and out](/entries/pushing-data-into-streams-in-the-eventstore.html) is great for our event sourced apps, but actually - if we have these streams, it's really useful to be able to consistently manipulate the events as they come in and either query those events or re-organise them so they can be queried.

To do this, we have the notion of projections, which are chunks of code that can be executed over a stream (with persisted state) as events are added to it. Now, in reality this isn't actually too different to what we'd be doing outside the event store when building up view models, and I foresee lots of bad things being done by people are the line between these different ways of reading streams are blurred and fought over.

I'm actually quite interested in these projections as a way of building up state for reports/charts, or re-partitioning into streams for different consumers - let's have a look how the most basic of these could work. 

Let's say I've got a few events in the general structure of

    {
      EventType: "PonyJumped",
      Data: {
        Pony: "Derpy Hooves",
        Height: 10,
        Distance: 13
      }
    }

and

    {
      EventType: "PonySpoke",
      Data: {
        Sentence: "This is the best day ever",
        Pony: "Pinkie Pie"
      }
    }

And I'm putting all of these events into a single stream *ponies*

Let's say that I've thrown a few hundred thousand of these events through the event store and I want to know something really basic, like how many times the ponies in my world have jumped since time began.

There is a Web UI available for managing projections in the event store (by default available at 127.0.0.1:2113). This is still subject to change though so I'll just be describing the ideas behind this concept.

There are some attributes of a projection that we can choose when creating a projection via HTTP or the UI

- Name: This is the name of the projection, I'll use this to look up the state
- Source: This is the code to be executed
- Emit Enabled: Projections can emit (or link) events if this is enabled
- Enabled: Is this projection going to run?

My options

- Name: "PonyJumpCount"
- Source: TBC
- Emit Enabled: Leaving this false (we'll cover usage of this later)
- Enabled: Yes please!

Now for the source, we'll start with the most basic projection which looks like this:

    fromAll()
      .whenAny(function(state,event) { 
         return null; 
      });
          
Basically, we have to select which streams we're going to be reading our events from, that "fromAll" bit means we're going to be reading from all of the streams, I'm going to go ahead and change that to "fromStream" and select our "ponies" stream.

    fromStream("ponies")
      .whenAny(function(state,event) { 
         return null; 
      });

How about that next bit "whenAny", well we've already matched which stream we want events from, well this is the bit we get to use to select which events from that stream we're interested in - "whenAny" just means "all the events in the stream".

I'm going to go ahead and change that to a when, which takes in a map of the events we're interested in and the callback to process the event with (pattern-matching on the EventType)

    fromStream("ponies")
      .when({
        "PonyJumped": function(state, event) {
          return null;
        })
      
Now for that callback - we have "state" and "event", the former being the state we're building up for this projection, and the latter being the event we're going to be adding to that state.

To begin with, we haven't actually got any state, but we can rectify that by chucking in an "$init" handler (anything starting with a dollar is something built in to the event store)

    fromStream("ponies")
      .when({
        $init: function() {
          return { count: 0 }
        },
        "PonyJumped": function(state, event) {
          return state
        }
      })

And now, the actual bit of code for building up our projection

    fromStream("ponies")
       .when({
            $init: function() {
               return { count: 0 }
            },
            "PonyJumped": function(state, event) {
              state.count += 1
            }
      })
        
If I hit save and navigate to

*http://127.0.0.1:2113/projection/PonyJumpCount/state*

    {
      count: 1337
    }

I get some wonderful state.

Well, that was very basic, next time we'll look at how we could generate one of these projections for each pony we have in our world.
