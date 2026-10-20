We're able to do [aggregations of a stream's content by writing a projection](/entries/basic-projections-in-the-eventstore.html), and that's great - if we want to know about every jump every pony has performed over the entire stream. This is unlikely to be what we actually want though, people are narccisstic and ponies are no different, Rainbow Dash probably wants to know about just her jumping activity and Pinkie Pie wants to know about her own too, Derpy Hooves is probably too busy eating muffins to care, but we'll include her anyway.

- We've got a stream, which we have "all events for all ponies", we called it "ponies" because we're imaginative.
- We've got a projection that can run over that stream and give us a total jump count for that stream
- We want a projection for each pony that gives us the total jump count for that pony
- We therefore want a stream for each pony, so we can generate a projection for that pony

It turns out that this isn't too hard, let's look at how we do this:

Let's start with a basic projection over our ponies stream that doesn't do anything:

    fromStream('ponies')
    .whenAny(function(state, ev) {

    })

To re-iterate what we learned last time, this projection says "From the stream 'ponies', please invoke this callback for every event in the stream regardless of what the EventType is"

What we can actually do is create a new stream per pony, but link back to the original events from those new streams. 

    fromStream('ponies')
    .whenAny(function(state, ev) {
      linkTo('pony-' + ev.data.PonyName, ev)
    })

linkTo takes two arguments

- The name of the stream to link the event to
- The event itself

For this projection, I "enable emits" beacuse that's what we're doing here

In our case, we're going to have a stream created for each pony, called pony-PONYNAME (so pony-rainbowdash, pony-derpyhooves, pony-pinkiepie), let's look at this to verify

*/streams/pony-rainbowdash*

    {
      title: "ponies #5502",
      id: "http://127.0.0.1:2113/streams/ponies/5502",
      updated: "2013-03-02T12:39:12.322785Z",
      author: {
        name: "EventStore"
      },
      summary: "Entry #5502",
      links: [
        {
          uri: "http://127.0.0.1:2113/streams/ponies/5502",
          relation: "edit"
        },
        {
          uri: "http://127.0.0.1:2113/streams/ponies/event/5502?format=text",
          type: "text/plain"
        },
        {
          uri: "http://127.0.0.1:2113/streams/ponies/event/5502?format=json",
          relation: "alternate",
          type: "application/json"
        },
        {
          uri: "http://127.0.0.1:2113/streams/ponies/event/5502?format=xml",
          relation: "alternate",
          type: "text/xml"
        }
      ]
    }


This is just one of the events in that stream, but we can see an important point here, that *a new event was not created, the links are the original events from the original stream*. Cool!

If I look through the list of all streams, I can see that I now have a stream for each pony (in my case this is)

- /streams/pony-rainbowdash
- /streams/pony-pinkiepie
- /streams/pony-derpyhooves

Now what I want to do is create a projection for each of these streams, so I can ask "how far has each of these ponies jumped", that's next :)
