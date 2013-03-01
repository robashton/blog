I'm [playing with the EventStore](/entries/playing-with-the-eventstore.html) and I need to push some data into it in the form of streams.

What does this look like? Well I'm using NodeJS, and naturally that means using JSON and object literals:

So, if I have an event

    // An Event
    {
      Data: {
        PonyName: "Rainbow Dash",
        TrampStamp: "Rainbow",
        Date: "January 2013"
      },
      EventType: "PonyBorn"
    }


And I want to get this into a stream, well first I want to package it up

    // A package with the event in it
    {
      CorrelationId: "something-i-know",
      ExpectedVersion: "last-version-i-knew-about",
      Events: [ ev ]
    }

And serialise it

    var body = JSON.stringify(package)


I can POST it to the event store with the following code

    var req = http.request({
      host: "127.0.0.1",
      port: 2113,
      path: "/streams/ponies",
      method: "POST",
      headers: {
        "Accept": "application/json",
        "Content-Type": "application/json",
        "Content-Length": body.length
      }
    }, function(res) {
      // Handle this
    })
    
    req.write(body)
    req.end()


What do we notice about the data?

- We can supply a correlation id for our own convenience
- We supply an expected version so our event can be rejected if things are not as they should be
- We can send a collection of events to be committed all as one
- EventType can be sent in alongside the event data

And what do we notice about the request?

- We choose which stream to post to as part of the URL
- We specify the content types we expect and are sending (because it can accept XML etc)

What happens once I've done this?

Well, we'll see that I have a ponies stream

*/streams*

    {
      title: "ponies",
      uri: "http://127.0.0.1:2113/streams/ponies",
      accepts: [
        {
          type: "text/xml"
        },
        {
          type: "application/atom+xml"
        },
        {
          type: "application/json"
        },
        {
          type: "application/atom+x.json"
        }
      ]
    },

And that if we go to this ponies stream via the URI specified we'll see

*/streams/ponies*

    [
      {
        title: "ponies #1",
        id: "http://127.0.0.1:2113/streams/ponies/1",
        updated: "2013-03-01T22:30:11.790066Z",
        author: {
          name: "EventStore"
        },
        summary: "Entry #1",
        links: [
          {
            uri: "http://127.0.0.1:2113/streams/ponies/1",
            relation: "edit"
          },
          {
            uri: "http://127.0.0.1:2113/streams/ponies/event/1?format=text",
            type: "text/plain"
          },
          {
            uri: "http://127.0.0.1:2113/streams/ponies/event/1?format=json",
            relation: "alternate",
            type: "application/json"
          },
          {
            uri: "http://127.0.0.1:2113/streams/ponies/event/1?format=xml",
            relation: "alternate",
            type: "text/xml"
          }
        ]
      },
      {
        title: "ponies #0",
        id: "http://127.0.0.1:2113/streams/ponies/0",
        updated: "2013-03-01T22:30:11.79004Z",
        author: {
          name: "EventStore"
        },
        summary: "Entry #0",
        links: [
          {
            uri: "http://127.0.0.1:2113/streams/ponies/0",
            relation: "edit"
          },
          {
            uri: "http://127.0.0.1:2113/streams/ponies/event/0?format=text",
            type: "text/plain"
          },
          {
            uri: "http://127.0.0.1:2113/streams/ponies/event/0?format=json",
            relation: "alternate",
            type: "application/json"
          },
          {
            uri: "http://127.0.0.1:2113/streams/ponies/event/0?format=xml",
            relation: "alternate",
            type: "text/xml"
          }
        ]
      }
    ]


We have two events, navigating to them we can see that one of them is for the creation of the stream

*/streams/ponies/event/0?format=json*

    {
      eventStreamId: "ponies",
      eventNumber: 0,
      eventType: "$stream-created-implicit",
      data: "",
      metadata: ""
    }

And the other one is the event we pushed

*/streams/ponies/event/1?format=json*

    {
      eventStreamId: "ponies",
      eventNumber: 1,
      eventType: "PonyBorn",
      data: {
        PonyName: "Rainbow Dash",
        TrampStamp: "Rainbow",
        Date: "January 2013"
      },
      metadata: ""
    }

Neato, I guess we notice a few things here then

- The stream doesn't contain the actual events, just links to the events
- The stream is pageable, and contains the links to the pages (well, it's AtomPub)
- Each event has its own unique uri, because events are immutable these can be cached by any intermdiate proxy

And indeed, if we look at the header on a HTTP request for one of these events we'll see

    Cache-Control:max-age=31556926

That's cool, we've discovered that

- We can throw events into the event store with a default partitioning (the stream name specified)
- We can get them back out again by paging through the links in that stream
- Events are infinitely cacheable
- Everything is AtomPub
- Everything is easily navigable

Now, if we were building a standard event sourced model we'd be able to page through these streams to build up our snapshots/viewmodels and that's all very neat and tidy and that would be the end of our story.

Next up however, it's more interesting to go and have a look at projections now, and see what questions we can ask of those streams in the event store itself.
