It's all very well and good [talking](/entries/playing-with-the-eventstore.html) [events](/entries/pushing-data-into-streams-in-the-eventstore.html) [in](/entries/basic-projections-in-the-eventstore.html) [the](/entries/re-partitioning-streams-in-the-event-store-for-better-projections.html) [abstract](/entries/creating-a-projection-per-stream-in-the-eventstore.html), but there is only so long I can blather about ponies before I run out of the kind of data I can ask interesting questions about.

We turn to the [Github Events API](http://developer.github.com/v3/activity/events/#list-public-events), something I have a [bit of experience](/entries/github-live.html) with for inspiration and start dumping all the events into the event store.

What does this look like?

Well, I'm interested in the public events stream, which can be polled up to 5000 times an hour (at current rates, it needs polling about every 10 seconds in order to keep up, so I'll not be able to get them all)

What this looks like

     var request = https.get(
     { host: 'api.github.com', path: '/events' + auth }, 
     function(res) {
        var data = ''
        res.on('data', function (chunk) {
          data += chunk
        });
        res.on('end', function() {
          processData(data)
        })
      })
      .on('error', function(e) {
        console.error(e)
      }).end()

      var processData = function(data) {
        var eventArray = JSON.parse(data)
        for(var i = eventArray.length-1 ; i >= 0; i--) {
          processEvent(eventArray[i])
        }
      }

An event looks like this

    {
      id: "somelongid",
      type: "PushEvent",
      actor: { // info about the user },
      repo: { // info about the repo }
      payload: { // the event itself }
    }

I'm going to be shoving these events 'as is' into the EventStore, and using their ids 'as is' too, this means I don't need to do any de-duping or anything like that.

Now, because of the kind of question I want to ask, it isn't enough for me to have the scant info about a repo that the event stream gives me (it looks like this)

    {
     "id": 3,
     "name": "octocat/Hello-World",
     "url": "https://api.github.com/repos/octocat/Hello-World"
    }

So I'm going to augment each event with repo information (this is quite common in the eventing world, augmenting events with useful information for query purposes), and therefore my processEvent method looks something like this:

    function processEvent(ev) {
      if(ev.repo) {
        fetchRepoInfo(ev.repo.name, function(repo) {
          ev.repo = repo
          pushEventIntoEventStore(ev)
        })
      } else {
        pushEventIntoEventStore(ev)
      }
    }

So, I'm not altering any of the events in any way, except by adding repo information to them, therefore if you're interested in the structure of any of the events I'm using you can easily look them up in the API.

By the time the code is readable there'll be some rate management code in there because I can't go looking up repo information for every single event and not go over the rate limit, but it's safe to say we'll be getting ~50% of the events from Github and that's a reasonable amount.

My script simply sits there running in the background and throws events into the event store and this little experiment is going to be about creating projections and asking questions of those events as we go along. Capiche? :)
