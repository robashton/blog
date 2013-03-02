I'm [ploughing all the events from Github into the EventStore](/entries/less-abstract,-pumping-data-from-github-into-the-eventstore.html) as is, but that doesn't mean they're instantly available for querying.

Lets say I want to write a few projections analysing the commits made across Github and performing some correlations off the back of that.

Well, currently there is no such thing as a CommitEvent - what we actually have is a PushEvent which contains a list of Commits in the payload like so

    {
       type: "PushEvent",
       repo: { // repo info },
       payload: {
         commits: [
          {
            sha: "etc",
            author: { //etc },
            message: "I am a banana"
          },
          {
            sha: "etc",
            author: { //etc },
            message: "My spoon is too big"
          },
          {
            sha: "etc",
            author: { //etc },
            message: "Tuesday's coming, did you bring your coat?"
          }
         ]
       }
    }

Let's say I want to build up projections off the the stream of commits, in each of my projections I'd have to write the following code

    fromStream("github")
      .when({
        "$init": function(state, ev) {
          return {}
        },
        "PushEvent": function(state, ev) {
          for(var i = 0 ; i < ev.body.payload.commits.length; i++) {
            var commit = ev.body.payload.commits[i]
            var repo = ev.body.repo

            // do stuff
          }
        }
      })

This doesn't cause a huge problem, but it is unwieldy and sub-optimal, if only we could instead for all the analysis we want to do over the commits.

    fromStream("github-commits")
      .when({
        "$init": function(state, ev) {

        },
        "Commit": function(state, ev) {
          var commit = ev.body.commit
          var repo = ev.body.repo
          
          // Do stuff
        }
      })

Well in fact we can, and that's where the 'emit' function comes into it, let's say we have our original projection which loops over those commits:

    fromStream("github")
      .when({
        "$init": function(state, ev) {
          return {}
        },
        "PushEvent": function(state, ev) {
          for(var i = 0 ; i < ev.body.payload.commits.length; i++) {
            var commit = ev.body.payload.commits[i]
            var repo = ev.body.repo
            emit("github-commits", "Commit", {
              commit: commit,
              repo: repo
            })
          }
        }
      })

And lo, we now have a new stream caled "github-commits", with a pile of "Commit" events with the commit and the repo information for that commit.

*/streams/github-commits*

    {
      title: "github-commits #2266",
      id: "http://127.0.0.1:2113/streams/github-commits/2266",
      updated: "2013-03-02T15:20:04.207363Z",
      author: {
        name: "EventStore"
      },
      summary: "Entry #2266",
      links: [
      {
        uri: "http://127.0.0.1:2113/streams/github-commits/2266",
        relation: "edit"
      },
      {
        uri: "http://127.0.0.1:2113/streams/github-commits/event/2266?format=text",
        type: "text/plain"
      },
      {
        uri: "http://127.0.0.1:2113/streams/github-commits/event/2266?format=json",
        relation: "alternate",
        type: "application/json"
      },
      {
        uri: "http://127.0.0.1:2113/streams/github-commits/event/2266?format=xml",
        relation: "alternate",
        type: "text/xml"
      }
      ]
    },

Now, unlike "linkTo", this actually creates new events - as can be seen by the URIs in the above sample, and this decision comes with its own considerations but this is what I'll roll with for now and see where it gets me.


