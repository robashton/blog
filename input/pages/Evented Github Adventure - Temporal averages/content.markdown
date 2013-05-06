Carrying on in the EventStore series...

- [Introduction to the EventStore](/entries/playing-with-the-eventstore.html)
- [Pushing data into the EventStore](/entries/pushing-data-into-streams-in-the-eventstore.html)
- [Projections in the EventStore](/entries/basic-projections-in-the-eventstore.html)
- [Re-partitioning streams in the EventStore](/entries/re-partitioning-streams-in-the-event-store-for-better-projections.html)
- [Creating a projection per stream](/entries/creating-a-projection-per-stream-in-the-eventstore.html)
- [Pumping data from Github into the EventStore](/entries/less-abstract,-pumping-data-from-github-into-the-eventstore.html)
- [Emitting new events from a projection](/entries/evented-github-adventure---emitting-commits-as-their-own-events.html)
- [Who is the sweariest of them all?](/entries/evented-github-adventure---who-writes-the-sweariest-commit-messages.html)
- [Temporal queries in the event store](/entries/evented-github-adventure---temporal-queries,-who-doesnt-trust-their-hardware.html)
- [Projections from multiple streams](/entries/evented-github-adventure---crossing-the-streams-to-gain-real-insights.html)
- Temporal averages

If we can [emit temporal events](/entries/evented-github-adventure---temporal-queries,-who-doesnt-trust-their-hardware.html) by partitioning streams and building up state over time, then we can probably go a bit further and build up averages over "How long things usually take", let's have a look at the temporal projection we used to emit time-based events:


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

What we can actually do here is instead of checking for a difference, simply emit an event for every time we receive a PushEvent notification which happened after another one we already saw:

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

            emit('successivepushes', "SuccessivePush", {
              first: state.lastPush,
              next: ev,
              difference: difference
            })
          }
          state.lastPush = ev
          return state
        }
      })


This can actually lead to better analysis of our data, for example

*What are the average times between pushes across the different languages*

So, this time we'll partition by language and work out an average time between pushes per language

    fromStream('successivepushes')
      .when({
       "$init": function() {
         return {}
       },
       "SuccessivePush": function(state, ev) {
         var langState = getPerLanguageState(state, ev)
         langState.total++
         langState.totaltime += ev.body.difference
         return state
       }
      })

Now how I got to re-use that per-repo stream I created earlier.

<div id="scaled"></div>

And there we can see that C# developers take the longest between pushes on average, pretty neat! (Presumably this is because they're waiting for their builds to finish)

<script type="text/javascript" src="/d3.v2.js"></script>
<script type="text/javascript">
  var data = {"Processing":{"totaltime":0,"total":1},"C":{"totaltime":8517,"total":31},"Shell":{"totaltime":2276,"total":7},"Ruby":{"totaltime":15029,"total":28},"Java":{"totaltime":8214,"total":28},"Python":{"totaltime":11622,"total":30},"Scala":{"totaltime":37,"total":1},"C++":{"totaltime":7968,"total":41},"null":{"totaltime":22123,"total":39},"JavaScript":{"totaltime":28994,"total":72},"Objective-C":{"totaltime":4185,"total":6},"PHP":{"totaltime":9349,"total":25},"Emacs Lisp":{"totaltime":410,"total":2},"C#":{"totaltime":4968,"total":7},"Rust":{"totaltime":1079,"total":2},"Clojure":{"totaltime":798,"total":3},"Haskell":{"totaltime":0,"total":2},"ASP":{"totaltime":919,"total":1},"R":{"totaltime":3744,"total":3},"Erlang":{"totaltime":489,"total":2},"CoffeeScript":{"totaltime":435,"total":1},"Dart":{"totaltime":210,"total":1},"VimL":{"totaltime":884,"total":2},"Visual Basic":{"totaltime":1571,"total":2},"ActionScript":{"totaltime":1504,"total":1},"OCaml":{"totaltime":0,"total":1},"Matlab":{"totaltime":2181,"total":3}}
</script>

<script type="text/javascript">

   var filteredData = []
   for(var i in data) {
     if(data[i].total >= 10) {
       var datum = data[i]
       datum.state = datum
       datum.state.average = Math.floor(datum.state.totaltime / datum.state.total)
c
       filteredData.push(datum)
     }
   }

  var svg = d3.select("#scaled").append("svg")
          .attr("width", 800)
          .attr("height", 480)

   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.state.average })])
     .range([0, 1]);

   var max = d3.max(filteredData, function(d) { return d.state.average });

   svg.append("text")
      .attr("fill", '#000')
      .attr("x", 710)
      .attr("y", 60)
      .text(max + " secs")

   svg.append("text")
      .attr("fill", '#000')
      .attr("x", 710)
      .attr("y", 350)
      .text(0 + " secs")

   svg.selectAll(".label")
      .data(filteredData)
      .enter()
        .append("text")
        .attr("class", "label")
        .attr("transform", function(d, i) { 
          var transform = "translate(" + i * (640 / filteredData.length) + "," + 380 + ") "
          transform += "rotate(75) "
          return transform
        })
        .attr("x", 0)
        .attr("y", 0)
        .text(function(d) { return d.key })

    svg.selectAll(".time")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "time")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - (280 * scale(d.state.average)) })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return 280 * scale(d.state.average) })
</script>

