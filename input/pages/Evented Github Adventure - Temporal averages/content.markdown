If we can [emit temporal events](/entries/evented-github-adventure---temporal-queries,-who-doesnt-trust-their-hardware.html) by partitioning streams and building up state over time, then we can probably go a bit further and build up averages over "How long things usually take", let's have a look at the temporal projection we used to emit time-based events:

    fromStream('github')
      .partitionBy(function(ev) {
        if(ev.body.repo) {
          return ev.body.repo.full_name
        }
      })
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
              emit('github-paranoidpushes', "ParanoidPush", {
                first: state.lastPush,
                next: ev
              })
            }
          }
          state.lastPush = ev
        }
      })

What we can actually do here is instead of checking for a difference, simply emit an event for every time we receive a PushEvent notification which happened after another one we already saw:


    fromStream('github')
      .partitionBy(function(ev) {
        if(ev.body.repo) {
          return ev.body.repo.full_name
        }
      })
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
              emit('github-successivepushes', "SuccessivePush", {
                first: state.lastPush,
                next: ev,
                difference: difference
              })
            }
          }
          state.lastPush = ev
        }
      })

This can actually lead to better analysis of our data, for example

*What are the average times between pushes across the different languages*

So, this time we'll partition by language and work out an average time between pushes per language

    fromStream('github-successivepushes')
       .partitionBy(function(ev) {
        if(ev.body.first.body.repo) {
          return ev.body.first.body.repo.language
        }
       })
       .when({
         "$init": function(state, ev) {
           return { total: 0, totaltime: 0 }
         },
         "SuccessivePush": function(state, ev) {
           state.total++
           state.totaltime += ev.body.difference
         }
       })


<div id="scaled"></div>

And there we can see that C# developers take the longest between pushes on average, pretty neat!

<script type="text/javascript" src="/d3.v2.js"></script>
<script type="text/javascript">
  var data = [
  {
    key: "Go",
    state: {
      total: 2,
      totaltime: 144
    }
  },
  {
    key: "R",
    state: {
      total: 2,
      totaltime: 114
    }
  },
  {
    key: "Common Lisp",
    state: {
      total: 2,
      totaltime: 1
    }
  },
  {
    key: "Clojure",
    state: {
      total: 1,
      totaltime: 35
    }
  },
  {
    key: "Rust",
    state: {
      total: 3,
      totaltime: 120
    }
  },
  {
    key: "Groovy",
    state: {
      total: 4,
      totaltime: 16
    }
  },
  {
    key: "Objective-C",
    state: {
      total: 5,
      totaltime: 299
    }
  },
  {
    key: "OpenEdge ABL",
    state: {
      total: 3,
      totaltime: 224
    }
  },
  {
    key: "F#",
    state: {
      total: 2,
      totaltime: 216
    }
  },
  {
    key: "Haskell",
    state: {
      total: 2,
      totaltime: 46
    }
  },
  {
    key: "ActionScript",
    state: {
      total: 1,
      totaltime: 98
    }
  },
  {
    key: "Perl",
    state: {
      total: 8,
      totaltime: 452
    }
  },
  {
    key: "CoffeeScript",
    state: {
      total: 3,
      totaltime: 124
    }
  },
  {
    key: "Erlang",
    state: {
      total: 2,
      totaltime: 143
    }
  },
  {
    key: "VimL",
    state: {
      total: 4,
      totaltime: 256
    }
  },
  {
    key: "PHP",
    state: {
      total: 41,
      totaltime: 2133
    }
  },
  {
    key: "C#",
    state: {
      total: 13,
      totaltime: 1052
    }
  },
  {
    key: "C++",
    state: {
      total: 15,
      totaltime: 363
    }
  },
  {
    key: "Emacs Lisp",
    state: {
      total: 4,
      totaltime: 32
    }
  },
  {
    key: "Python",
    state: {
      total: 54,
      totaltime: 2870
    }
  },
  {
    key: "JavaScript",
    state: {
      total: 158,
      totaltime: 9350
    }
  },
  {
    key: "Lua",
    state: {
      total: 5,
      totaltime: 209
    }
  },
  {
    key: "Matlab",
    state: {
      total: 8,
      totaltime: 538
    }
  },
  {
    key: "C",
    state: {
      total: 45,
      totaltime: 1821
    }
  },
  {
    key: "Java",
    state: {
      total: 103,
      totaltime: 3214
    }
  },
  {
    key: "Shell",
    state: {
      total: 25,
      totaltime: 1123
    }
  },
  {
    key: "Ruby",
    state: {
      total: 39,
      totaltime: 1704
    }
  }
  ]
</script>

<script type="text/javascript">

   var filteredData = []
   for(var i =0 ; i < data.length; i++) {
     if(data[i].state.total >= 10) {
       var datum = data[i]
       datum.state.average = Math.floor(datum.state.totaltime / datum.state.total)
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
