It's not enough to just create a [stream of 'events' from correlated actions over time](/entries/evented-github-adventure---temporal-queries,-who-doesnt-trust-their-hardware.html), although that *is* super cool, what we want to do is then use that stream to tell us something interesting about the activity of developers on Github.

Like, given that we have this information about paranoid pushes, how do they stack up as a percentage to pushes overall when broken down by language?

*Problem*: This information is in completely different streams.

Not to worry, this is where the Event Store's ability to re-partition and consume streams in a variety of ways comes to the rescue once more, want to consume the events from two different streams? Not a problem

    fromStreams([ "github", "github-paranoidpushes" ])
       .when({

       })


First off, let's keep it simple and just find out as a percentage what the paranoid pushes are out of the overal github stream.


    fromStreams([ "github", "github-paranoidpushes" ])
       .when({
         "$init": function() {
           return { paranoid: 0, total: 0 }
         },
         "ParanoidPush": function(state, ev) {
           state.paranoid++
         },
         "PushEvent": function(state, ev) {
           state.total++
         }
       })

So yeah, er - that was stupidly easy, I didn't even have to think about that one, we just get all the events from those streams as a unified collection and were able to generate stats from these.

What does the result look like?

    {"paranoid":598,"total":8226}

So, in fact around 7.2% of all pushes to Github happen within 2 minutes of the previous one - that's actually quite high - and my guess is that these are either people rectifying mistakes in previous commits (actually, there's an idea for another projection)

Let's partition by language and see what we get

    fromStreams([ "github", "github-paranoidpushes" ])
      .partitionBy(function(ev) {
        if(ev.body.repo)
          return ev.body.repo.language
        if(ev.body.first)
          return ev.body.first.body.repo.language
      })
      .when({
       "$init": function() {
         return { paranoid: 0, total: 0 }
       },
       "ParanoidPush": function(state, ev) {
         state.paranoid++
       },
       "PushEvent": function(state, ev) {
         state.total++
       }
      })


Slightly more complicated because we have two different kinds of event, we have to check in a couple of places for language, and because I didn't think ahead I'm shoving the whole event into my 'first' and 'last' property so the law of demeter be damned. (If we had a more complicated scenario, we could check explicitly for the event type and be more specific.

Anyway, the results of this?


<div id="notscaled"></div>

And scaled

<div id="scaled"></div>


So, JavaScript and Lua developers top this one - what's the bet that's just because they're running on a high of caffeine and alcohol and don't even know what time of day it is?


*note: Once again, probably not enough data to draw real conclusions from*

Anyway, next let's do a bit of analysis on seeing if these are "accidents" or not.

<script type="text/javascript" src="/d3.v2.js"></script>
<script type="text/javascript">
  var data = [
  {
    key: "VHDL",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "Apex",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "AutoHotkey",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "Tcl",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "Smalltalk",
    state: {
      paranoid: 0,
      total: 3
    }
  },
  {
    key: "Common Lisp",
    state: {
      paranoid: 2,
      total: 10
    }
  },
  {
    key: "FORTRAN",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "ooc",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "Vala",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "ColdFusion",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "ASP",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "OpenEdge ABL",
    state: {
      paranoid: 3,
      total: 14
    }
  },
  {
    key: "Julia",
    state: {
      paranoid: 0,
      total: 3
    }
  },
  {
    key: "Puppet",
    state: {
      paranoid: 0,
      total: 11
    }
  },
  {
    key: "Factor",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "R",
    state: {
      paranoid: 1,
      total: 17
    }
  },
  {
    key: "HaXe",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "Racket",
    state: {
      paranoid: 0,
      total: 9
    }
  },
  {
    key: "Prolog",
    state: {
      paranoid: 0,
      total: 7
    }
  },
  {
    key: "PowerShell",
    state: {
      paranoid: 0,
      total: 5
    }
  },
  {
    key: "Verilog",
    state: {
      paranoid: 0,
      total: 5
    }
  },
  {
    key: "F#",
    state: {
      paranoid: 2,
      total: 15
    }
  },
  {
    key: "Rust",
    state: {
      paranoid: 3,
      total: 15
    }
  },
  {
    key: "Nemerle",
    state: {
      paranoid: 0,
      total: 2
    }
  },
  {
    key: "Scheme",
    state: {
      paranoid: 0,
      total: 11
    }
  },
  {
    key: "D",
    state: {
      paranoid: 0,
      total: 5
    }
  },
  {
    key: "Ceylon",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "Arduino",
    state: {
      paranoid: 0,
      total: 7
    }
  },
  {
    key: "OCaml",
    state: {
      paranoid: 0,
      total: 7
    }
  },
  {
    key: "Assembly",
    state: {
      paranoid: 0,
      total: 13
    }
  },
  {
    key: "Delphi",
    state: {
      paranoid: 0,
      total: 11
    }
  },
  {
    key: "Dart",
    state: {
      paranoid: 0,
      total: 5
    }
  },
  {
    key: "ActionScript",
    state: {
      paranoid: 1,
      total: 15
    }
  },
  {
    key: "Erlang",
    state: {
      paranoid: 2,
      total: 21
    }
  },
  {
    key: "Clojure",
    state: {
      paranoid: 1,
      total: 29
    }
  },
  {
    key: "Groovy",
    state: {
      paranoid: 3,
      total: 28
    }
  },
  {
    key: "Haskell",
    state: {
      paranoid: 2,
      total: 44
    }
  },
  {
    key: "Visual Basic",
    state: {
      paranoid: 0,
      total: 8
    }
  },
  {
    key: "Emacs Lisp",
    state: {
      paranoid: 4,
      total: 60
    }
  },
  {
    key: "Go",
    state: {
      paranoid: 0,
      total: 33
    }
  },
  {
    key: "VimL",
    state: {
      paranoid: 4,
      total: 105
    }
  },
  {
    key: "Scala",
    state: {
      paranoid: 0,
      total: 69
    }
  },
  {
    key: "Ada",
    state: {
      paranoid: 0,
      total: 1
    }
  },
  {
    key: "Lua",
    state: {
      paranoid: 5,
      total: 58
    }
  },
  {
    key: "Perl",
    state: {
      paranoid: 7,
      total: 113
    }
  },
  {
    key: "Objective-C",
    state: {
      paranoid: 4,
      total: 144
    }
  },
  {
    key: "Matlab",
    state: {
      paranoid: 8,
      total: 30
    }
  },
  {
    key: "Shell",
    state: {
      paranoid: 23,
      total: 325
    }
  },
  {
    key: "C#",
    state: {
      paranoid: 11,
      total: 219
    }
  },
  {
    key: "Ruby",
    state: {
      paranoid: 34,
      total: 797
    }
  },
  {
    key: "PHP",
    state: {
      paranoid: 36,
      total: 712
    }
  },
  {
    key: "CoffeeScript",
    state: {
      paranoid: 3,
      total: 63
    }
  },
  {
    key: "C",
    state: {
      paranoid: 34,
      total: 536
    }
  },
  {
    key: "JavaScript",
    state: {
      paranoid: 128,
      total: 1676
    }
  },
  {
    key: "Java",
    state: {
      paranoid: 83,
      total: 988
    }
  },
  {
    key: "C++",
    state: {
      paranoid: 13,
      total: 420
    }
  },
  {
    key: "Python",
    state: {
      paranoid: 47,
      total: 767
    }
  }
  ]

   var filteredData = []
   for(var i =0 ; i < data.length; i++) {
     if(data[i].state.total >= 50) {
       var datum = data[i]
       datum.state.percentage = Math.floor((datum.state.paranoid / datum.state.total) * 10000) / 100
       filteredData.push(datum)
     }
   }
</script>

<script type="text/javascript">

  var svg = d3.select("#notscaled").append("svg")
          .attr("width", 640)
          .attr("height", 480)


   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.state.total })])
     .range([0, 280]);

   svg.selectAll("text")
      .data(filteredData)
      .enter()
        .append("text")
        .attr("transform", function(d, i) { 
          var transform = "translate(" + i * (640 / filteredData.length) + "," + 380 + ") "
          transform += "rotate(75) "
          return transform
        })
        .attr("x", 0)
        .attr("y", 0)
        .text(function(d) { return d.key })

   svg.selectAll(".total")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "total")
         .attr("fill", '#00A')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - scale(d.state.total); })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return scale(d.state.total) })

    svg.selectAll(".paranoid")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "paranoid")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - scale(d.state.paranoid); })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return scale(d.state.paranoid) })

</script>

<script type="text/javascript">

  var svg = d3.select("#scaled").append("svg")
          .attr("width", 800)
          .attr("height", 480)

   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.state.percentage })])
     .range([0, 1]);

   var maxPercentage = d3.max(filteredData, function(d) { return d.state.percentage });

   svg.append("text")
      .attr("fill", '#000')
      .attr("x", 710)
      .attr("y", 60)
      .text(maxPercentage + "%")

   svg.append("text")
      .attr("fill", '#000')
      .attr("x", 710)
      .attr("y", 350)
      .text(0 + "%")

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

    svg.selectAll(".paranoid")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "paranoid")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - (280 * scale(d.state.percentage)) })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return 280 * scale(d.state.percentage) })
</script>
