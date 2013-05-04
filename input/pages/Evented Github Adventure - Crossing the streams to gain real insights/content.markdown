It's not enough to just create a [stream of 'events' from correlated actions over time](/entries/evented-github-adventure---temporal-queries,-who-doesnt-trust-their-hardware.html), although that *is* super cool, what we want to do is then use that stream to tell us something interesting about the activity of developers on Github.

Like, given that we have this information about paranoid pushes, how do they stack up as a percentage to pushes overall when broken down by language?

*Problem*: This information is in completely different streams.

Not to worry, this is where the Event Store's ability to re-partition and consume streams in a variety of ways comes to the rescue once more, want to consume the events from two different streams? Not a problem

    fromStreams([ "github", "paranoidpushes" ])
       .when({

       })


First off, let's keep it simple and just find out as a percentage what the paranoid pushes are out of the overal Github stream.


    fromStreams([ "github", "paranoidpushes" ])
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

So, in fact around 7.2% of all pushes to Github happen within 2 minutes of the previous one - that's actually quite high - and my guess is that these are either people rectifying mistakes in previous commits (actually, there's an idea for another projection) or people who are new to git.

Let's generate a result object by language and see what we get

    function getPerLanguageState(state, ev) {
      var language = getLanguageFromEvent(ev)
      var langState = state[language]
      if(!langState) {
        langState = { paranoid: 0, total: 0 }
        state[language] = langState
      }
      return langState
    }
  
    function getLanguageFromEvent(ev) {
      if(ev.body.repo)
        return ev.body.repo.language
      if(ev.body.first)
        return ev.body.first.body.repo.language
    }

    fromStreams([ "github", "paranoidpushes" ])
      .when({
       "$init": function() {
         return {}
       },
       "ParanoidPush": function(state, ev) {
         var langState = getPerLanguageState(state, ev)
         langState.paranoid++
         return state
       },
       "PushEvent": function(state, ev) {
         var langState = getPerLanguageState(state, ev)
         langState.total++
         return state
       }
      })


There is a lot to take in here, but we can see

- I can use custom functions in my projection definition to break down the work
- The custom functions simply work out what language my event is
- The custom functions return a per-language state object
- The projection just adds to the values in that per-language state object

Anyway, the results of this?


<div id="notscaled"></div>

And scaled

<div id="scaled"></div>


So, JavaScript and Lua developers top this one by being the developers who push to their remote repository within a couple of minutes of another push - what's the bet that's just because they're running on a high of caffeine and alcohol and don't even know what time of day it is? *must ship code bro!!*

<script type="text/javascript" src="/d3.v2.js"></script>
<script type="text/javascript">


  var data = {"undefined":{"paranoid":0,"total":197},"Matlab":{"paranoid":0,"total":2},"C":{"paranoid":3,"total":22},"JavaScript":{"paranoid":4,"total":75},"Processing":{"paranoid":1,"total":2},"Python":{"paranoid":5,"total":49},"R":{"paranoid":0,"total":3},"C++":{"paranoid":5,"total":21},"null":{"paranoid":2,"total":31},"Java":{"paranoid":4,"total":50},"Ruby":{"paranoid":3,"total":36},"PHP":{"paranoid":5,"total":38},"Shell":{"paranoid":1,"total":10},"Rust":{"paranoid":0,"total":1},"VimL":{"paranoid":0,"total":3},"Scheme":{"paranoid":0,"total":1},"CoffeeScript":{"paranoid":0,"total":3},"Emacs Lisp":{"paranoid":0,"total":2},"Dart":{"paranoid":0,"total":1},"Clojure":{"paranoid":0,"total":2},"Scala":{"paranoid":1,"total":4},"Perl":{"paranoid":0,"total":1},"Erlang":{"paranoid":0,"total":1},"Factor":{"paranoid":0,"total":1},"Visual Basic":{"paranoid":0,"total":1},"C#":{"paranoid":0,"total":9},"AutoHotkey":{"paranoid":0,"total":1},"ASP":{"paranoid":0,"total":1},"Objective-C":{"paranoid":1,"total":4},"Lua":{"paranoid":0,"total":1},"ActionScript":{"paranoid":0,"total":2},"Parrot":{"paranoid":0,"total":1},"Groovy":{"paranoid":0,"total":1},"Puppet":{"paranoid":0,"total":1}}

   var filteredData = []
   for(var i in data) {
     if(data[i].total >= 5) {
       var datum = data[i]
       datum.state = {
        paranoid: data[i].paranoid,
        total: data[i].total,
        percentage: Math.floor((data[i].paranoid / data[i].total) * 10000) / 100
       }
       datum.key = i
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

