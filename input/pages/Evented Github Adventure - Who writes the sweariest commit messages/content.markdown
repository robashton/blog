We now have a [stream for the commits](/entries/evented-github-adventure---emitting-commits-as-their-own-events.html) inside Github, and we have information about the repos associated with those commits, now how about asking a question about those commits.

*"Oh Github Github, in the cloud, who is the sweariest developer out loud?"*

Well, we want to do this per language so it makes sense to re-partition the commits by language and build up a projection for each of these partitions:

    var swearwords = [ "poop", "arse", "sugarlumps" ] // Changed to protect the innocent

    fromStream('github-commits')
      .partitionBy(function(ev) {
        if(ev.body.repo)
          return ev.body.repo.language
      })
      .when({
        "$init": function(state, ev) {
          return { total: 0, curses: 0 }
        },
        "Commit": function(state, ev) {
          state.total += 1

          for(var i = 0 ; i < swearwords.length; i++) {
            var curse = swearwords[i]
            if(ev.body.commit.message.indexOf(curse) >= 0)
              state.curses += 1
          }
        }
      })

And the results?

Well, I can go to 

*/projections/curses/all-states*

And get a big pile of JSON, which looks a bit like this

    [
      {
        key: "ASP",
        state: {
          total: 1,
          curses: 0
        }
      },
      {
        key: "OpenEdge ABL",
        state: {
          total: 2,
          curses: 0
        }
      },
      {
        key: "Julia",
        state: {
          total: 11,
          curses: 0
        }
      }
    ]


Plugging this into d3, and filtering out the items without enough entries, we get 

<div id="graph"></div>
<script type="text/javascript" src="/d3.v2.js"></script>

<script type="text/javascript">

  var svg = d3.select("#graph").append("svg")
          .attr("width", 640)
          .attr("height", 480)

   var data = {"JavaScript":{"total":13173,"swears":377},"C":{"total":6300,"swears":309},"Shell":{"total":2260,"swears":45},"Ruby":{"total":7121,"swears":322},"Python":{"total":8519,"swears":282},"null":{"total":6267,"swears":109},"Java":{"total":10871,"swears":572},"R":{"total":179,"swears":4},"PHP":{"total":7349,"swears":329},"Perl":{"total":928,"swears":56},"C++":{"total":4443,"swears":269},"C#":{"total":1735,"swears":70},"Common Lisp":{"total":109,"swears":2},"ColdFusion":{"total":23,"swears":2},"CoffeeScript":{"total":415,"swears":11},"Emacs Lisp":{"total":397,"swears":10},"Lua":{"total":284,"swears":12},"Objective-C":{"total":1490,"swears":42},"Verilog":{"total":21,"swears":0},"Erlang":{"total":382,"swears":6},"Delphi":{"total":40,"swears":0},"Haskell":{"total":389,"swears":8},"ooc":{"total":5,"swears":0},"VimL":{"total":844,"swears":9},"FORTRAN":{"total":55,"swears":0},"ActionScript":{"total":108,"swears":4},"Assembly":{"total":23,"swears":1},"Clojure":{"total":231,"swears":11},"Matlab":{"total":280,"swears":0},"Scheme":{"total":25,"swears":0},"Julia":{"total":108,"swears":0},"Racket":{"total":68,"swears":0},"Go":{"total":284,"swears":4},"Scala":{"total":682,"swears":46},"ASP":{"total":50,"swears":0},"F#":{"total":15,"swears":0},"Ada":{"total":103,"swears":4},"SuperCollider":{"total":3,"swears":0},"PowerShell":{"total":17,"swears":1},"Groovy":{"total":219,"swears":3},"Haxe":{"total":13,"swears":0},"Kotlin":{"total":6,"swears":1},"VHDL":{"total":30,"swears":1},"OpenEdge ABL":{"total":11,"swears":1},"Rust":{"total":122,"swears":5},"Puppet":{"total":144,"swears":4},"AutoHotkey":{"total":7,"swears":0},"Visual Basic":{"total":43,"swears":0},"Dart":{"total":25,"swears":0},"D":{"total":47,"swears":5},"Arduino":{"total":63,"swears":1},"Tcl":{"total":7,"swears":0},"Apex":{"total":14,"swears":1},"Smalltalk":{"total":16,"swears":2},"OCaml":{"total":149,"swears":0},"Coq":{"total":5,"swears":0},"Standard ML":{"total":11,"swears":0},"Rebol":{"total":15,"swears":1},"HaXe":{"total":12,"swears":0},"Prolog":{"total":33,"swears":2},"Nemerle":{"total":5,"swears":0},"Io":{"total":6,"swears":0},"XQuery":{"total":17,"swears":2},"Ioke":{"total":1,"swears":0},"Eiffel":{"total":23,"swears":3},"Elixir":{"total":12,"swears":0},"Gosu":{"total":4,"swears":0},"Vala":{"total":3,"swears":0},"undefined":{"total":113,"swears":0},"AppleScript":{"total":3,"swears":0},"Logtalk":{"total":5,"swears":0},"Parrot":{"total":8,"swears":0},"Fantom":{"total":1,"swears":0},"Lasso":{"total":20,"swears":1},"TypeScript":{"total":22,"swears":1},"LiveScript":{"total":1,"swears":0},"XML":{"total":8,"swears":0},"Factor":{"total":4,"swears":0}}

   </script>

   <script type="text/javascript">

   var filteredData = []
   for(var i in data) {
     if(data[i].total >= 50) {
       var datum = data[i]
       datum.percentage = Math.floor((datum.swears / datum.total) * 10000) / 100
       datum.language = i
       filteredData.push(datum)
     }
   }

   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.total })])
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
        .text(function(d) { return d.language })

   svg.selectAll(".total")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "total")
         .attr("fill", '#00A')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - scale(d.total); })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return scale(d.total) })

    svg.selectAll(".curse")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "curse")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - scale(d.swears); })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return scale(d.swears) })

</script>

Actually, let's normalise this for the lols and see who is actually the sweariest, normalised from about 0% to 7% (the majority of developers are quite clean about things ;) )

<div id="normalised"></div>
<script type="text/javascript">

  var svg = d3.select("#normalised").append("svg")
          .attr("width", 800)
          .attr("height", 480)

   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.percentage })])
     .range([0, 1]);

   var maxPercentage = d3.max(filteredData, function(d) { return d.percentage });

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
        .text(function(d) { return d.language })

    svg.selectAll(".curse")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "curse")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - (280 * scale(d.percentage)) })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return 280 * scale(d.percentage) })

</script>


So... turns out that C# developers are quite mild-mannered and PHP/Scala developers are fucking angry - and who can blame them?

*note: There isn't really enough data to draw this sort of result, but let's not let that stop us making outrageous statements*

Projections are a great way to analyse streams to generate knowledge about what is going on, of course simply doing aggregations over data over time is something we can achieve in most systems, in the next entry we'll look at something more interesting.

