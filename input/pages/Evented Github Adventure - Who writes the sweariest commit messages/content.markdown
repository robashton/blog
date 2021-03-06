Okay, so now I've ran all my projections over all that crazy data and have some results to show!

- [Introduction to the EventStore](/entries/playing-with-the-eventstore.html)
- [Pushing data into the EventStore](/entries/pushing-data-into-streams-in-the-eventstore.html)
- [Projections in the EventStore](/entries/basic-projections-in-the-eventstore.html)
- [Re-partitioning streams in the EventStore](/entries/re-partitioning-streams-in-the-event-store-for-better-projections.html)
- [Creating a projection per stream](/entries/creating-a-projection-per-stream-in-the-eventstore.html)
- [Pumping data from Github into the EventStore](/entries/less-abstract,-pumping-data-from-github-into-the-eventstore.html)
- [Emitting new events from a projection](/entries/evented-github-adventure---emitting-commits-as-their-own-events.html)
- [Event store database storage](/entries/evented-github-adventure---database-storage-and-backing-up.html)
- Who is the sweariest of them all?


We now have a [stream for the commits](/entries/evented-github-adventure---emitting-commits-as-their-own-events.html) inside Github, and we have information about the repos associated with those commits, now how about asking a question about those commits.

For reference, there are about 20 million commit messages in my event store, so I have more than enough data for this to be statistically relevant!

*"Oh Github Github, in the cloud, who is the sweariest developer out loud?"*

Well, this is the kind of thing we might do outside the store (after re-partitioning per-language inside the store), but I haven't got a secondary store so I'm just going to build up a view model for my charting library directly inside the event store (using the commit events I made)

    var swearwords = [ "poop", "arse", "sugarlumps" ] // Changed to protect the innocent

    fromStream('github-commits')
      .when({
        "$init": function(state, ev) {
          return { }
        },
        "Commit": function(state, ev) {
          var language = ev.body.repo.language

          if(!state[language])
            state[language] = { count: 0, total: 0 }

          var languageState = state[language]
          languageState.total += 1

          for(var i = 0 ; i < swearwords.length; i++) {
            var curse = swearwords[i]
            if(ev.body.commit.message.indexOf(curse) >= 0)
              languageState.count += 1
          }
          return state
        }
      })

And the results?

Well, I can go to 

*/projection/curses/state*

And get a big pile of JSON, which looks a bit like this

    {
      "ASP": { total: 1, curses: 200 },
      "OpenEdge ABL": { total: 2, curses: 0 },
      "Julia": { total: 11, curses: 0 }
    }

Plugging this into d3, and filtering out the items without enough entries (5000 events), we get 

<div id="graph"></div>
<script type="text/javascript" src="/d3.v2.js"></script>

<script type="text/javascript">

  var svg = d3.select("#graph").append("svg")
          .attr("width", 640)
          .attr("height", 480)


var data = {"Lua":{"count":365,"total":14410},"Java":{"count":24321,"total":478695},"C":{"count":10997,"total":247049},"AutoHotkey":{"count":23,"total":376},"null":{"count":5493,"total":317226},"C++":{"count":11779,"total":231490},"Ruby":{"count":16148,"total":364129},"undefined":{"count":192,"total":4821},"Python":{"count":13667,"total":351107},"JavaScript":{"count":18063,"total":651759},"PHP":{"count":12383,"total":290428},"Emacs Lisp":{"count":420,"total":19623},"Objective-C":{"count":1828,"total":53214},"Shell":{"count":1976,"total":96757},"Erlang":{"count":312,"total":10949},"ColdFusion":{"count":54,"total":2065},"CoffeeScript":{"count":782,"total":25129},"C#":{"count":3706,"total":74267},"Groovy":{"count":373,"total":7892},"Go":{"count":363,"total":15837},"Rust":{"count":372,"total":6349},"Arduino":{"count":54,"total":2817},"Standard ML":{"count":20,"total":949},"R":{"count":316,"total":9411},"Perl":{"count":1764,"total":42810},"Haskell":{"count":417,"total":16458},"Common Lisp":{"count":131,"total":3284},"Verilog":{"count":69,"total":1930},"Haxe":{"count":92,"total":1672},"ooc":{"count":3,"total":208},"VimL":{"count":515,"total":34469},"Scala":{"count":1592,"total":26538},"Clojure":{"count":352,"total":14451},"FORTRAN":{"count":56,"total":2040},"ActionScript":{"count":379,"total":6599},"Assembly":{"count":108,"total":2481},"OCaml":{"count":137,"total":5293},"ASP":{"count":57,"total":2555},"Puppet":{"count":467,"total":8035},"OpenEdge ABL":{"count":24,"total":490},"HaXe":{"count":44,"total":894},"Julia":{"count":125,"total":3937},"D":{"count":163,"total":3049},"Tcl":{"count":28,"total":1038},"Visual Basic":{"count":69,"total":2225},"Racket":{"count":152,"total":3041},"Delphi":{"count":69,"total":2300},"Matlab":{"count":256,"total":10542},"Dart":{"count":112,"total":3689},"Coq":{"count":10,"total":819},"Vala":{"count":9,"total":565},"Gosu":{"count":0,"total":92},"F#":{"count":66,"total":2176},"Logtalk":{"count":4,"total":67},"Scheme":{"count":65,"total":2930},"Prolog":{"count":63,"total":1440},"Augeas":{"count":2,"total":24},"PowerShell":{"count":82,"total":1644},"VHDL":{"count":104,"total":1747},"Turing":{"count":3,"total":24},"DCPU-16 ASM":{"count":1,"total":92},"Smalltalk":{"count":87,"total":863},"XQuery":{"count":11,"total":227},"Dylan":{"count":20,"total":295},"Objective-J":{"count":31,"total":635},"Factor":{"count":55,"total":1173},"Ada":{"count":11,"total":353},"Kotlin":{"count":27,"total":314},"Rebol":{"count":4,"total":54},"Io":{"count":0,"total":135},"Nemerle":{"count":5,"total":224},"Elixir":{"count":6,"total":577},"Eiffel":{"count":70,"total":898},"Boo":{"count":5,"total":118},"SuperCollider":{"count":28,"total":312},"AppleScript":{"count":2,"total":228},"Parrot":{"count":11,"total":248},"Scilab":{"count":10,"total":109},"Apex":{"count":17,"total":282},"Ceylon":{"count":2,"total":92},"Bro":{"count":1,"total":8},"Pure Data":{"count":4,"total":213},"Max":{"count":0,"total":1},"Fancy":{"count":0,"total":5},"Lasso":{"count":40,"total":1222},"TypeScript":{"count":251,"total":5617},"XML":{"count":100,"total":4026},"LiveScript":{"count":17,"total":459},"Awk":{"count":1,"total":131},"Mirah":{"count":0,"total":5},"Xtend":{"count":9,"total":69},"Ioke":{"count":0,"total":12},"Monkey":{"count":1,"total":55},"Logos":{"count":156,"total":3502},"eC":{"count":0,"total":9},"Nimrod":{"count":4,"total":203},"CLIPS":{"count":1,"total":62},"Arc":{"count":0,"total":14},"DOT":{"count":13,"total":999},"Rouge":{"count":0,"total":3},"Ecl":{"count":0,"total":10},"Processing":{"count":61,"total":1679},"Nu":{"count":6,"total":102},"Forth":{"count":0,"total":34},"PogoScript":{"count":1,"total":20},"Pike":{"count":5,"total":37},"TXL":{"count":0,"total":2},"Fantom":{"count":0,"total":25},"MoonScript":{"count":3,"total":51},"Ragel in Ruby Host":{"count":1,"total":23},"Opa":{"count":2,"total":2},"ABAP":{"count":0,"total":1}}

   </script>

   <script type="text/javascript">

   var filteredData = []
   for(var i in data) {
     if(i === 'null') continue
     if(data[i].total >= 5000) {
       
       var datum = data[i]
       datum.percentage = Math.floor((datum.count / datum.total) * 10000) / 100
       datum.swears = datum.count
       datum.language = i
       filteredData.push(datum)
     }
   }

   if(filteredData.sort) {
    filteredData = filteredData.sort(function(a, b) { return a.percentage - b.percentage })
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

**% of commit messages containing curse words**

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


I'll leave you to draw your own conclusions about this chart, but I can't say that it comes as a huge surprise judging from the various developers on *my* Twitter feed ;-)

Scala developers are ducking *filthy*, but the lisp programmers probably save their curse words for Emacs rather than the language they're using. *Seems legit.*

Projections are a great way to analyse streams to generate knowledge about what is going on, of course simply doing aggregations over data over time is something we can achieve in most systems, in the next entry we'll look at something more interesting.

