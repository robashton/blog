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

My old version of this chart had C# developers at the right hand of the chart, but more data showed this was not the case (which is a shame because I was going to make a snarky remark about build times)

As it happens, the ML folk are the folk who push often and furiously - I can't explain why this is but this is from over 3000 pushes and is quite representative. (Anybody who works in standard ML care to comment? I didn't even know this was a popular thing...)

VimL being on the right hand side makes a lot of sense - how often do we change our configs after all? :)


<script type="text/javascript" src="/d3.v2.js"></script>
<script type="text/javascript">
var data = {"Java":{"count":253580,"totaldifference":38238330856},"JavaScript":{"count":377205,"totaldifference":56632429808},"PHP":{"count":153121,"totaldifference":24771723876},"C":{"count":102470,"totaldifference":17105126095},"Python":{"count":166696,"totaldifference":28787843794},"null":{"count":172572,"totaldifference":14480451092},"Shell":{"count":55989,"totaldifference":10294378504},"Lua":{"count":7462,"totaldifference":1435117732},"C++":{"count":115765,"totaldifference":16678557230},"Ruby":{"count":176623,"totaldifference":27952013780},"ColdFusion":{"count":1079,"totaldifference":112788357},"Objective-C":{"count":26592,"totaldifference":5090102450},"C#":{"count":39300,"totaldifference":6771578404},"CoffeeScript":{"count":12882,"totaldifference":1958762763},"FORTRAN":{"count":1079,"totaldifference":257931768},"ActionScript":{"count":3559,"totaldifference":658819864},"Assembly":{"count":1410,"totaldifference":270141323},"Perl":{"count":19860,"totaldifference":3899985663},"R":{"count":5853,"totaldifference":1247020443},"Clojure":{"count":6951,"totaldifference":1281534272},"Arduino":{"count":1636,"totaldifference":378845394},"Go":{"count":8604,"totaldifference":1303815011},"Haskell":{"count":8268,"totaldifference":1718963984},"OpenEdge ABL":{"count":280,"totaldifference":49990984},"Erlang":{"count":4511,"totaldifference":838173598},"Julia":{"count":1390,"totaldifference":210414747},"VimL":{"count":18202,"totaldifference":6334893565},"Tcl":{"count":361,"totaldifference":107774886},"Common Lisp":{"count":1608,"totaldifference":319061186},"Rust":{"count":1753,"totaldifference":190856284},"Scala":{"count":11826,"totaldifference":2331705101},"Groovy":{"count":4365,"totaldifference":834385794},"Puppet":{"count":4504,"totaldifference":744092109},"Emacs Lisp":{"count":8986,"totaldifference":2641763583},"ASP":{"count":1885,"totaldifference":245957381},"Verilog":{"count":1180,"totaldifference":166834193},"Visual Basic":{"count":1476,"totaldifference":322967626},"AutoHotkey":{"count":197,"totaldifference":58191551},"Standard ML":{"count":3119,"totaldifference":62683634},"Matlab":{"count":7773,"totaldifference":978243889},"F#":{"count":961,"totaldifference":184019680},"Vala":{"count":356,"totaldifference":68369602},"Scheme":{"count":1781,"totaldifference":307656750},"D":{"count":1358,"totaldifference":347429358},"Racket":{"count":1249,"totaldifference":235521807},"OCaml":{"count":2135,"totaldifference":423226057},"Prolog":{"count":888,"totaldifference":198471986},"Dart":{"count":1978,"totaldifference":502985423},"ooc":{"count":179,"totaldifference":8666096},"HaXe":{"count":308,"totaldifference":64412989},"PowerShell":{"count":1015,"totaldifference":207512933},"Turing":{"count":20,"totaldifference":5009659},"Logtalk":{"count":71,"totaldifference":4315767},"Smalltalk":{"count":436,"totaldifference":59346112},"XQuery":{"count":102,"totaldifference":35513771},"Haxe":{"count":1045,"totaldifference":176098167},"Factor":{"count":491,"totaldifference":11389691},"Delphi":{"count":1359,"totaldifference":232268974},"VHDL":{"count":739,"totaldifference":141829947},"Ada":{"count":284,"totaldifference":42642935},"Kotlin":{"count":98,"totaldifference":19861112},"Rebol":{"count":32,"totaldifference":9488848},"Elixir":{"count":277,"totaldifference":43585554},"Bro":{"count":2,"totaldifference":389},"Coq":{"count":490,"totaldifference":76134746},"Parrot":{"count":71,"totaldifference":6472209},"DCPU-16 ASM":{"count":48,"totaldifference":9578220},"Eiffel":{"count":433,"totaldifference":47667533},"Objective-J":{"count":181,"totaldifference":44845404},"Scilab":{"count":69,"totaldifference":19688536},"Apex":{"count":152,"totaldifference":27645682},"Fancy":{"count":4,"totaldifference":3523197},"AppleScript":{"count":122,"totaldifference":28262738},"Ceylon":{"count":36,"totaldifference":14033292},"Pure Data":{"count":111,"totaldifference":26227613},"Io":{"count":70,"totaldifference":13317185},"Lasso":{"count":517,"totaldifference":53654351},"TypeScript":{"count":1922,"totaldifference":312329074},"XML":{"count":2200,"totaldifference":510073123},"LiveScript":{"count":249,"totaldifference":34549381},"Xtend":{"count":53,"totaldifference":12040358},"SuperCollider":{"count":209,"totaldifference":45851945},"Ioke":{"count":11,"totaldifference":2397847},"Dylan":{"count":81,"totaldifference":27279547},"Awk":{"count":90,"totaldifference":20843043},"Boo":{"count":50,"totaldifference":17946733},"Monkey":{"count":30,"totaldifference":7011348},"Nimrod":{"count":64,"totaldifference":14514326},"Logos":{"count":2398,"totaldifference":407157116},"Mirah":{"count":1,"totaldifference":3521},"CLIPS":{"count":37,"totaldifference":7394334},"Nemerle":{"count":147,"totaldifference":17242985},"DOT":{"count":671,"totaldifference":113091927},"Gosu":{"count":64,"totaldifference":10434170},"Augeas":{"count":14,"totaldifference":1169525},"Processing":{"count":1188,"totaldifference":206758704},"Nu":{"count":68,"totaldifference":3581684},"PogoScript":{"count":14,"totaldifference":3207543},"eC":{"count":5,"totaldifference":2925691},"Forth":{"count":7,"totaldifference":2797176},"Pike":{"count":6,"totaldifference":3902037},"TXL":{"count":1,"totaldifference":113769},"Fantom":{"count":18,"totaldifference":2346866},"Ecl":{"count":17,"totaldifference":2004560},"MoonScript":{"count":46,"totaldifference":6038229},"Ragel in Ruby Host":{"count":18,"totaldifference":1364124},"Rouge":{"count":1,"totaldifference":1833},"Opa":{"count":1,"totaldifference":648},"Arc":{"count":15,"totaldifference":5975508}}
</script>

<script type="text/javascript">

   var filteredData = []
   for(var i in data) {
     if(i === 'null') continue
     if(data[i].count >= 2500) {
       var datum = data[i]
       datum.state = datum
       datum.key = i
       datum.state.average = Math.floor(datum.state.totaldifference / datum.state.count) / (60 * 60)
       filteredData.push(datum)
     }
   }

   if(filteredData.sort)
     filteredData = filteredData.sort(function(a,b) { return a.state.average - b.state.average })

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
      .text(parseInt(max, 10) + " hours")

   svg.append("text")
      .attr("fill", '#000')
      .attr("x", 710)
      .attr("y", 350)
      .text(0 + " hours")

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

