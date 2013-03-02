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

   var data = 
   [
   {
     key: "Common Lisp",
     state: {
       total: 8,
       curses: 0
     }
   },
   {
     key: "FORTRAN",
     state: {
       total: 1,
       curses: 0
     }
   },
   {
     key: "ooc",
     state: {
       total: 1,
       curses: 0
     }
   },
   {
     key: "Vala",
     state: {
       total: 20,
       curses: 3
     }
   },
   {
     key: "ColdFusion",
     state: {
       total: 3,
       curses: 0
     }
   },
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
       total: 7,
       curses: 0
     }
   },
   {
     key: "Julia",
     state: {
       total: 12,
       curses: 0
     }
   },
   {
     key: "Puppet",
     state: {
       total: 6,
       curses: 0
     }
   },
   {
     key: "Factor",
     state: {
       total: 21,
       curses: 0
     }
   },
   {
     key: "R",
     state: {
       total: 14,
       curses: 0
     }
   },
   {
     key: "HaXe",
     state: {
       total: 1,
       curses: 0
     }
   },
   {
     key: "Racket",
     state: {
       total: 8,
       curses: 2
     }
   },
   {
     key: "Prolog",
     state: {
       total: 4,
       curses: 0
     }
   },
   {
     key: "PowerShell",
     state: {
       total: 8,
       curses: 2
     }
   },
   {
     key: "Verilog",
     state: {
       total: 2,
       curses: 1
     }
   },
   {
     key: "F#",
     state: {
       total: 19,
       curses: 4
     }
   },
   {
     key: "Rust",
     state: {
       total: 60,
       curses: 3
     }
   },
   {
     key: "Nemerle",
     state: {
       total: 2,
       curses: 0
     }
   },
   {
     key: "Scheme",
     state: {
       total: 10,
       curses: 1
     }
   },
   {
     key: "D",
     state: {
       total: 3,
       curses: 0
     }
   },
   {
     key: "Ceylon",
     state: {
       total: 1,
       curses: 0
     }
   },
   {
     key: "Arduino",
     state: {
       total: 3,
       curses: 0
     }
   },
   {
     key: "OCaml",
     state: {
       total: 7,
       curses: 0
     }
   },
   {
     key: "Assembly",
     state: {
       total: 7,
       curses: 0
     }
   },
   {
     key: "Delphi",
     state: {
       total: 6,
       curses: 0
     }
   },
   {
     key: "Dart",
     state: {
       total: 15,
       curses: 0
     }
   },
   {
     key: "ActionScript",
     state: {
       total: 31,
       curses: 2
     }
   },
   {
     key: "Erlang",
     state: {
       total: 10,
       curses: 0
     }
   },
   {
     key: "Clojure",
     state: {
       total: 24,
       curses: 0
     }
   },
   {
     key: "Groovy",
     state: {
       total: 24,
       curses: 1
     }
   },
   {
     key: "Haskell",
     state: {
       total: 97,
       curses: 1
     }
   },
   {
     key: "Visual Basic",
     state: {
       total: 9,
       curses: 0
     }
   },
   {
     key: "Emacs Lisp",
     state: {
       total: 75,
       curses: 0
     }
   },
   {
     key: "Go",
     state: {
       total: 22,
       curses: 0
     }
   },
   {
     key: "VimL",
     state: {
       total: 96,
       curses: 2
     }
   },
   {
     key: "Scala",
     state: {
       total: 60,
       curses: 4
     }
   },
   {
     key: "Ada",
     state: {
       total: 2,
       curses: 1
     }
   },
   {
     key: "Lua",
     state: {
       total: 38,
       curses: 1
     }
   },
   {
     key: "Perl",
     state: {
       total: 167,
       curses: 3
     }
   },
   {
     key: "Objective-C",
     state: {
       total: 128,
       curses: 5
     }
   },
   {
     key: "Matlab",
     state: {
       total: 21,
       curses: 0
     }
   },
   {
     key: "Shell",
     state: {
       total: 320,
       curses: 16
     }
   },
   {
     key: "C#",
     state: {
       total: 196,
       curses: 3
     }
   },
   {
     key: "Ruby",
     state: {
       total: 794,
       curses: 32
     }
   },
   {
     key: "PHP",
     state: {
       total: 745,
       curses: 43
     }
   },
   {
     key: "CoffeeScript",
     state: {
       total: 58,
       curses: 2
     }
   },
   {
     key: "C",
     state: {
       total: 608,
       curses: 28
     }
   },
   {
     key: "JavaScript",
     state: {
       total: 1409,
       curses: 40
     }
   },
   {
     key: "Java",
     state: {
       total: 905,
       curses: 36
     }
   },
   {
     key: "C++",
     state: {
       total: 418,
       curses: 14
     }
   },
   {
     key: "Python",
     state: {
       total: 902,
       curses: 36
     }
   }
   ]

   </script>

   <script type="text/javascript">

   var filteredData = []
   for(var i =0 ; i < data.length; i++) {
     if(data[i].state.total >= 50)
       filteredData.push(data[i])
   }

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

    svg.selectAll(".curse")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "curse")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - scale(d.state.curses); })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return scale(d.state.curses) })

</script>

Actually, let's normalise this for the lols and see who is actually the sweariest, normalised from about 0% to 1% (the majority of developers are quite clean about things ;) )

<div id="normalised"></div>
<script type="text/javascript">

  var svg = d3.select("#normalised").append("svg")
          .attr("width", 800)
          .attr("height", 480)


   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.state.curses })])
     .range([0, 1]);

   var maxPercentage = scale( d3.max(filteredData, function(d) { return d.state.curses }) );

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

    svg.selectAll(".curse")
     .data(filteredData)
     .enter()
       .append("rect")
         .attr("class", "curse")
         .attr("fill", '#AAF')
         .attr("x", function(d, i) { return i * (640 / filteredData.length)})
         .attr("y", function(d, i) { return 370 - (280 * scale(d.state.curses)) })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return 280 * scale(d.state.curses) })

</script>


So... turns out that C++ developers are  quite civil and PHP developers are fucking angry - who can blame them?

Projections are a great way to analyse streams to generate knowledge about what is going on, of course simply doing 

