Okay, back to more practical things now we've covered how easy [temporal queries are with the event store](/entries/evented-github-adventure---temporal-averages.html).

Ever wondered how happy developers from different languages were? Well, let's find out 

First off, I downloaded a list of words for both positive and negative sentiment from the internet, here are the references to the studies done which provided these word lists for use:

       Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews."
           Proceedings of the ACM SIGKDD International Conference on Knowledge
           Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle,
           Washington, USA, 
       Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing
           and Comparing Opinions on the Web." Proceedings of the 14th
           International World Wide Web conference (WWW-2005), May 10-14,
           2005, Chiba, Japan.


So, how to use this? Well, I just pasted the list of words into a file in vim, and ran a macro over them to convert them into two arrays like so:

    var happyWords = [ "yay", "funsome", "winsome" ]
    var sadWords = [ "boo", "crap", "lame" ]

There are actually about 5000 words in total, but essentially what I'm going to do is partition by language and keep a count of 

- How many commits per language I see
- How many happy words I see in each commit
- How many sad words I see in each commit

Now, real sentiment analysis is a little more complicated than simply looking for words, but we'll be happy with this for now, let's have a look at the projection:

    function collectHappinessIndexOfCommit(commit, state) {
       var index = 0
       for(var i in happyWords) {
           if(commit.message.indexOf(happyWords[i]) >= 0)
              state.happycount++
       }
       for(var i in sadWords) {
           if(commit.message.indexOf(sadWords[i]) >= 0)
              state.sadcount++
       }
       state.commits++
    }

    fromStreams(['github-commits'])
      .partitionBy(function(ev) {
        if(ev.body.repo)
          return ev.body.repo.language
      })
      .when({
        "$init": function() {
          return { 
             commits: 0, sadcount: 0, happycount: 0
          }
        },
        "Commit": function(state, ev) {
           collectHappinessIndexOfCommit(ev.body.commit, state)
        },
      })


I guess I'll say that my "happiness index" can be expressed by 

    var happinessIndex = (state.happycount - state.sadcount) / state.commits

Or something similar, let's have a look at the chart of happiness over languages

<div id="scaled"></div>

Well, I guess Coffeescript has something going for it after all...

<script type="text/javascript" src="/d3.v2.js"></script>
<script type="text/javascript">

var data = [
{
key: "Logtalk",
state: {
commits: 1,
sadcount: 0,
happycount: 0
}
},
{
key: "Standard ML",
state: {
commits: 2,
sadcount: 3,
happycount: 1
}
},
{
key: "Coq",
state: {
commits: 1,
sadcount: 0,
happycount: 0
}
},
{
key: "Eiffel",
state: {
commits: 3,
sadcount: 10,
happycount: 1
}
},
{
key: "VHDL",
state: {
commits: 5,
sadcount: 2,
happycount: 3
}
},
{
key: "Apex",
state: {
commits: 1,
sadcount: 7,
happycount: 0
}
},
{
key: "AutoHotkey",
state: {
commits: 1,
sadcount: 0,
happycount: 0
}
},
{
key: "Tcl",
state: {
commits: 3,
sadcount: 3,
happycount: 0
}
},
{
key: "Smalltalk",
state: {
commits: 21,
sadcount: 13,
happycount: 9
}
},
{
key: "Common Lisp",
state: {
commits: 65,
sadcount: 64,
happycount: 21
}
},
{
key: "FORTRAN",
state: {
commits: 4,
sadcount: 0,
happycount: 3
}
},
{
key: "ooc",
state: {
commits: 3,
sadcount: 0,
happycount: 0
}
},
{
key: "Vala",
state: {
commits: 20,
sadcount: 115,
happycount: 41
}
},
{
key: "ColdFusion",
state: {
commits: 4,
sadcount: 11,
happycount: 2
}
},
{
key: "ASP",
state: {
commits: 3,
sadcount: 0,
happycount: 0
}
},
{
key: "OpenEdge ABL",
state: {
commits: 33,
sadcount: 24,
happycount: 10
}
},
{
key: "Julia",
state: {
commits: 14,
sadcount: 29,
happycount: 5
}
},
{
key: "Puppet",
state: {
commits: 20,
sadcount: 20,
happycount: 4
}
},
{
key: "Factor",
state: {
commits: 21,
sadcount: 28,
happycount: 4
}
},
{
key: "R",
state: {
commits: 27,
sadcount: 17,
happycount: 10
}
},
{
key: "HaXe",
state: {
commits: 3,
sadcount: 11,
happycount: 1
}
},
{
key: "Racket",
state: {
commits: 35,
sadcount: 19,
happycount: 10
}
},
{
key: "Prolog",
state: {
commits: 12,
sadcount: 19,
happycount: 3
}
},
{
key: "PowerShell",
state: {
commits: 10,
sadcount: 13,
happycount: 5
}
},
{
key: "Verilog",
state: {
commits: 18,
sadcount: 12,
happycount: 15
}
},
{
key: "F#",
state: {
commits: 25,
sadcount: 15,
happycount: 11
}
},
{
key: "Rust",
state: {
commits: 99,
sadcount: 232,
happycount: 75
}
},
{
key: "Nemerle",
state: {
commits: 2,
sadcount: 3,
happycount: 3
}
},
{
key: "Scheme",
state: {
commits: 21,
sadcount: 12,
happycount: 9
}
},
{
key: "D",
state: {
commits: 13,
sadcount: 11,
happycount: 5
}
},
{
key: "Ceylon",
state: {
commits: 1,
sadcount: 0,
happycount: 0
}
},
{
key: "Arduino",
state: {
commits: 21,
sadcount: 11,
happycount: 12
}
},
{
key: "OCaml",
state: {
commits: 12,
sadcount: 14,
happycount: 2
}
},
{
key: "Assembly",
state: {
commits: 19,
sadcount: 11,
happycount: 7
}
},
{
key: "Delphi",
state: {
commits: 16,
sadcount: 12,
happycount: 10
}
},
{
key: "Dart",
state: {
commits: 20,
sadcount: 8,
happycount: 9
}
},
{
key: "ActionScript",
state: {
commits: 45,
sadcount: 32,
happycount: 30
}
},
{
key: "Erlang",
state: {
commits: 37,
sadcount: 25,
happycount: 20
}
},
{
key: "Clojure",
state: {
commits: 77,
sadcount: 54,
happycount: 33
}
},
{
key: "Groovy",
state: {
commits: 63,
sadcount: 41,
happycount: 18
}
},
{
key: "Haskell",
state: {
commits: 178,
sadcount: 201,
happycount: 87
}
},
{
key: "Visual Basic",
state: {
commits: 14,
sadcount: 13,
happycount: 6
}
},
{
key: "Emacs Lisp",
state: {
commits: 157,
sadcount: 145,
happycount: 82
}
},
{
key: "Go",
state: {
commits: 86,
sadcount: 111,
happycount: 52
}
},
{
key: "VimL",
state: {
commits: 256,
sadcount: 193,
happycount: 110
}
},
{
key: "Scala",
state: {
commits: 175,
sadcount: 132,
happycount: 93
}
},
{
key: "Ada",
state: {
commits: 2,
sadcount: 2,
happycount: 1
}
},
{
key: "Lua",
state: {
commits: 116,
sadcount: 52,
happycount: 20
}
},
{
key: "Perl",
state: {
commits: 322,
sadcount: 349,
happycount: 188
}
},
{
key: "Objective-C",
state: {
commits: 316,
sadcount: 233,
happycount: 95
}
},
{
key: "Matlab",
state: {
commits: 32,
sadcount: 7,
happycount: 0
}
},
{
key: "Shell",
state: {
commits: 705,
sadcount: 707,
happycount: 321
}
},
{
key: "C#",
state: {
commits: 550,
sadcount: 478,
happycount: 200
}
},
{
key: "Ruby",
state: {
commits: 2122,
sadcount: 1637,
happycount: 778
}
},
{
key: "PHP",
state: {
commits: 1703,
sadcount: 1498,
happycount: 615
}
},
{
key: "CoffeeScript",
state: {
commits: 118,
sadcount: 104,
happycount: 36
}
},
{
key: "C",
state: {
commits: 1724,
sadcount: 2571,
happycount: 1157
}
},
{
key: "JavaScript",
state: {
commits: 3700,
sadcount: 2615,
happycount: 1280
}
},
{
key: "Java",
state: {
commits: 2278,
sadcount: 2411,
happycount: 1097
}
},
{
key: "C++",
state: {
commits: 1215,
sadcount: 1199,
happycount: 610
}
},
{
key: "Python",
state: {
commits: 2153,
sadcount: 2049,
happycount: 925
}
}
] 
</script>

<script type="text/javascript">

   var filteredData = []
   for(var i =0 ; i < data.length; i++) {
     if(data[i].key === "VimL") continue;
     if(data[i].state.commits >= 100) {
       var datum = data[i]
       datum.state.index = (datum.state.happycount / datum.state.sadcount) / datum.state.commits
       filteredData.push(datum)
     }
   }

  var svg = d3.select("#scaled").append("svg")
          .attr("width", 800)
          .attr("height", 480)

   var scale = d3.scale.linear()
     .domain([0, d3.max(filteredData, function(d) { return d.state.index })])
     .range([0, 1]);

   var max = d3.max(filteredData, function(d) { return d.state.index });

   svg.append("text")
      .attr("fill", '#000')
      .attr("x", 110)
      .attr("y", 60)
      .text("Happiness index of github commit analysis")

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
         .attr("y", function(d, i) { return 370 - (280 * scale(d.state.index)) })
         .attr("width", 640 / (filteredData.length + 1))
         .attr("height", function(d, i) { return 280 * scale(d.state.index) })
</script>


