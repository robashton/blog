I went for lunch with the a friend the other day and he kindly gave me an idea for a blog post about JavaScript frameworks...
  
  <blockquote>
    I keep an eye on StackOverflow/Github/etc and mentally tot up the popularity of all these JS frameworks...
  </blockquote>

*ooooh*, that's a good one that. If we're *really* going to say "We use FrameworkX" (and I advocate that you *never* do this, it's a silly thing to do), then let's make our decision on cold hard numbers.

Except let's not just look at popularity, let's think about the support and education and how we could find some metrics to determine thee things.

Here are a few assumptions I'm making for this post to work

- StackOverflow is popular enough for the data to be representative (it's the de-facto place to ask programming questions)
- The number of questions/un-answered questions is indicative of support
- Github is popular enough for the data to be representative
- The number of projects, and the number of stars on a project is indicative of popularity/use

# Number of questions and unanswered questions on StackOverflow

The Stackoverflow guys are amazing, and [provide querying capabilities right there on their site](http://data.stackexchange.com/stackoverflow/queries), which means I can get some numbers right off the bat for the various frameworks

I ended up with the following parameterised query to get my data

    DECLARE @TagName varchar(128) = '%##TagName##%'

    SELECT COUNT(*) from Posts P
    WHERE P.Id IN (
      SELECT DISTINCT(PostId) from PostTags 
      WHERE PostTags.TagId In (
        SELECT Id From Tags Where TagName LIKE @TagName
      )
    )

    SELECT COUNT(*) from Posts P
    WHERE P.Id IN (
      SELECT DISTINCT(PostId) from PostTags 
      WHERE PostTags.TagId In (
        SELECT Id From Tags Where TagName LIKE @TagName
      )
    ) AND P.AcceptedAnswerId IS NOT NULL

    SELECT SUM(P.AnswerCount) from Posts P
    WHERE P.Id IN (
      SELECT DISTINCT(PostId) from PostTags 
      WHERE PostTags.TagId In (
        SELECT Id From Tags Where TagName LIKE @TagName
      )
    )

Doing this meant I got all the questions with all the tags in the eco-system around that framework, and the data I want is

- How many questions are there?
- How many of those questions have accepted answers?
- How mant answers do we get on average per question?

  <table>
    <thead>
      <tr>
        <td>Framework</td><td>Questions</td><td>Answered</td><td>%</td><td>Average answers per question</td>
      </tr>
    </thead>
      <tr><td>backbone</td><td>8863</td><td>5741</td><td>64.7%</td><td>1.42</td></tr>
      <tr><td>angular</td><td>5498</td><td>3401</td><td>61.8%</td><td>1.32</td></tr>
      <tr><td>knockout</td><td>5624</td><td>3917</td><td>69.6%/5624</td><td>1.31</td></tr>
      <tr><td>ember</td><td>10086</td><td>6426</td><td>63.71%</td><td>1.63</td></tr>
  </table>

This in itself is quite interesting, as it doesn't seem like there is a lot of difference between them - although there are a lot more questions about Ember than some of the other frameworks the accept rate is about the same.

To get some sense of this data, let's look at Github to shed further light on this

# Framework popularity

For this, we can head to [Github](http://github.com) as that's where all sensible OSS projects are hosted these days - let's have a quick look at some basic stats about these projects. They're all actively using Github to track issues from the look of things so we can gleam a little bit about them from this data.


  <table>
    <thead>
      <tr>
        <td>Framework</td>  <td>Stars</td> <td>Forks</td>  <td>Issues open</td>  <td>Issues closed</td>  <td>Pull requests open</td>   <td>Pull requests closed</td>
      </tr>
    </thead>
      <tr><td>backbone</td> <td>14148</td> <td>2713</td>  <td>23</td>             <td>2508</td>          <td>5</td>                    <td>1100</td>          </tr>
      <tr><td>angular</td>  <td>9692</td>  <td>1942</td>  <td>655</td>            <td>2037</td>          <td>52</td>                   <td>1150</td>          </tr>
      <tr><td>knockout</td> <td>3708</td>  <td>559</td>   <td>235</td>            <td>738</td>           <td>59</td>                   <td>275</td>           </tr> 
      <tr><td>ember</td>    <td>6997</td>  <td>1324</td>  <td>196</td>            <td>2487</td>          <td>61</td>                   <td>1250</td>          </tr>
  </table>


I'm surprised Ember being the second least "popular" framework on Github given the high number of questions on Stackoverflow, but I can keep digging before jumping to any conclusions.

# Safety in numbers

This data (when combined with the previous queries) begins to get a little bit more useful if we want to jump to some conclusions already; I'd like to dig deeper into the history of these projects and see who exactly has contributed to them. I don't want to be choosing a framework just because it has a large number of users, I care about who is doing the development too!

To do this, we have the Github API to look at 

    https://api.github.com/repos/{user}/{repo}/contributors

What do the contribution distributions look like for these projects?

### Top 10 contributors and their commit counts

  <div id="contribution-graph">

  </div>


There's not an awful lot on this, although Backbone seems to be a bit of a one-man show, angular and ember get love from a few people and Knockout looks like most .NET projects do (sorry Steve).

# A brief history of source

Now I'm looking at committers, we can probably dig a bit deeper and see how this looks over time to get some further insight into the stability of the projects and how this contributor activity looks over time.

I was going to just paste the charts from Github here, but they weren't really representative of activity and the Angular one just plain-old-wasn't-working at the time

I've found a git library for nodejs and will use it to generate a graph of activity over time. What you'd expect to see is that older more stable projects should see a tendency towards less activity and hopefully more contributors as the community fixes issues)


  <div id="contribution-over-time">

  </div>

# Conclusions

Now I can probably draw a few rudimentary conclusions at this point:

- Despite having only third of the followers on Github, Ember has far more questions being asked on Github
- Ember also has a similar amount of activity on Github itself to Backbone (although this could be indicative of the way they use Github)
- Knockout really doesn't have a lot of love (presumably that's mostly the .NET crowd), backed up by the high activity and answer-rate on Stackoverflow
- Backbone is a "mature" project, if its number of "open" issues is anything to go by
- Angular is undergoing *heavy activity* and churn still, with over 600 issues still open on Github
- Judging by its size, the Knockout contributors aren't keeping up well with the 
 
If I was waving my arms in the air and saying "*we must choose and use a framework* based on this data so far, I'd be jumping on the Backbone or angular ships because

- They both have a large community as evidenced by activity on Github
- High acceptance rate on Stackoverflow
- Low number of questions for its size
 

And that's that.

<script type="text/javascript" src="/d3.v2.js"> </script>

<script type="text/javascript">
  d3.json("/mvvmfw/angular.js.json", function(angular) {
    d3.json("/mvvmfw/knockout.json", function(knockout) {
      d3.json("/mvvmfw/ember.js.json", function(ember) {
        d3.json("/mvvmfw/backbone.json", function(backbone) {
          generateGraph({
            angular: angular,
            knockout: knockout,
            ember: ember,
            backbone: backbone
          })
        })
      })
    })
  })

  function generateGraph(data) {
    var svg = d3.select('#contribution-over-time')
                .append("svg")
                .attr("width", 800)
                .attr("height", 480)


      var maxx = 0, maxy = 0
      for(var fw in data) {
        var fwdata = data[fw]
        for(var i in fwdata) {
          maxx = Math.max(maxx, fwdata[i].month * fwdata[i].year)
          maxy = Math.max(maxy, fwdata[i].count / fwdata[i].committerCount)
        }
      }
      var scalex = d3.scale.linear()
      .domain([0, maxx])
      .range([100, 700]);

      var scaley = d3.scale.linear()
      .domain([0, maxy])
      .range([100, 340])

    var line = d3.svg.line()
              .interpolate('basis')
              .x(function(d) { return scalex(d.month * d.year)})
              .y(function(d) { return 480 - scaley(d.count / d.committerCount)})
      
    function addCircle(language, colour, y) {
      var langaugeData = data[language]

    svg.append("path")
      .attr("class", language)
      .attr("d", line(langaugeData))
      .attr("stroke",colour)
      .attr("stroke-width", 5)
      .attr("fill", "none")
    }
        
    addCircle('backbone', "blue")
    addCircle('knockout', "red")
    addCircle('ember', "green")
    addCircle('angular', "black")

  }

</script>

<script type="text/javascript">

d3.json("/mvvmfw/contribution.json", function(data) {

  var svg = d3.select("#contribution-graph").append("svg")
  .attr("width", 800)
  .attr("height", 480)

  var maxx = 0, maxy = 0
  for(var fw in data) {
    var fwdata = data[fw]
    for(var i in fwdata) {
      maxx = Math.max(maxx, fwdata[i].x)
      maxy = Math.max(maxy, fwdata[i].y)
    }
  }


  var scalex = d3.scale.linear()
  .domain([0, maxx])
  .range([100, 700]);

  var scaley = d3.scale.linear()
  .domain([0, maxy])
  .range([100, 340])

  var line = d3.svg.line()
              .interpolate('basis')
              .x(function(d) { return scalex(d.x)})
              .y(function(d) { return 480 - scaley(d.y)})

  svg.append("text")
    .attr("x", 25)
    .attr("y", 50)
    .style("font-weight", "bolder")
    .text("Commits")

  svg.append("text")
    .attr("x", 25)
    .attr("y", 450)
    .style("font-weight", "bolder")
    .text("Commiter #")

  svg.append("text")
    .attr("x", 25)
    .attr("y", 140)
    .text(maxy)

  svg.append("text")
    .attr("x", 25)
    .attr("y", 380)
    .text(0)

  svg.selectAll(".labelx")
    .data([1,2,3,4,5,6,7,8,9,10])
    .enter()
    .append("text")
    .attr("class", "labelx")
    .text(function(d) { return '#' + d })
    .attr("y", 450)
    .attr("x", function(d) { return scalex(d) })

  var legendLine = d3.svg.line()
                    .x(function(d) { return d.x })
                    .y(function(d) { return d.y })


  function addLine(language, colour, y) {
    var langaugeData = data[language]

    svg.append("path")
      .attr("class", language)
      .attr("d", line(langaugeData))
      .attr("stroke",colour)
      .attr("stroke-width", 5)
      .attr("fill", "none")

    svg.append("path")
      .attr("d", legendLine([{x:400, y: y},{x:500, y: y}]))
      .attr("stroke", colour)
      .attr("stroke-width", 5)

    svg.append("text")
    .attr("x", 520)
    .attr("y", y)
    .text(language)

  }
      
  addLine('backbone', "blue", 50)
  addLine('knockout', "red", 100)
  addLine('ember', "green", 150)
  addLine('angular', "black", 200)

})


</script>



