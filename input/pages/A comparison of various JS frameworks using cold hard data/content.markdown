I went for lunch with the a friend the other day and he kindly gave me an idea for a blog post about JavaScript frameworks...
  
  <blockquote>
    I keep an eye on StackOverflow/Github/etc and mentally tot up the popularity of all these JS frameworks...
  </blockquote>

*ooooh*, that's a good one that. If we're *really* going to say "We use FrameworkX" (and I advocate that you *never* do this, it's a silly thing to do), then let's at least look at some cold hard facts when doing so.

Except let's not just look at popularity, let's think about the support and education and how we could find some metrics to determine these things.

Here are a few assumptions I'm making for this post to work

- StackOverflow is popular enough for the data to be representative (it's the de-facto place to ask programming questions)
- The number of questions/un-answered questions is indicative of support
- Github is popular enough for the data to be representative
- The number of projects, and the number of stars on a project is indicative of popularity/use

# Number of questions and unanswered questions on StackOverflow

The Stackoverflow guys are amazing, and [provide querying capabilities right there on their site](http://data.stackexchange.com/stackoverflow/queries), which means I can get some numbers right off the bat for the various frameworks

I ended up with the following parameterised query to get my data


```sql
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
```


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
      <tr><td>angular</td><td>5498</td><td>3401</td><td class="red">61.8%</td><td>1.32</td></tr>
      <tr><td>knockout</td><td class="green">5624</td><td>3917</td><td class="green">69.6%</td><td>1.31</td></tr>
      <tr><td>ember</td><td class="red">10086</td><td>6426</td><td>63.71%</td><td>1.63</td></tr>
  </table>

This in itself is quite interesting, as it doesn't seem like there is a lot of difference between them - although there are a lot more questions about Ember than I was expecting. Knockout is at the top of the heap as far as accept-rate goes but that might be indicative of the .NET-centric community found on StackOverflow?

To get some sense of this data, we could do with looking elsewhere for some data...

# Framework popularity

For this, we can head to [Github](http://github.com) as that's where all sensible OSS projects are hosted these days. Let's have a quick look at some basic stats about these projects. They're all actively using Github to track issues from the look of things so we can gleam a little bit about them from this data.


  <table>
    <thead>
      <tr>
        <td>Framework</td>  <td>Stars</td> <td>Forks</td>  <td>Issues open</td>  <td>Issues closed</td>  <td>Pull requests open</td>   <td>Pull requests closed</td>
      </tr>
    </thead>
      <tr><td>backbone</td> <td class="green">14148</td>  <td class="green">2713</td>            <td class="green">23</td>             <td>2508</td>          <td class="green">5</td>      <td>1100</td>          </tr>
      <tr><td>angular</td>  <td>9692</td>                 <td>1942</td>                          <td class="red">655</td>              <td>2037</td>          <td >52</td>                  <td>1150</td>          </tr>
      <tr><td>knockout</td> <td class="red">3708</td>     <td class="red">559</td>               <td>235</td>                          <td>738</td>           <td>59</td>                   <td>275</td>           </tr> 
      <tr><td>ember</td>    <td>6997</td>                 <td>1324</td>                          <td>196</td>                          <td>2487</td>          <td class="red">61</td>       <td>1250</td>          </tr>
  </table>


Backbone apparently by far the healthiest project amongst this bunch, with very few issues or pull requests left open (but with a similar amount of activity). I'm surprised a more "mature" project like Angular has so much outstanding on it - but this might just be because of how they use Github issues.

# Safety in numbers

This data (when combined with the previous queries) begins to get a little bit more useful if we want to jump to some conclusions already; I'd like to dig deeper into the history of these projects and see who exactly has contributed to them. I don't want to be choosing a framework just because it has a large number of users, I care about who is doing the development too!

To do this, we have the Github API to look at 

    https://api.github.com/repos/{user}/{repo}/contributors

What do the contribution distributions look like for these projects?

### Top 10 contributors and their commit counts

  <div id="contribution-graph">

  </div>


The higher a line stays before it drops down to the 1st, a better indication that there is a healthy spread of committers giving love to the project. By this measure, Angular and Ember seem to be getting some shared love, Backbone is a bit of a one man show and Knockout doesn't really have a lot going on.

# A brief history of source

Now I'm looking at committers, we can probably dig a bit deeper and see how this looks over time to get some further insight into the stability of the projects and how this contributor activity looks over time.

I was going to just paste the charts from Github here, but they weren't really representative of activity and the Angular one just plain-old-wasn't-working at the time

I've found a git library for nodejs and will use it to generate a graph of activity over time. What you'd expect to see is that older more stable projects should see a tendency towards less activity and hopefully more contributors as the community fixes issues)

## Average commits per person over time per repo

  <div id="contribution-over-time">

  </div>

# Conclusions

I've got no real desire to draw conclusions off this data, as this was a harmless bit of digging over a spare few hours I had going, I do have some rough comments though:

- EmberJS has a third of the followers on Github to Backbone, a similar amount of activity on Github but *more questions on StackOverflow* - is this an indication as to it being hard to pick up or use? It also has a high amount of code churn still - presumably because it's still quite an immature project (I didn't realise how much younger it was than the others!)
- Backbone is clearly reaching some sort of maturity, with few issues left open on Github, commit activity subsiding and some sort of stability being reached. It doesn't have the high number of core contributors of other projects but there are plenty of people willing to fix bugs still. The support rate on StackOverflow is about the same as the other projects and while it might be the most popular project on Github it doesn't have that many questions considering that huge size.
- Knockout doesn't really get the love that other frameworks do, this might be due to its conception taking place in the .NET community or because most people steer clear of it for some reason. That said - if you're using it and asking for help on StackOverflow you're more likely to get help than on the other frameworks. The response rate on issues/pull requests is high which means the core contributors are quite active even if there aren't many of them.
- Angular is *waaaaay* older than I thought it was, so it staggers me the amount of issues pending closure. It has a very solid contributor base and it does seem as though the commit-rate is reaching some sort of stability.  It does however have the lowest accept-rate on StackOverflow so read into that what you will. 

I'm saying no more on the matter, trololol.

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


      var maxx = 0, maxy = 0, minx = Infinity, miny = Infinity
      for(var fw in data) {
        var fwdata = data[fw]
          , newdata = []
        for(var i in fwdata) {
          if(fwdata[i].year < 2000) continue
          fwdata[i].month++
          var monthstr = fwdata[i].month > 9 ? fwdata[i].month : '0' + fwdata[i].month
          var date =  new Date(fwdata[i].year + '-' + monthstr + '-01')

          fwdata[i].x = date.getTime()

          maxx = Math.max(fwdata[i].x, maxx)
          minx = Math.min(minx, fwdata[i].x)
          maxy = Math.max(maxy, fwdata[i].count / fwdata[i].committerCount)
          miny = Math.min(miny, fwdata[i].count / fwdata[i].committerCount)
          newdata.push(fwdata[i])
        }
        data[fw] = newdata
      }

      var scalex = d3.scale.linear()
      .domain([minx, maxx])
      .range([100, 700]);

      var scaley = d3.scale.linear()
      .domain([miny, maxy])
      .range([100, 340])

    var line = d3.svg.line()
              .interpolate('basis')
              .x(function(d) { return scalex(d.x)})
              .y(function(d) { return 480 - scaley(d.count / d.committerCount)})
      
    var legendLine = d3.svg.line()
                    .x(function(d) { return d.x })
                    .y(function(d) { return d.y })

    svg.append("text")
      .attr("x", 25)
      .attr("y", 50)
      .style("font-weight", "bolder")
      .text("Avg commits")

    svg.append("text")
      .attr("x", 25)
      .attr("y", 460)
      .style("font-weight", "bolder")
      .text("Time")

    svg.selectAll(".labely")
      .data(d3.range(miny, maxy, 10))
      .enter()
        .append("text")
        .attr("class", "labely")
        .text(function(d) { return d })
        .attr("y", function(d) { return 480 - scaley(d)})
        .attr("x", 20)

    svg.selectAll(".labelx")
      .data(d3.range(minx, maxx, 1000 * 60 * 60 * 24 * 30 * 12))
      .enter()
        .append("text")
        .attr("class", "labelx")
        .text(function(d) { return new Date(d).getFullYear() })
        .attr("y", 460)
        .attr("x", function(d) { return scalex(d)})
    
    function addCircle(language, colour, y) {
      var langaugeData = data[language]

      svg.append("path")
        .attr("class", language)
        .attr("d", line(langaugeData))
        .attr("stroke",colour)
        .attr("stroke-width", 5)
        .attr("fill", "none")

      svg.append("path")
        .attr("d", legendLine([{x:500, y: y},{x:600, y: y}]))
        .attr("stroke", colour)
        .attr("stroke-width", 5)

        svg.append("text")
        .attr("x", 620)
        .attr("y", y)
        .text(language)
    }
        
    addCircle('backbone', "blue", 50)
    addCircle('knockout', "red", 100)
    addCircle('ember', "green", 150)
    addCircle('angular', "black", 200)

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
      .attr("d", legendLine([{x:500, y: y},{x:600, y: y}]))
      .attr("stroke", colour)
      .attr("stroke-width", 5)

    svg.append("text")
    .attr("x", 620)
    .attr("y", y)
    .text(language)

  }
      
  addLine('backbone', "blue", 50)
  addLine('knockout', "red", 100)
  addLine('ember', "green", 150)
  addLine('angular', "black", 200)

})


</script>



