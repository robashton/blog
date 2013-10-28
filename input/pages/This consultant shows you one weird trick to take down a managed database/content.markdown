### Sysadmins hate him...

I was visiting a client last week who have been having trouble with their RavenDB instance for a few months and understandably getting a bit frustrated as time went on.

## The scene

I arrived, drank some coffee and we hit a room with the projector in it, and brought up the graphs of resource usage on the server running RavenDB - they've been pretty handy with the [splunk](http://www.splunk.com) and they have quite a few graphs! (Their usage of Splunk was *awesome* actually, can highly recommend looking at it)

Memory usage looks something like this through the day

  <script type="text/javascript" src="/d3.v2.js"></script>
  <script type="text/javascript" src="/dimple.js"></script>

  <div id="initial-memory-usage"></div>

  <script type="text/javascript">
    var svg = dimple.newSvg("#initial-memory-usage", 590, 400);
    var myChart = new dimple.chart(svg, [
    { Hour: 0, Memory: 15},
    { Hour: 1, Memory: 15},
    { Hour: 2, Memory: 15},
    { Hour: 3, Memory: 15},
    { Hour: 4, Memory: 0},
    { Hour: 5, Memory: 2},
    { Hour: 6, Memory: 4},
    { Hour: 7, Memory: 4},
    { Hour: 8, Memory: 4},
    { Hour: 9, Memory: 6},
    { Hour: 10, Memory: 6},
    { Hour: 11, Memory: 7},
    { Hour: 12, Memory: 8},
    { Hour: 13, Memory: 10},
    { Hour: 14, Memory: 10},
    { Hour: 15, Memory: 10},
    { Hour: 16, Memory: 14},
    { Hour: 17, Memory: 14},
    { Hour: 18, Memory: 10},
    { Hour: 19, Memory: 10},
    { Hour: 20, Memory: 15},
    { Hour: 21, Memory: 15},
    { Hour: 22, Memory: 15},
    { Hour: 23, Memory: 15}
    ]);
    myChart.setBounds(60, 30, 510, 305)
    var x = myChart.addCategoryAxis("x", "Hour");
    x.addOrderRule("Hour");
    myChart.addMeasureAxis("y", "Memory");
    myChart.addSeries(null, dimple.plot.bar);
    myChart.draw();
  </script>


That 4am block is a result of an automated process to kill their RavenDB instance every day because if they left it running it would being down the server when people were actually using the system - not so good! (It starts spiking around 9am because it starts being under quite a reasonable load).

## My line of questioning on seeing this

- How many databases on that one instance: *12*
- How much memory on the server: *16gb*
- How many cores on that server: *2*
- How much data in the databases?: *Between 500mb and 13gb*
- How many documents in that largest database?: *Er, not that many*
- How big are those documents?: *Some are quite big, they have PDFs attached to them*

# Ah.

The thing is, RavenDB can deal with large documents. Internally it does quite a few things to avoid objects ending up on the [Large Object Heap](http://msdn.microsoft.com/en-us/magazine/cc534993.aspx) or being promoted to the 2nd generation.

- Using streams in and out of core storage
- Using streams in an out of HTTP
- De-serializing only into RavenObject structures (lots of small objects)
- Not holding onto objects any longer than it has to

If you were to create objects with lots of fields that reached the above size in all likelihood RavenDB's practises around this kind of thing would result in happy developers, happy ops and happy sales teams?

But byte arrays that are automatically put on the Large Object Heap? There is little Raven can do about these, as when the objects internally are de-serialized into tokens, the smallest token it can make with them is however large the byte array is! 

Under what circumstances does RavenDB load these fields?

- Indexing
- Querying
- Loading

Imagine now that you create a new index on the server and it has to 

- run through all of the documents to put content into Lucene
- To do this it has to de-serialize them
- When being de-serialized .NET is going to say "That field is large, it is going on the Large Object Heap"
- It is going to have to look for space on the LOH
- It is going to expand the LOH
- They're all different sizes, it is unlikely to find space in the middle very often
- The LOH is going to keep expanding during the indexing process
- The machine is going to run out of memory

This is just typical .NET behaviour, and to make things worse, when the issues first started being noticed the first port of call was to open Raven Studio and start inspecting the server (performing queries), thus adding to the problem and causing even more hilarious memory spikes.

To give an indication, when opening up the performance counters for the server the kind of thing we were seeing looked like this:

  <img src="/img/lho.png">

Yes indeed, that's nearly all the memory on the server being allocated to the LHO as a result of excessive large objects of varying sizes being aggressively loaded through the indexing and querying processes.

# The solution?

Much like with every other database out there, storing binary blobs in a store which is built for querying/transactions isn't ideal - but there are two options available here

- External storage (s3, fileservers, anything else)
- RavenDB Attachments

The latter isn't encouraged as it's just a convenience - but to prove a point I generated 1.5 million documents of varying sizes with byte arrays on the fields to reproduce the problem successfully on my laptop (that's actually the screenshot above), then migrated them into attachments to show what a difference this would make as attachments are *never* loaded fully into memory.

  <img src="/img/beforeafter.jpg">

What a difference choosing an appropriate store makes! In the second number the "PDFs" are *still being stored in RavenDB*, just not in the primary document store.

When I left the client their server was sitting flat at 4gb consumption (with the database still full of PDFs, but instructions in how to avoid causing issues until they had been purged)

# The summary

- Well, I think Raven could benefit from having some sort of warning when it sees this sort of usage, although it's not *that* common so not really a priority
- There is little a database can do to get around this sort of thing, save sticking things in off-heap storage - but that's not going to work when your indexing is written in .NET


I'm currently [writing my own database](https://github.com/robashton/cravendb) in a different managed platform and I'm strongly considering sticking indexing into its own process to avoid this sort of long-term build up of issues. That said - the JVM doesn't do per-process GC so that might not help that much.

Either way it's interesting and points to one of the limitations of writing a database or any high throughput system in a managed environment if you're going to be expecting big chunks of data that can't be broken up somehow. (Okay, this is quite specific, and will rarely catch anybody out).
