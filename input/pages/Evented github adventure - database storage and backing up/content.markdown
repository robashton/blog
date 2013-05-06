If you're joing us after the bank holiday weekend in the UK, then we're on the Event Store and looking at Github data.

- [Introduction to the EventStore](/entries/playing-with-the-eventstore.html)
- [Pushing data into the EventStore](/entries/pushing-data-into-streams-in-the-eventstore.html)
- [Projections in the EventStore](/entries/basic-projections-in-the-eventstore.html)
- [Re-partitioning streams in the EventStore](/entries/re-partitioning-streams-in-the-event-store-for-better-projections.html)
- [Creating a projection per stream](/entries/creating-a-projection-per-stream-in-the-eventstore.html)
- [Pumping data from Github into the EventStore](/entries/less-abstract,-pumping-data-from-github-into-the-eventstore.html)
- [Emitting new events from a projection](/entries/evented-github-adventure---emitting-commits-as-their-own-events.html)


I actually wrote all of these blog entries several months ago, and then queued them up because I wanted to get some real data before running my projections.

*Problem: I ran the events into the store on my EC2 "small" instance and now have 25gb of data sat there to run projections over.*

I can't actually run my projections on that much data on that small instance without either waiting for a bazillion years or taking everthing else down on that box.

I quickly realise that I'm not going to be able to push the next blog post out on time either way, and decide to write this post and have a look at what I need to do to move my data onto a new EC2 machine.

**Step 1: Create an AMI of the current EC2 instance**

  <img src="/img/create_ami.png" title="creating an AMI" />


**Step 2: Create an xlarge instance from this AMI**

  <img src="/img/event_store.png" title="Creating xlarge" />

**Step 3: Log in and give this instance access to the old instance**

- ssh-keygen *etc*
- copy the public key and add it to the old EC2 authorized\_keys
- ssh from new machine to old machine to make sure it works

**Step 4: Get that data onto the new machine**

This is interesting, if you look in the folder I pointed the EventStore at to store its information it looks like this:

  <img src="/img/data_storage.png" title="raw files for storage">

Apparently all I have to do is copy this folder over and I do this with SCP like so

  <img src="/img/backing_up.png" title="backing up" />

And then I'm able to run the already-built event store because that came with the AMI I made. Everything works great and I celebrate.

**Step 5: Wait**

There may be some gaps in my blog entries from this point on as I run the projections over the events to get the results, there is a lot of data and patience is required...


  <img src="/img/running_projections.png" title="running hte projections" />


