I was recently called in to do an emergency consult at a new client because their RavenDB instance was in a bit of a pickle and anybody they *might* have wanted to call in was already busy so I got the call ;-)

## The situation

- I arrived bleary eyed at 9am, and I had less than four hours until I had to leave to get on a plane to Geneva(!!)
- "We have conflict documents all over the place in our secondary database"
- "We have documents that exist in the secondary and not the primary"

Ruh oh!

My immediate assumption on hearing that their secondary was full of documents that weren't on the primary was that they were actually running a primary/primary set-up by accident, but in actual fact it was slightly more involved than that.

## The RavenDB Replication Bundle

RavenDB replication is set up by telling a server that it has a replication destination, that is a primary is told about the secondary and instructed to push documents over there when it can get the chance. It uses etags to determine which documents need to go over and uses etags to detect conflicts and create mulitiple versions of a conflicted document.

The different between primary/primary or primary/secondary is simply whether you set up both servers with a replication destination or just one of them.

*So far so good*

- The thing is, the client has the ability to failover automatically when the primary stops being reachable for whatever reason. 
- By default the client isn't allowed to write to the secondary and it is an explicit option to turn that on.
- The assumption is that if the primary goes down, the system should go into *read only* mode and prevent further damage

This is quite a sensible set-up, and a very reasonable one because you don't know what caused that failure, don't want the failure to repeat on the secondary and a diminished functionality is still better than no functionality at all.

A primary/secondary set-up is conceptually a lot easier to deal with than a primary/primary or cluster-setup because you never have to deal with conflicts. Conflicts in a lot of state-based systems are painful to deal with so if we can avoid dealing with them then we should.

RavenDB also has the option to allow writes to secondary and *this is where the fun begins*.

- The application is writing to the primary
- The primary becomes unreachable for some reason
- The application starts writing to secondary
- The primary becomes reachable once more
- The application starts writing to primary
- There are documents left orphaned on secondary!!!
- Eventually, conflicts (or in this client's case, pretty quickly because *batch processes*)

What we're saying here actually, is that if you are writing to secondary on failure, what you actually have is a primary/primary with the wrong name.

It's left in userland to determine what to do here, we could:

- Set up a replication target for the secondary so when this happens we get write-backs once primary returns
- Manually trigger a copy of the data back to primary once we establish that primary is "okay"
- Etc

The essence of this though is that if we're going to be allowing writes to secondary during failure, then we need to have some form of conflict resolution set up because it's not *really* a primary/secondary relationship these servers have.

## Their fix

We have two choices, we can 

- Have a read-only failure mode
- Have a write-on-failure mode but handle conflicts gracefully

Opting for the latter because their internal consumer *always* wants to be able to write, the easiest approach was to write a "last write wins" conflict resolver. Not always advisable but in this case there were few side effects from adopting such a position.

## The summary

You should think about your topology and what you actually want to support when setting this up. This is usually always a business decision, as it revolves around what levels of availability they need to do their job.

