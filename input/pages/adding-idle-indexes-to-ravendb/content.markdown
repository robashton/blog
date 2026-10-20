As I mentioned [yesterday](/entries/working-at-hibernating-rhinos.html), I'm currently doing a week or so of work at Hibernating Rhinos (more specifically on RavenDB).

One of the first things I was asked to look at was the indexing system in RavenDB, as there was a backlog of tasks and requests on these, which tie into each other in numerous ways.

I don't like working on more than one thing at once when I'm Getting Things Done, and while it was tempting to jump right in and say *"We need to do the feature where dynamic indexes can be turned off, deleted and compacted over time"*, the task was broken up into a few discrete features - some which had been requested individually anyway.

That brings me to the first feature we added in the first few hours of starting work at the office, I sat down with [@ayende](http://twitter.com/ayende) and added the capability for indexes to be idle.

What does that mean? Why would we want to do it? 
--------------------------------

In RavenDB, querying is cheap because the answers have already been pretty much worked out by the indexing process. The indexing process has to crawl over every document as they're added and run user-logic over those documents, this is a process that tends to be IO bound as well as using up a few CPU cycles.

Thus, the more indexes you have, the more work the database has to do during heavy write periods - and as some customers have many indexes and also undergo heavy write periods, it has been requested more than once that indexes be prioritisable - this feature is a big step in that direction. 

![](/img/idlewis.png)

Indexes on the server now have a new flag, "Priority", which can take the following values (Normal | Idle | Forced), the idea being that the user can use the administration console to set indexes that aren't as important to not be run during busy write periods, and instead only be processed during idle time.

The forced flag? Well, I'll write about that tomorrow when I cover how idle indexes have been used to make auto-indexing a bit cleverer.
