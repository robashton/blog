I talked about RavenDB's new ability to promote and demote automatically created indexes [yesterday](/entries/auto-idling-auto-indexes-in-ravendb.html), but what are the implications of this?

I mentioned that the process in RavenDB for managing dynamically created indexes is as thus:

- Look for appropriate index to use in query
- If found, return the most appropriate index
- If not found, create an index that will deal with the query
- Return that index as a Temporary
- If that index is used enough, promote it into an Auto index
- If that index is not used enough, delete it

Well, we now have a way of marking indexes as idle and therefore not taking up needless resources, we can *remove temporary indexes entirely*.

That's actually a lot of code we can remove, in fact a pile of code that I wrote over two years ago, that's kinda cool, as it means entire code-path that doesn't need maintaining any more.

Obviously we can still remove auto indexes that were only queried once or twice, but that can take part with the algorithm that marks them as idle, and to make things even better we can add some functionality to the Studio to manage these idle indexes or delete them.

Now, this still isn't as awesome as it could be, but we've laid the ground work for a pretty awesome feature, I'll talk about that tomorrow.
