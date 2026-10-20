[Other day](/entries/getting-rid-of-temporary-indexes-in-ravendb.html) I wrote about the awesome news that we'd managed to delete an entire code-path from RavenDB as a result of some of the other changes coming in.

Well, it's not quite as simple as that, you see - temporary indexes in RavenDB had a reallly big advantage in that they were run entirely in memory (until they reached a configurable threshold anyway).

That's good, because  indexes and running data through indexes is typically an IO bound process, and if we can keep the indexes in memory while we work on them, the whole proces can be a lot more efficient.

Well, having gotten rid of an entire code path, surely it makes sense to move the code that used to do this to be applicable to all indexes?

Absolutely, so starting from whenever my pull request gets merged, all new indexes in RavenDB will be ran in memory to begin with (meaning you get less stale results much faster - great if you're using the auto-indexes or experimenting with manual indexes on large sets of data).

After the index has caught up with reality, it will be flushed to disk and celebrations will be had, or if the index reaches a configurable memory threshold that will happen too.

This is a small change that makes a big difference, and is possible (in part) because we managed to get rid of having two different types of index and two different code paths.

In the next entry, I'll talk about further optimisations made in this pull request to do with auto indexes.
