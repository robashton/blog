So, in [trying out LevelDB](/entries/investigating-ravendb-on-mono-for-reals.html), I need to work out the basics, and then apply those learnings to whether the functionality exposed is compatible with RavenDB's storage needs.

I spent a few hours in the car on the way to and back from the Dead Sea and this seemed like an ideal time to crack out the g++ and write some C++ on top of LevelDB.

The first thing I did was download [the source](https://code.google.com/p/leveldb/) and un-pack it, I also grabbed [@kellabytes](http://twitter.com/kellabyte)'s '[Dazzle](https://github.com/kellabyte/Dazzle)' source code as an 'RTFM' back-up.

What I really like is that the header files for LevelDB are the best means of documentation, I forgot I liked this about C++, I now remember - all the learnings done ended up being done by just reading the source code, pretty neat.

So, what do we have?

*Opening a database*


    leveldb::DB* db;
    leveldb::DB::Open(options, "play/testdb", &db);
    

*Putting something in*


    status = db->Put(leveldb::WriteOptions(), "Key", "Hello World");


*Getting something out*

    std::string document;
    store->Get(leveldb::ReadOptions(), "key", &document);


*Deleting something*

    store->Delete(leveldb::WriteOptions(), "key");


*Squeeeeeeee*

I love how simple that is, and that each of these is a safe operation, important to note the following at this point:

  - I'm passing in strings to those put/get operations
  - LevelDB is copying to/from those strings
  - Because we're using strings, scope determines the release of memory

This is elegant, Put actually takes a "Slice" type too, but that's implicitly convertable from a string and therefore this works nicely. 

I'll cover Slice on its own as it's an interesting notion if I understand it correctly.

Importantly for RavenDB, we need to be able to write multiple operations in an atomic fashion, LevelDB appears to accommodate for this neatly.

     leveldb::WriteBatch batch;
     batch.Delete("key");
     batch.Put("key2", value);
     db->Write(leveldb::WriteOptions(), &batch);


LevelDB can actually operate in async or synchronous mode, but because Raven makes gaurantees about writes having happened I can't think we'd be able to use async mode (because there isn't any way to know when these writes are finished to my knowledge).

These guarantees aren't actually enough for RavenDB, and I'll cover the reasons for that in the next entry.

