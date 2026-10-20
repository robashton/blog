So, I'm [experimenting with LevelDB](/entries/basic-operations-with-leveldb.html) and I've discovered that it does atomic writes both on single operations and batches of operations.

This isn't actually all we need in RavenDB, as we need to be able to support multiple reads and writes - potentially over a period of time.

We can't just be reading from a database that might be changing under our feet all of the time, and it turns out that LevelDB gives us the ability to read from a particular version of the database.

When beginning a long running operation that needs a consistent read-state, we can create a snapshot and use this in all of our reads to ensure we have a consistent view of the database.

    // Create a snapshot at the beginning of a sequence of operations
    leveldb::Snapshot* snapshot = db->GetSnapshot();


    // For each read operation we can use this snapshot
    leveldb::ReadOptions options;
    options.snapshot = snapshot;

    db->Get(options, "key", &document);


That solves that problem then, although it leaves another question in the air - which is how LevelDB handles multiple writers modifying the same key.

Consider a thread coming along and beginning an operation

    tx1 = store->BeginOperation();

And another thread beginning an operation

    tx2 = store->BeginOperation();


And then
  
    // Thread one deletes a key
    store->Delete("key1", tx1);

    // Thread two Writes to that key
    store->Put("key1", tx2);


By default (at least as I understand) LevelDB will happily accept these operations as it doesn't have any concurrency control.

Happily, this is easy enough to work through as we'll see in the next entry.

