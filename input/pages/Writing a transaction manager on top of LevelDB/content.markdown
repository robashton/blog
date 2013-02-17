So we can do [consistent reads](/entries/transactional-guarantees-on-top-of-leveldb.html) and [batched writes](/entries/basic-operations-with-leveldb.html), but consistency across the two of these isn't built in.

Turns out this isn't a big deal and doesn't require an awful lot of work to build in on top of LevelDB. 

This might actually be one of the strengths of LevelDB, that it does some of the more technically challenging stuff (putting all the levels and atomicity into the DB for the experts), but doesn't make decisions like what sort of guarantees you need in your database for you.

So, into the C# for a little experiment I wrote a very rudimentary transaction manager on top of ConcurrentDictionary.


    var myStore = new Storage(leveldb);
    myStore.Batch(accessor => {
      var doc = accessor.Get("key1");
      accessor.Put("key2", "Hello");
    });
    


What does this look like?

    public void Batch(Action<StorageAccessor> actions) {
      var transaction = this.CreateTransaction();
      var accessor = new StorageAccessor(this, transaction);
      try {
        actions(accessor);
      } catch(Exception ex) {
        this.RollbackTransaction(transaction);
        throw;
      }
      this.CommitTransaction(transaction);
    }


Where the accessor simply calls the Get/Put/Delete methods on the storage and passes in the transaction associated with it.

    public class StorageAccessor {
      private Storage storage;
      private StorageTransaction transaction;

      public StorageAccessor(Storage storage, StorageTransaction transaction) {
        this.storage = storage;
        this.transaction = transaction;
      }

      public Object Get(string id) {
        return this.storage.Get(id, this.transaction);
      }

      public void Put(string id, Object obj) {
        this.storage.Put(id, obj, this.transaction);
      }

      public void Delete(string id) {
        this.storage.Delete(id, this.transaction);
      }
    }


Where we then make a check to see if another transaction has modified that key in the meantime


    public void Put(string id, Object obj, StorageTransaction transaction) {
      this.keysToTransactionId.AddOrUpdate(id, (key) => {
          transaction.AddOperation(storage => storage.Put(id, obj));
          return transaction.Id;
        }, 
        (key, oldValue) => {
        // NOTE: This doesn't handle the transaction doing multiple operations on the same key
        throw new Exception("This should be a concurrency exception but I'm lazy");
      });
    }


Obviously a real implementation needs to do things like keep modified information around so reads can go via that before hitting the LevelDB snapshot, but in essence you can build a pretty simple transaction manager providing your underlying storage does the hard work of managing atomicity of operations.

Next up, back to the C++ as I look at what we're doing do about large values in RavenDB documents.
