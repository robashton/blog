One of the great things about RavenDB, is the in-memory mode that gives users the ability to write tests against one interface, but switch in an embedded in-memory system for testing. Not touching the disk but getting fast (semi) integration tests and feedback is pretty leet and I want it too.

- [I wrote a document database in Clojure](/entries/i-wrote-a-document-database-in-clojure.html)
- [The Client API for my Clojure document database](/entries/the-client-api-for-my-clojure-document-database.html)
- [The HTTP API for my Clojure document database](/entries/the-http-api-for-my-clojure-document-database.html)
- [The core storage protocols for the Clojure document database](/entries/the-core-storage-protocols-for-my-clojure-document-database.html)
- [LevelDB storage implementation for the Clojure document database](/entries/the-leveldb-storage-for-my-clojure-document-database.html)

Well, this is quite a simple thing. As described in the last two entries - we have a very small surface area to implement for our storage system, and all objects are passed in as Plain Old Clojure data structures. The implemention therefore clocks in at under 40 lines of code and is shown in its entirety below.

Memory storage is implemented as an atom containing a sorted map.

    (defn create [] (MemoryStorage. (atom (sorted-map))))

MemoryStorage merely returns whatever is in this atom at the start of a transaction (giving read isolation)

    (defrecord MemoryStorage [memory]
      java.io.Closeable
      Reader
      Storage
      (ensure-transaction [ops] (MemoryTransaction. @(:memory ops) (:memory ops)))
      (from-db [this id] (get @memory id))
      (open-iterator [this] (MemoryIterator. nil memory (atom nil)))
      (close [this] nil))

Commiting the transaction, is just a reduction of operations inside the transaction into whatever is in the atom.

    (defrecord MemoryTransaction [snapshot memory]
      java.io.Closeable
      Writer
      Reader
      (open-iterator [this] (MemoryIterator. snapshot memory (atom nil)))
      (from-db [this id] (get snapshot id))
      (commit! [this] (swap! memory #(reduce (fn [m [k v]]
                      (if (= :deleted v) (dissoc m k) (assoc m k v)))
                                            %1 (:cache this))))
      (close [this] nil))

And because we created a sorted map, iterating means dropping up to the point where we want to begin iterating, and returning the sequence.

    (defrecord MemoryIterator [snapshot memory start]
      java.io.Closeable
      Iterator
      (seek! [this value] (swap! start (fn [i] value)))
      (as-seq [this]
        (map (fn [i] {:k (key i) :v (val i)})
          (drop-while #(> 0 (compare (key %1) @start)) (or snapshot @memory))))
      (close [this] nil))

Obviously not suitable for production, but it keeps the tests fast (and runnable on various environments without needing native compiles). It also makes it easier to work in the REPL as there is no need to worry about on-disk artifacts or locking.

**Summary**

Having a small surface area for storage implementation, and keeping communication with core storage a matter of using plain old clojure data structures made this easy to write.

**Related Files**

- memorystorage.clj
