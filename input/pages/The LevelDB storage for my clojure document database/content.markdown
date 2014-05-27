Having shown how we take most of the work and keep it common across different storage implementations, let's dive into an actual implementation of this storage protocol and look at the LevelDB implementation.

- [I wrote a document database in Clojure](/entries/i-wrote-a-document-database-in-clojure.html)
- [The Client API for my Clojure document database](/entries/the-client-api-for-my-clojure-document-database.html)
- [The HTTP API for my Clojure document database](/entries/the-http-api-for-my-clojure-document-database.html)
- [The core storage protocols for the Clojure document database](/entries/the-core-storage-protocols-for-my-clojure-document-database.html)

All of the code for the LevelDB storage can be found in the file *levelstorage.clj* and it clocks in at just under 100 lines of code which isn't too shabby at all.

LevelDB is actually a native code project, and I found myself using a wrapper that some folks have kindly written and open sourced for Java, thus we have the first of our Java Interop imports in this database.

    (:import (org.iq80.leveldb Options ReadOptions WriteOptions DBIterator)
              (org.fusesource.leveldbjni JniDBFactory)
              (java.io File)
              (java.nio ByteBuffer)))

Lovely lovely classes.

I mentioned in the previous entry that we ended up with some simple protocols describing what we expect from our storage implementation

**storageops.clj**

    (defprotocol Reader
      (open-iterator [this])
      (from-db [this id]))

    (defprotocol Writer
      (commit! [this]))

    (defprotocol Iterator
      (seek! [this k])
      (as-seq [this]))

    (defprotocol Storage
      (ensure-transaction [this]))

In the *levelstorage.clj*, these are implemented as thinly as possible before farming out the actual work to pure clojure methods, thus we have

Our core storage record

    (defrecord LevelStorage [path db]
      java.io.Closeable
      Reader
      Storage
      (ensure-transaction [ops]
        (debug "Opening transaction")
        (let [options (ReadOptions.)
              snapshot (.getSnapshot (:db ops))]
          (.snapshot options snapshot)
          (LevelTransaction. (:db ops) options (:path ops))))
      (from-db [this id] (from-storage this id))
      (open-iterator [this] (get-iterator this))
      (close [this]
        (.close db)
        nil))

Our transaction record

    (defrecord LevelTransaction [db options path]
      java.io.Closeable
      Writer
      Reader
      (open-iterator [this] (get-iterator this))
      (from-db [this id] (from-storage this id))
      (commit! [this] (commit! this))
      (close [this]
        (.close (.snapshot options))))

And our iterator

    (defrecord LevelIterator [inner]
      java.io.Closeable
      Iterator
      (seek! [this value] (.seek inner (to-db value)))
      (as-seq [this]
        (->> (iterator-seq inner) (map expand-iterator-str)))
      (close [this] (.close inner)))

First off, we can see that all these records implement "Closeable", this is so that they can be used within the *with-open* built-in macro, which ensures that anything opened gets closed, regardless of exceptions or whatever.


We can also see that the transaction and storage implement the same methods and farm the actual work out to pretty much the same functions.


**Getting an object**

This is the method called by both the transaction and storage records

    (defn from-storage [ops id]
      (from-db (safe-get (:db ops) (to-db id) (:options ops))))

And safe-get looks like this

    (defn safe-get [db k options]
      (try
        (if options
          (.get db k options)
          (.get db k))
        (catch Exception e
          nil)))

Options are what LevelDB use to do things within a transaction/snapshot. This could have been a multi-method, but I felt the if statement kept things pretty expressive and easy to follow.

That from-db thing is for the conversion of a byte array into a nice clojure data structure:

    (defn from-db [v]
      (if (nil? v) nil
      (with-open [reader (java.io.PushbackReader.
                              (clojure.java.io/reader
                                (java.io.ByteArrayInputStream. v)))]
        (edn/read reader))))


**The iterator**

The iterator is interesting, as this shows the easiest way I found to wrap up a native resource that implements iterators, and still allow the use of *iterator-seq*.

The difficulty with iterator-seq, is that if you were to call it on something that kept a native handle (such as the above), the lazy nature of Clojure makes it very easy to write code that by accident doesn't evaluate until after the handle is closed.

By pushing this into a Closeable object and providing an as-seq method, we make the consumers responsible for evaluating the sequence and closing the resource in the correct order.

*expand-iterator* seemed like a sensible notion, as the iterator returns a weird object with getKey and getValue which have byte arrays and aren't very friendly to consuming code.

    (defn expand-iterator-str [i]
      { :k (from-db (.getKey i))
        :v (from-db (.getValue i)) })


**Committing a transaction**

Well you can see that the transaction just calls commit with itself

    (commit! [this] (commit! this))

And has some pretty simple logic for calling "delete" or "put" depending on the contents of the transaction

    (defn commit! [{:keys [db cache] :as tx}]
      (with-open [batch (.createWriteBatch db)]
          (doseq [[id value] cache]
            (if (= value :deleted)
              (.delete batch (to-db id))
              (.put batch (to-db id) (to-db value))))
          (let [wo (WriteOptions.)]
            (.sync wo true)
            (.write db batch wo))))

Again, we have a to-db function there, this just does the opposite of the from-db function shown above

    (defn from-db [v]
      (if (nil? v) nil
      (with-open [reader (java.io.PushbackReader.
                              (clojure.java.io/reader
                                (java.io.ByteArrayInputStream. v)))]
        (edn/read reader))))

Keeping the to/from DB functions in just one place in the codebase simplified things immensely, this is the only place we deal with byte arrays and if need be they could be replaced with streaming functions for performance (if the LevelDB wrapper supported such things).

Everywhere else, it's just plain old Clojure Data Structures and that keeps the rest of the codebase pretty clean.

**Summary**

Interop with plain old Java isn't too bad if you keep it behind nice little wrappers like this. Records and protocols are still nice, but in this case it seemed much better to keep the actual code out of the records and deal with the data inside a record as if it were a Plain Old Map.

Keeping the storage dealing only with clojure data structures, and hiding how we convert to/from the actual storage layout means I can optimise in the future if need be and keep those large byte arrays out of memory.

**Related Files**

- levelstorage.clj
- storage.clj
