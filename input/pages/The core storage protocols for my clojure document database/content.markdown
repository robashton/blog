So obviously when I say I'm writing a database in order to learn clojure, I'm not talking about writing a storage engine for a database (although no doubt that would be a fun thing to attempt in a language like Clojure).

So I'll need to use "*something*" to do storage for me which comes with some sort of gaurantees, but I'll also want to do an in-memory version without those guarantees for fast tests written against the database.

- [I wrote a document database in Clojure](/entries/i-wrote-a-document-database-in-clojure.html)
- [The Client API for my Clojure document database](/entries/the-client-api-for-my-clojure-document-database.html)
- [The HTTP API for my Clojure document database](/entries/the-http-api-for-my-clojure-document-database.html)

Once again, this smacks of polymorphism, and once again the initial pass at this was undertaken with multi-methods before it became a mess and it became necessary to split things up with proper protocols. (Seriously, I don't understand why I'd ever use multi-methods at this point)

**Core Storage Protocols**

So, my core storage system needs to

- Have a small surface area, as I don't want to have to implement too much for each storage subsystem
- Storage arbitrary key-value pairs
- Support iteration through a range of keys
- Support bulk operations

For the actual storage engine, I went with LevelDB as that supports the above and intitially wrote the entire system against that before extracting the protocol seen below: (*storageops.clj*)

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

Well, you'll notice immediately that there is no trace of any methods for "put/get/delete" operations and that's because it seemed to me that this sort of thing was common between all the storage subsystems and actually was a matter of building up a suitable data structure for flushing to the subsystem via that "*commit!*" method.

Why Reader/Writer/Storage as separate protocols? Because a transaction can be a reader and a writer, and the core storage can be a reader and a writer as well (operations outside of a transaction).

**Transaction lifecycle**

So, we actually end up with another module, "*storage.clj*" to keep these operations over the top of the actual storage mechanism.

So what happens is

- ensure-transaction returns a map for writing to
- the storage module contains methods to manipulating this map
- commit! is called with the original map + any changes

**Core Storage Operations**

The storage module exposes the actual functions used to communicate with the underlying storage system, and also surfaces an "ensure-transaction" function.

    (defn ensure-transaction [storage]
      (assoc (s/ensure-transaction storage)
          :last-synctag (:last-synctag storage)))

Any operations happening within this transaction are managed by the storage module and are just a matter of

- Putting things into that map
- Taking things out of that map
- Loading things from the underlying storage if a version doesn't exist in the map

For example, retrieving an item from that map:

    (defn get-obj [ops id]
      (let [cached (get-in ops [:cache id])]
        (if (= cached :deleted) nil
          (or cached (s/from-db ops id)))))

or marking an item as deleted:

    (defn delete [ops id]
      (assoc-in ops [:cache id] :deleted))

**Zero mutation until flush**

Because all the operations taking place are taking place over the top of a plain old Clojure Map, the code written on top of this is easy to follow as there is no incidental mutation, and it's easy to dump out the contents of the map in the REPL to see what is going on.

To write a document from a module in the database, as well as some metadata and some useful marker for example, we can take a transaction from the underlying storage and do

    (-> (s/ensure-transaction db)
        (s/store "doc-1" { :id "foo" })
        (s/store "metadata-doc-1" { :owner "bob" })
        (s/store "last-change" (now))
        s/commit!)

The majority of the logic here is just putting things into a map, and indeed the only code that actually touches the underying storage is the commit call in this instance. This is using data as integration between the different layers of the database and is quite simple to follow and understand.

**Related files**

- storageops.clj
- storage.clj

