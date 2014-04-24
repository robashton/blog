Let's start our little foray into seeing what code I cooked up by looking at how I did the Client API for CRavenDB as it'll give us a good indication as to the sort of features I wanted to support.

- [I wrote a document database in Clojure](/entries/i-wrote-a-document-database-in-clojure.html)

Just like RavenDB I decided that I wanted the same interface for talking to the database regardless of whether I was using a remote database over HTTP, an embedded database, or an in-memory database for testing.

For this, it seems that protocols are the best option we have in Clojure as it's essentially what they're for. This also gives me a convenient place to shove documentation and surface the Official Public API.

So I ended up with [this](https://github.com/robashton/cravendb/blob/80314f64f25ff4af8906e7d3117cec9566d80ed0/src/cravendb/database.clj), also listed below without the documentation for brevity.

    (ns cravendb.database)

    (defprotocol DocumentDatabase
      (close [this])
      (load-document-metadata [this id])
      (query [this opts])
      (clear-conflicts [this id])
      (conflicts [this])
      (put-document [this id document metadata])
      (load-document [this id])
      (delete-document [this id metadata])
      (bulk [this operations])
      (put-index [this index])
      (load-index-metadata [this id])
      (delete-index [this id])
      (load-index [this id]))

This is a low level interface obviously, the key operations being

  - put-document
  - load-document
  - delete-document
  - query

The great thing about this low level interface is that I can make various implementations of it, and then pass a "database" around without worrying what it is actually behind that.

So we have the ability to do

    ; In-memory
    (def instance (embedded/create))
    ; Embedded on disk
    (def instance (embedded/create "var/db"))
    ; Remote via HTTP
    (def instance (remote/create "http://example.host:8000"))

And then each of those implementations supports the above operations transparently.

I wanted to support transactions with this database too, so a *bulk* operation is supported which is just a combination of the above operations.

NOTE: Implementation of these and how it's a good idea to break out of the record ASAP or you end up with pain



