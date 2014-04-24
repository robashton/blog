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

    (-> (t/open instance)
        (t/store "doc-1" { :message "hello world" })
        (t/store "doc-2" { :message "hello alice" })
        (t/store "doc-3" { :message "hello bob" })
        (t/delete "doc-4")
        (t/commit!)

I only have two implementations of this and they're actually pretty empty because they merely farm out into the code that really does something.

### The remote implementation

I ended up using a couple of packages from Clojars to do the hard work for me here

- *[http.async.client](https://github.com/neotyk/http.async.client)*: This was an arbitrary choice, I just wanted a HTTP client that worked and supported async
- *[cemerick.url](https://github.com/cemerick/url)*: I'm only using this for url encoding, but it seemed more sensible than trying to use the Java ones

I have some pretty hideous functions for building URLs, there are some jars for this, I should have probably used them.

    (defn url-for-doc-id [url id]
      (str url "/document/" id))
    (defn url-for-index-id [url id]
      (str url "/index/" id))
    (defn url-for-bulk-ops [url]
      (str url "/bulk"))
    (defn url-for-conflicts [url]
      (str url "/conflicts"))
    (defn url-for-conflict-id [url id]
      (str url "/conflict/" id))
    (defn url-for-stream [url synctag]
      (str url "/stream?synctag=" (or synctag "")))

An actual operation in the record (others removed for brevity)

    (defrecord RemoteDatabase [url]
      DocumentDatabase
      (close [this])

      (query [this opts]
        (with-open [client (http/create-client)]
          (force-into-list
            (process-response
              (http/GET client (url-for-query url opts)
                        :headers default-headers
                        :query (dissoc opts :filter :index))))))

I really like the brevity of Clojure for this. The "close" isn't really needed for this implementation so it's an empty function that returns nothing. The rest of the operations look the same, a http request and the processing of that http request. Lovely.

### The embedded implementation

The embedded implementation is obviously the heart and soul of the whole database, everything comes through this (it sits behind the HTTP interface we'll see in the next post).

    (defrecord EmbeddedDatabase [storage index-engine ifh counters]

The constructor for this record takes in the underlying storage engine, index engine, the in-flight transaction system and some performance counters. This wasn't really what I had in mind when I through it together, but the [code itself](https://github.com/robashton/cravendb/blob/master/src/cravendb/embedded.clj#L24) is quite concise as it mostly just farms out the work to modules responsible for managing document operations, indexing operations and etc.

### Lessons learned about Protocols and Records

It seems from this (and it carries across into other places I've used protocols too). I tend to end up using a protocol for the polymorphism and the records to hold some handles/state and then delegate the work out into pure functions.

I could probably have used multi-methods for this (based on some property in the state), but I found them to be a bit messy because it meant bundling several implementations in the same file. I quickly moved away from the attempts where I did this when it got hard to follow.

Apparently protocols are also faster, but given performance was not really one of my goals I doubt that is a bottleneck in the database.





