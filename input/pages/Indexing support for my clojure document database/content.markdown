What is a document database without the ability to query?

- [I wrote a Clojure document database](/entries/i-wrote-a-document-database-in-clojure.html)
- [The Client API for my Clojure document database](/entries/the-client-api-for-my-clojure-document-database.html)
- [The HTTP API for my Clojure document database](/entries/the-http-api-for-my-clojure-document-database.html)
- [The core storage protocols for the Clojure document database](/entries/the-core-storage-protocols-for-my-clojure-document-database.html)
- [LevelDB storage implementation for the Clojure document database](/entries/the-leveldb-storage-for-my-clojure-document-database.html)
- [In-memory storage for the Clojure document database](/the-inmemory-storage-for-my-clojure-document-database.html)
- [Document storage in the Clojure document database](/entries/document-storage-in-my-clojure-document-database.html)

**What is an index?**

An index, is just something that transforms a potentially complex document into something that can be put into our index store. For example, if we had the following document and we wanted to be able to search for ponies by the town in which they live.

    { name: "Pinkie Pie"
      address: {
        :town "Ponyville"
      }}

We might write the following map function

    (defn pony-by-town [pony] { "town" (get-in pony [:address :town])})

This needs storing in the database too, as it will need to go through all of these indexes as documents are written/modified and execute them before putting their results into lucene. Sadly there is no "tidy" way to serialize functions in Clojure and while I now know about macros and could probably make them do it for me, I opted for taking in strings representing these index functions.

    { :id "ponies-by-town"
      :map "(fn [doc] { \"town\" (get-in doc [:address :town])})"
    }

These have their own GET/PUT/DELETE methods in the HTTP API and Document API and are treated just like documents.

**The index store**

Again, no way am I writing a secondary index store from scratch and just like RavenDB did, I'm reaching for Lucene to provide these capabilities.

Looking around, there are a few half-finished wrappers for Lucene written in Clojure - presumably to the point where they did what the authors needed and then no further. Code owned is often better than code borrowed and I therefore decided to write my own domain specific wrappers of Lucene.

    (:import
              (org.apache.lucene.analysis.standard StandardAnalyzer)
              (org.apache.lucene.store FSDirectory RAMDirectory)
              (org.apache.lucene.util Version)
              (org.apache.lucene.index IndexWriterConfig IndexWriter DirectoryReader)
              (org.apache.lucene.search IndexSearcher Sort SortField SortField$Type)
              (org.apache.lucene.queryparser.classic QueryParser)
              (org.apache.lucene.document Document Field Field$Store Field$Index
                                          TextField IntField FloatField StringField)))

Very classes. Much namespace. Wow.

**Some more protocols**

While in the rest of the database, protocols have been used for convenient polymorphism, in this case they were used to provide very thin wrappers over Lucene (hiding the immense object construction required to build up the various reader/writer/indexes) and ensuring the consistent use of Lucene.

    (defrecord LuceneIndexWriting [writer analyzer]
      java.io.Closeable
      (close [this]
        (.close writer)))
    (defrecord LuceneIndexReading [reader analyzer]
      java.io.Closeable
      (close [this]
        (.close reader)))
    (defrecord LuceneIndex [analyzer directory config]
      java.io.Closeable
      (close [this]
        (.close directory)))


And lo, the following factory methods were born.

    (defn open-writer [index] (LuceneIndexWriting.
                          (IndexWriter. (:directory index) (:config index))
                            (:analyzer index)))

    (defn open-reader [index] (LuceneIndexReading.
                          (DirectoryReader/open (:directory index)) (:analyzer index)))

    (defn create-index [file]
      (let [analyzer (StandardAnalyzer. Version/LUCENE_CURRENT)
            directory (FSDirectory/open file)
            config (IndexWriterConfig. Version/LUCENE_CURRENT analyzer) ]
        (LuceneIndex. analyzer directory config)))

    (defn create-memory-index []
      (let [analyzer (StandardAnalyzer. Version/LUCENE_CURRENT)
            directory (RAMDirectory.)
            config (IndexWriterConfig. Version/LUCENE_CURRENT analyzer)]
        (LuceneIndex. analyzer directory config)))

Not much to it, but how do we write to this?

**Writing to our indexes**

The only important function really, is this.

    (defn put-entry [index ref-id content]
      (let [doc (Document.)]
        (doseq [f (map-to-lucene content)] (.add doc f))
        (.add doc (document-id-field ref-id))
        (.addDocument (:writer index) doc))
      index)

So, somehow we'll need to go through all of our documents as they are written/modified and execute all the indexes we have against them before calling put-entry and storing the results in Lucene.

That's a job for the indexing engine, but we'll leave that for now as it's one of the more complicated pieces of code and will need a bit of explanation.

**Summary**

Again, interop with Java is a useful tool if we want to make the most of the existing OSS ecosystem. Obviously the Java interface to Lucene is butt ugly and hiding it from the rest of the database within this *lucene.clj* module makes sense and that's exactly what I've done.

**Related Files**

- indexes.clj
- lucene.clj

