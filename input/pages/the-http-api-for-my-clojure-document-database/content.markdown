I mentioned in the last post that I had decided on a common set of protocols for a client wishing to talk to the database, regardless of whether it was in-memory, remote or on embedded on disk.

- [I wrote a document database in Clojure](/entries/i-wrote-a-document-database-in-clojure.html)
- [The Client API for my Clojure document database](/entries/the-client-api-for-my-clojure-document-database.html)

**Recap**

This took the form of a protocol that looked like this:

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

It's fairly well established that document database operations map well into a restful interface - in the above mappings we only have two resources,

- /document (PUT/GET/DELETE/HEAD)
- /index    (PUT/GET/DELETE/HEAD)

With a convenience "bulk" endpoint which we'd use if we wanted to do multiple operations in a single transaction.

**Enter Liberator**

It just so happens that there is a great library available for Clojure (built on top of some other decisions (ring) made by the clojure community) which enforces valid http over the notion of resources. This library is called "[Liberator](http://clojure-liberator.github.io/liberator/)"

You can describe a resource and it'll manage all the appropriate HTTP operations for you:


    (resource
      :allowed-methods [:put :get :delete :head]
      :put! some-handler
      :delete! some-handler
      :handle-ok some-handler)


**And HTTP-Kit**


By itself, liberator isn't really enough to talk to the world over HTTP, as you need a http server to actually host it.

I used HTTP Kit because it supports asynchronous operations and seems to be getting the love these days. What this means, is you create some handlers using liberator and pass them into the 'run-server' function from http-kit.

    (run-server handlers)

**So I have an embedded database**

I don't want these routes I've defined to be coupled to either

- The HTTP server hosting them (http-kit in this case)
- The type of database being used (in-memory or otherwise)

So what I did is created a closure, which takes in the instance of database to talk to and returns the routes defined around that instance

    (defn create-http-server [instance]
      (let [db-routes (create-routes instance)]
        (handler/api db-routes)))

I can then use that in tests, the REPL or the main application to create a http server around an instance of a database

This is what my main application looks like for example.

    (run-server
      (http/create-http-server embedded-instance) { :port 8001 :join? true }))


**The routes themselves**

    (defn create-routes [instance]
      (routes
        (ANY "/document/:id" [id]
          (resource
            :allowed-methods [:put :get :delete :head]
            :exists? (fn [ctx] (resource-exists
                                ctx
                                #(db/load-document instance id)
                                #(db/load-document-metadata instance id)))
            :available-media-types accepted-types
            :etag (fn [ctx] (etag-from-metadata ctx))
            :put! (fn [ctx] (db/put-document instance id (read-body ctx) (read-metadata ctx)))
            :delete! (fn [_] (db/delete-document instance id (read-metadata _)))
            :handle-ok (fn [_] (standard-response _ (::resource _) (::metadata _)))))
        (ANY "/index/:id" [id]
          (resource
            :allowed-methods [:put :get :delete :head]
            :exists? (fn [ctx] (resource-exists
                                ctx
                                #(db/load-index instance id)
                                #(db/load-index-metadata instance id)))
            :available-media-types accepted-types
            :etag (fn [ctx] (etag-from-metadata ctx))
            :put! (fn [ctx] (db/put-index instance (merge { :id id } (read-body ctx))))
            :delete! (fn [_] (db/delete-index instance id))
            :handle-ok (fn [_] (standard-response _ (::resource _) (::metadata _) ) )))

        ;; ETC ETC ETC ETC


They're a bit more complicated than any demo because

- The database support Etags for versioning
- An exists? check is needed and we cache the results
- The database will return json/edn/html depending on the requester (standard-response)

Other than that, the routes are just a wrapper around the db operations already described in the previous entry.

**What did we learn**

Liberator is a really tidy way of wrapping up something and exposing it over HTTP, and that we can pick and choose HTTP servers to go with this is the icing on the cake. This is something I liked about this eco-system for sure. (See 'connect' in node or 'OWIN' in .NET)

Next up, we'll look at the core storage protocols in the document database and how that helped me write different storage mechanisms.

**Related Files**

- http.clj
- database.clj


