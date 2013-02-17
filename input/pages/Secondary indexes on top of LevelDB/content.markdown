Got [atomic writes](/entries/basic-operations-with-leveldb.htm), [consistent reads](/entries/transactional-guarantees-on-top-of-leveldb.html), [transactions](/entries/writing-a-transaction-manager-on-top-of-leveldb.html) and we can [do streaming](/entries/streaming-large-values-from-leveldb.html) if we need to - now if we can do secondary indexes we can move forwards with creating a storage implementation for RavenDB.

An example of a couple of indexes we need in RavenDB is:

  - The document itself stored by key (primary index)
  - The document key stored by etag (secondary index)

Whenever we write a document in RavenDB, we want to

  - Remove the old Etag (or at least update it)
  - Write the document against its key
  - Write a new etag for the document

(Actually, a bit more goes on than this as we store meta data for headers etc, but this will do for now)

The operation we want to do when indexing is

  - What are all the new etags since last we indexed?
  - Loop through each of these, load the document and index it
  
Etags are sortable by design, in my rough and ready document database the etag is just an integer that I increase every time I add a new document.

Now, when writing and reading from the database, we need to be able to differentiate between types of key in the database, and ordering for each of these types is set by the key.

*For example, reading a document*

    #define DOCPREFIX "/docs/"
    #define ETAGPREFIX "/etags/"


    void Get(std::string id, std::string* doc) {
      std::stringstream st;
      st << DOCPREFIX;
      st << id;
      std::string key = st.str();
      this->lastStatus = this->db->Get(leveldb::ReadOptions(), key, doc);
    }

So I'm sticking a prefix in front of each key to denote what sort of key it is, yay.

That means that my secondary index of etags will end up looking like this

    /etags/0 -> /docs/1
    /etags/1 -> /docs/5
    /etags/2 -> /docs/4
    /etags/3 -> /docs/6
    /etags/4 -> /docs/3
    /etags/6 -> /docs/7
    /etags/8 -> /docs/12
    /etags/9 -> /docs/5


When I want to index documents newer than /etags/5, I can do

    int IndexNewDocumentsNewerThan(int etag) {
      std::stringstream st;
      st << ETAGPREFIX;
      st << etag;
      std::string startkey = st.str();

      leveldb::Iterator* it = this->db->NewIterator(leveldb::ReadOptions());
      for (it->Seek(startkey); it->Valid() && this->IsEtagKey(it->key().ToString()); it->Next()) {
        std::string document = it->value().ToString();
        IndexDocument(value);
      }
      delete it;
    }

Building up other indexes is a trivial matter of making sure we have a suitable ordering in place, and queries are simply a matter of building up these keys appropriately.

With all of this, I think we have enough information to go and build persistence for RavenDB on top of LevelDB (and then test test test, performance performance performance etc).

If you want to check out the spikes and playarounds with LevelDB, you can find them on my [Github](https://github.com/robashton/leveldb-play)

