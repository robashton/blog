We've uncovered that we can do [atomic writes](/entries/basic-operations-with-leveldb.htm), [consistent reads](/entries/transactional-guarantees-on-top-of-leveldb.html) and [implement transactions](/entries/writing-a-transaction-manager-on-top-of-leveldb.html) on top of LevelDB, but we've got another need in RavenDB which is that we have large objects (documents) inside RavenDB and we don't always want to load them into managed memory as a whole unit (instead, we want to stream them into a set of smaller objects)

Why don't we want to load them into memory in one go? Because large objects can wreck havoc with the garbage collector (the large object heap), and we also have streaming APIs in RavenDB that allow us to limit this sort of thing that we'd like to honour.

So, back to C++ as we answer the question "What about large objects inside LevelDB"?

Looking at the APIs in our first entry, we can see that "Get" will load entire value into memory ala this:

    db->Get(leveldb::ReadOptions(), key, &document);

We *could* simply read bytes from this document (remember, it's an std::string) and marshal them through to C# bit by bit, but that means copying the whole document out of the store before doing this. This isn't a big deal as it's still completely on the native side at this point, but it's still an expense I'd prefer to skip if we're reading more than a few documents.

That's where Slice comes in.

      leveldb::Iterator* it = this->db->NewIterator(leveldb::ReadOptions());
      for (it->Seek(start_key); it->Valid() && it->key().ToString() <= end_key; it->Next()) {
        Slice* slice = it->value();
        
        // Do stuff with a slice
      }

This is quite cool, LevelDB gives us the notion of a "Slice", which is merely a pointer to the beginning of the value, and the length of the value.

A "Get" operation merely copies this data into an std::string, and calling slice->ToString() will do this as well.

What we can do is 

    char buffer[BUFFER_SIZE];
    memcpy(slice->data(), buffer, BUFFER_SIZE);

Rinse and repeat, we can actually do this within an enumerator and send appropriately sized chunks aross to C# to be processed without copying the whole thing out at once.  This could be useful when iterating over a set of documents, although most likely simply copying into the string and using string.c_str() to get the pointer to the copied data will give us easier code - we'll see how that works out when it comes to actually writing the C# we need.

Anyway, now we just need support for secondary indexes, and for this I'll write a rudimentary document store.
