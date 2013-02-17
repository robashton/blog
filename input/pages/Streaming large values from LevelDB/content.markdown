We've uncovered that we can do [atomic writes](/entries/basic-operations-with-leveldb.htm), [consistent reads](/entries/transactional-guarantees-on-top-of-leveldb.html) and [implement transactions](/entries/writing-a-transaction-manager-on-top-of-leveldb.html) on top of LevelDB, but we've got another need in RavenDB which is that we have large objects inside RavenDB and we don't always want to load them into managed memory as a whole unit.

Why not? Because large objects can wreck havoc with the garbage collector, and we also have streaming APIs in RavenDB that allow us to limit this sort of thing that we'd like to honour.

So, back to C++ as we answer the question "What about large objects inside LevelDB"?

Looking at the APIs in our first entry, we can see that "Get" will load entire value into memory ala this:

    db->Get(leveldb::ReadOptions(), key, &document);

We *could* simply read bytes from this document (remember, it's an std::string) and marshal them through to C# bit by bit, but that means copying the whole document out of the store before doing this.

That's where Slice comes in.

      leveldb::Iterator* it = this->db->NewIterator(leveldb::ReadOptions());
      for (it->Seek(key); it->Valid() && it->key().ToString() <= key; it->Next()) {
        Slice* slice = it->value();
        
        // Do stuff with a slice
      }

This is quite cool, LevelDB gives us the notion of a "Slice", which is merely a pointer to the beginning of the value, and the length of the value.

A "Get" operation merely copies this data into an std::string, and calling slice->ToString() will do this as well.

What we can do is 

    char buffer[1024];
    memcpy(slice->data(), buffer, 1024);


Rinse and repeat, we can actually do this within an enumerator and send appropriately sized chunks aross to C# to be processed without copying the whole thing out at once.

Whether this is necessary or not..., well it's nice to know we have the flexibility to touch and manipulate the memory as we need to.

Great, now we just need support for secondary indexes, and for this I'll write a rudimentary document store.
