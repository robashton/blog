So, in our little foray into LevelDB, we've uncovered the following:

- We can do [atomic writes](/entries/basic-operations-with-leveldb.htm)
- We can have [consistent reads](/entries/transactional-guarantees-on-top-of-leveldb.html), 
- We can create a [transaction manager](/entries/writing-a-transaction-manager-on-top-of-leveldb.html) on top of LevelDB 
- We can can [stream data out of LevelDB](/entries/streaming-large-values-from-leveldb.html)
- And we can create [secondary indexes](/entries/secondary-indexes-on-top-of-leveldb.html)

With this, I set about creating a fork of RavenDB with a starting implementation of LevelDB for document storage.

I grabbed the [C# bindings](https://github.com/meebey/leveldb-sharp) from Github and set about re-implementing my in-memory transaction manager on top of them.

The first thing I notice about the C# bindings is that the API looks like this:

    string value = db.Get(string key)
    db.Put(string key, string value)

This is not what [RavenDB wants](/entries/streaming-large-values-from-leveldb.html), what RavenDB wants is a MemoryStream or at the very least a ByteArray - as it is not in the business of pushing large strings around the place in managed memory.

No problem, I fork the original project and set about addding Byte array support and then notice something.

*The bindings are using the C API for LevelDB* (which makes sense if you're going to support cross platform C#), but more importantly the C API looks something like this:

    char* leveldb_get(char* key)

See that return result? That's a malloc, what does the LevelDB C# code do?

     var value = Marshal.PtrToStringAnsi(valuePtr, (int) valueLength);
     return value;

Ignoring the [memory leak](https://github.com/meebey/leveldb-sharp/issues/2), as it's a known issue and an easy mistake to make, we can see we're copying the data out of the unmanaged array into a managed string via the PtrToStringAnsi call.

What does the C api do?
    
    Status s = db->rep->Get(options->rep, Slice(key, keylen), &tmp);
    if (s.ok()) {
      *vallen = tmp.size();
      result = CopyString(tmp);
    } else {
      *vallen = 0;
      if (!s.IsNotFound()) {
        SaveError(errptr, s);
      }
    }
    return result;
    
And CopyString?

  
    static char* CopyString(const std::string& str) {
      char* result = reinterpret_cast<char*>(malloc(sizeof(char) * str.size()));
      memcpy(result, str.data(), sizeof(char) * str.size());
      return result;
    }

*urk*


In order to get a stored document from LevelDB, the following operations are performed:

    - Internally, LevelDB will load the document into memory
    - Internally, LevelDB will copy that memory into a string on a Get call
    - The C API copies that memory into a mallocced byte array
    - The C# API copies that memory into a managed string (probably on the Large Object Heap)


What we have here, is a C# wrapper around a C API that is a wrapper around a C++ API, and each of them is trying to be semantically equivalent to the underlying API - and the abstraction is *killing us*.

For small values, this probably isn't a big deal, but RavenDB documents can be any size, and RavenDB documents often *are* any size - ideally what we want is


    MemoryStream stream = db.Get(string key)
    

Where that stream reads from the start of an unmanaged array to the end of that unmanaged array and then disposes it.

In essence, the job of getting LevelDB into RavenDB is going to involve writing a new C API for LevelDB specific to our needs, then writing a C# API around that API specific to our needs.

Thankfully we need about 10% of the exposed functionality in LevelDB so our job isn't as hard as "writing a set of managed bindings for LevelDB", but this means that I won't have time to do this before I leave Israel.

I put my sad face on and get to work on something more manageable for my final day of work :-(

