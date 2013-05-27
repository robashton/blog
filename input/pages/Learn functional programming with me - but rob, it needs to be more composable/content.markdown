As I write my Clojure and the related blog posts, I've been having them reviewed by [@bodil](http://twitter/bodil) who is a proper functional programming neckbeard emacs type.

Her most recent piece of feedback was

  <blockquote>
    What strikes me about your code right now is that it's not very composable
  </blockquote>

Well I have to admit I had no idea what she meant by this, although it was in reference to a problem I was highlighting myself of *I'm passing too much data around*

It turns out that this is pretty much what the FP guys mean when they say that code isn't composable.

One of the beautiful things about the way languages like Clojure make you build up code, is that the functions you write are supposed to be re-usable across very basic and well-known data structures.

Lists and sequences are probably the best structures to be operating on top of, maybe with some primitives as directives of how to operate on them. 

User defined structures such as a "vector" probably come in next and then my big chunk of state as a hash... well that's the worst - especially if I'm passing it around as one big thing all the time.

With that in mind, suddenly we have a use for defined data structures like records (something I have been putting off for a while), and perhaps I can begin to solve my awkward relationship with that big ball of state. Something I have been muttering about for the past few blog entries.

So, I've read the [page on types](http://clojure.org/datatypes) in Clojure and from what I understand it seems that deftype is for structures with no behaviour, and defrecord is for more OOP scenarios.

Apparently I have to read about [defprotocol](http://clojure.org/protocols) too because that'll help with my collision code.

At this point, my brain is melting a bit so I go to find beer. Nice cool beer.
