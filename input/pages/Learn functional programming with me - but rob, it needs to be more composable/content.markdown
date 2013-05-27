As I write my Clojure and the related blog posts, I've been having them reviewed by [@bodil](http://twitter/bodil) who is a proper functional programming neckbeard emacs type.

Her most recent piece of feedback was

  <blockquote>
    What strikes me about your code right now is that it's not very composable
  </blockquote>

Well I have to admit I had no idea what she meant by this, although it was in reference to a problem I was highlighting myself of *I'm passing too much data around*

It turns out that this is pretty much what the FP guys mean when they say that code isn't composable.

Obeerne of the beautiful things about the way languages like Clojure make you build up code, is that the functions you write are supposed to be re-usable across very basic and well-known data structures.

### Composability and Types

[Sequences](http://clojure.org/sequences) are probably the best structures to be operating on top of, maybe with some primitives as directives of how to operate on them. 

User defined structures such as a "vector" probably come in next and then my big chunk of state as a hash... well that's the worst - especially if I'm passing it around as one big thing all the time.

With that in mind, suddenly we have a use for defined data structures like records (something I have been putting off for a while), and perhaps I can begin to solve my awkward relationship with that big ball of state. Something I have been muttering about for the past few blog entries.

So, I've read the [page on types](http://clojure.org/datatypes) in Clojure and from what I understand it seems that deftype is for structures with no behaviour, and defrecord is for more OOP scenarios but there is also [defprotocol](http://clojure.org/protocols) which is somehow related to both and will probably help me with my current data-ball-of-mud problem.

To further confound me, the docs have snippets like this in them

  <blockquote>
  Protocols are fully reified
  </blockquote>

I'll need a dictionary and a copy of SICP to understand this and my brain is melting a bit so I go to find wine. Nice cool wine on a terrace overlooking St Pauls Cathedral and try to make sense of these docs a bit more.

### Composability and Space Invaders

Starting at the other end of the problem - away from wordy confusion, it's very easy to see that there are some basic data types in my Space Invaders game and I have some basic operations over the top of them.

First up, I'm passing 'rects' all over the place, and a rect is defined as 

    x : LHS of the rect
    y : TOP of the rect
    w : Width of the rect
    h : Height of the rect

That's the state, and there are some basic operations over the top of this - some which involve mutation and some which do not

- Draw it (no mutation of internal state) 
- bottom of the rect (no mutation)
- RHS of the rect (no mutation)
- move left/right/up/down (mutation - perhaps just 'move')

From what I understand of the docs, I could probably get a lot done by defining a rect as a record, and exposing these as methods on that record, this might make some of the code easier to follow.

I could also use a protocol to describe two things as being 'collideable' (a suggestion from my tutor), which is the Clojure equivalent to an interface if I understand correctly.

### No code here

I'll leave the implementation of this to the next post, as this is quite a lot of data to digest and I highly recommend reading the above docs if you're following along with my learning.





