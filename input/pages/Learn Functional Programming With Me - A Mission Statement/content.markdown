I was at NDC2012 last week and ended up hanging around the tongue-in-cheek "Hipster stand" along with a fine bunch of people who sat around coding and chatting most of the week on various interesting projects.

One of those fine people was [@bodiltv](https://twitter.com/bodiltv) who proclaimed their love for Lisp, Emacs and various other things that I thought (like most people) I'd left behind after university never to touch again.

However, functional programming is something that is seeing somewhat of a re-emergence in the crowds of people who I tend to lump into a bucket of "Smarter than Rob", and I was looking for an excuse to give it a go - having to build something with somebody sat next to me to show me the ropes was an excellent motivator.

Over the course of a couple of hours on Thursday, [@bodiltv](https://twitter.com/bodiltv) and I sat down and wrote a simple little script that looks something like this:


    (ns foo.core 
    (:use [net.cgrand.enlive-html :as html])  
    )
  
    (def BASEURL "https://en.wikipedia.org/wiki/")
  
    (defn fetch-url [url]
      (html/html-resource (java.net.URL. url)))
  
    (defn define
      "Fetches a page from wikipedia and prints the first paragraph"
        [q]
          (let [url (str BASEURL q)
               content (fetch-url url)]
               ((comp html/text
                      first
                      html/select) content [:#mw-content-text :p])
          )
    )
    

We also then looked at futures and how such things like that could help us, and with the knowledge of state being immutable know that we were safe when doing so. I then asked the question that most people ask when encountering this stuff for the (first/second) time, "How on earth do you build something *real* with this stuff".

So here goes, a mission statement - *I am going to build space invaders on top of HTML5 Canvas with Clojurescript*.

Yes, I am breaking out the blog again and relying on YOU, the reader who knows more about these things than I do to guide me on my way as I inevitably make lots of mistakes.

It's hard to do this, because on the surface of things it seems so far away from the style of development I am used to, but by learning out in the open I am hoping we'll accelerate the process of getting to the point where I can be productive in a functional environment.

Next entry: Let's see what my environment is going to look like, how I am going to interact with the Canvas and structure myself during this experiment.
