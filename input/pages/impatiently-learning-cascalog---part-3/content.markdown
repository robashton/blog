Onwards to Part 3 of the Cascalog for the Impatient tutorial, now my JVM appears to be behaving itself and I'm able to run the examples again.

- [Part 1](/entries/impatiently-learning-cascalog---part-1.html)
- [Part 2](/entries/impatiently-learning-cascalog---part-2.html)

Part 3 seems to be pretty simple, adding a custom function to scrub text - written in a style I can actually understand thanks to the Clojure I've written so far.

```clojure
(defn scrub-text [s]
  "trim open whitespaces and lower case"
  ((comp s/trim s/lower-case) s))
```

The main point of this step of the guide seems to be about data sanitisation, and why you would use custom functions to do that - rather than anything new relating specifically to Cascalog.

Being able to use vanilla Clojure functions to manipulate data comes with some benefits, the main one being that we can test this stuff independently and then import it into the Cascalog query where we know at least that bit works. 

This is pretty important because tidying up edge cases is generally a rather iterative process and you want the feedback loop to be fast (I can get behind that notion)

This is used in the following manner

```clojure
(defn -main [in out & args]
  (?<- (hfs-delimited out)
       [?word ?count]
       ((hfs-delimited in :skip-header? true) _ ?line)
       (split ?line :> ?word-dirty)
       (scrub-text ?word-dirty :> ?word)
       (c/count ?count)))
```

In this query, it's just another step - where we call the function with the output of the split function (which is *?word-dirty*), and then output the result of that as *?word*

This is starting to make sense now, and it no longer looks like vomit when I read it. I'd like to see what is going on under the hood to satisfy my curiosity but that can wait until I've finished the tutorial.

Onwards to Part 4...
