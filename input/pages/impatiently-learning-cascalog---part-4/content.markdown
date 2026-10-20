I'm on Part 4 of Cascalog for the Impatient and I've learned

- I still don't enjoy the JVM that much
- How to use generators and sinks 
- What the *?<-* and *<-* thingies do (roughly)
- How to define map operations
- How to use vanilla Clojure to clean stuff up

Great, so this is where we are.

- [Part 1](/entries/impatiently-learning-cascalog---part-1.html)
- [Part 2](/entries/impatiently-learning-cascalog---part-2.html)
- [Part 3](/entries/impatiently-learning-cascalog---part-3.html)

I've opened up the source for part4 and it is doing something a bit different so I guess I should be a little less impatient and have a look at the tutorial to see what I'm looking at...

    Today's lesson takes that same Word Count app and expands on it to implement a stop words filter.

There is some waffle about the TF-IDF algorithm doing some of this for us, but we might want to do it ourselves because of swear-words. I suppose I should go and read about TD-IDF at some point but for now I'm going to move on with this Cascalog stuff and accept that stuff exists and I don't know about it yet.

# Left join

Feasibly we could write a custom filter method and check if a word is valid, but that's going to be a bit of a ball-ache. So, we're told that we're doing a left-join between the two sets of data.

That's an interesting notion to me because we're in the world of Map/Reduce and that's not something I'm used to reading in this world.

So how does this work?

```clojure
  (let [rain (hfs-delimited in :skip-header? true)
        stop (hfs-delimited stop :skip-header? true)]
```

Unlike  in the previous codes, rather than simply invoke the generator with the variables, we stick it in a let block alongside another generator containing stop words.

At this point in my mind I'm viewing them as two tables of data, ripe for *doing stuff with*.

The next line we start going about things the way we're used to

```clojure
    (?<- (hfs-delimited out)
         [?word ?count]
```

So we're sinking words and counts into the destination file again - that's cool.

We then invoke the rain generator and tidy it up like in the past two examples

```clojure
    (rain _ ?line)
    (split ?line :> ?word-dirty)
    ((c/comp s/trim s/lower-case) ?word-dirty :> ?word)
```

I can't really see why we wouldn't just write this with the generator inline like this

```clojure
    ((hfs-delimited in :skip-header? true) _ ?line)
    (split ?line :> ?word-dirty)
    ((c/comp s/trim s/lower-case) ?word-dirty :> ?word)
```

Other than I guess declaring the generators at the top just makes it clear to the reader where our data is coming from.

The interesting bit is where we then invoke the stop word generator wit hteh word that we have
         
```clojure
(stop ?word :> false)
```

Well wow, I'm not really sure what I'm looking at here at all, I guess there is some more magic going on where it knows I've used the *?word* as a field and it can work out that I want to do a join on these two things based on that. I'm mapping the result of this to 'false' which I assume means *I don't want that crap if it returns something*.

The major difference between the files I'm looking at here (the stop file, and the rain file) is that the rain file has two columns and the stop file has one column so it has to just be the fact that I used the same name here that made the magic happen.

I don't find any of this intuitive - but I can take it at face value once more and accept that it works if I do it this way. My urge to go and see how this stuff is implemented is growing and there are only two more stages of this tutorial left for me to follow.

The complete query looks like this by the way

```clojure
(defn -main [in out stop & args]
  (let [rain (hfs-delimited in :skip-header? true)
        stop (hfs-delimited stop :skip-header? true)]
    (?<- (hfs-delimited out)
         [?word ?count]
         (rain _ ?line)
         (split ?line :> ?word-dirty)
         ((c/comp s/trim s/lower-case) ?word-dirty :> ?word)
         (stop ?word :> false)
         (c/count ?count))))
```

Running the demo with

    ~/build/hadoop-1.2.1/bin/hadoop jar target/impatient.jar data/rain.txt output/wc data/en.stop

Seems to yield in a sensible word count being generated like so

    air             1
    area            4
    australia       1
    broken          1
    california's    1
    cause           1

etc

So, I guess the lesson here is that you can do joins between generators by using the same field name when running them. Neato.

At this point I have to go away for the weekend to get drunk in a field, so I'll have to skip the rest of the tutorial and start work on something real at the Mastodon C office.
