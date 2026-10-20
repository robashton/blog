My client for the next 8-9 days is going to be [Mastodon C](https://www.mastodonc.com/), who have kindly agreed to let me work for them in order to get some real-world Clojure-ish experience and pick up whatever other skills are there to be picked up while I'm at it in the field of data anlytics.

[@otfrom](http://twitter.com/otfrom) has pointed me in the direction of "[Cascalog for the Impatient](https://github.com/Cascading/Impatient-Cascalog)" and a the "[CDEC Open Health Data Platform](http://openhealthdata.cdehub.org/) (mapping diabetes across the UK)" in preparation for my week working with them and before I turn up I'm going to go through the tutorial and see what I understand and what I don't understand and write about my thought processes as I do it.

Anybody else following the Cascalog for the Impatient tutorial for the first time might find my notes useful, but at any rate *I'll* be able to go back and look at them myself and that's worthwhile too.

So...

## Part 1

I git cloned the repo, and typed 

    lein uberjar

This built me a jar and I was able to run Hadoop with the following command:

     hadoop jar target/impatient.jar data/rain.txt output/rain

Which did something with the input "rain.txt" and dumped it in the specified output folder.

This satisfied for me that my environment is sane and I could therefore carry on with the tutorial.

First up, we are shown this code with some rough explanation of how it works

```clojure
(defn -main [in out & args]
  (?<- (hfs-delimited out)
    [?doc ?line]
    ((hfs-delimited in :skip-header? true) ?doc ?line)))
```

I find the tutorial at this point to make some assumptions about my knowledge of what is going on here, it states that

- We have a main method that takes in an 'input' and 'output' path
- We can create a *generator* over 'in' using (hfs-delimited)
- We can create a *sink* over 'out' using (hfs-delimited)
- We then name the vars from the generator with the ?doc ?line bits

However, I have a few questions outstanding from this, chiefly

- Wtf is a generator
- Wtf is a sink
- Wtf is up with that *(?<-* thingy
- How are we naming those vars, how come its different for the sink and the generator?

Dumb questions I'm sure - but I'm happy enough to admit my ignorance and press on.

I read the page linked in the impatient docs: [How cascalog executes a query](https://github.com/nathanmarz/cascalog/wiki/How-cascalog-executes-a-query)

And I establish that (at least as a working hypothesis)

- A generator is just a predicate that gives you data?
- A sink is somewhere the output can be put (a query can just have an output specified and that's it)
-  The (?<- thingy is like the (<- thingy, and what they are is simply
  - The one without a question mark is a definition of a query
  - the one with a question mark is both the definition and an execution of a query
- the naming of the vars - ... is just "because", I'll take that at face value and move on

# The result of running this thing

I see that in the output folder there is a file called part-0000 which has the same content as the input file. So it looks like our generator and our sink are effectively the same thing and we're just streaming the data from one place to another without changing it.

Okay then, I guess I'll look at part two next and see if anything I've made a guess about here is right at all.
