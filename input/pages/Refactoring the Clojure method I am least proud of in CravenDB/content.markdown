I've had a bit of a duck in my [database](http://github.com/robashton/cravendb) for a while (and I'm not making reference to the read-strings that were in there for months while I got on with more important things).

The duck came in the form of this method

```clojure
(defn perform-query 
  [tx reader query offset amount sort-field sort-order]
  (loop [results ()
         current-offset offset
         total-collected 0
         attempt 0 ]
         (let [requested-amount (+ current-offset (max amount 100))
               raw-results (lucene/query reader query requested-amount sort-field sort-order)
               document-results (convert-results-to-documents tx (drop current-offset raw-results))
               new-results (take amount (concat results document-results))
               new-total (count new-results) 
               new-offset (+ current-offset requested-amount)]

           (debug "Requested" requested-amount 
                    "CurrentTotal" total-collected 
                    "Skipped" current-offset "Of"
                    "Received" (count raw-results))
           (if (and (= (count raw-results) requested-amount)
                    (not= new-total amount)
                    (> 10 attempt))
             (recur new-results 
                    new-offset 
                    new-total
                    (inc attempt))
             new-results))))
```

Essentially we have a lucene reader and we want to

- Query it for some results (offset + amount-ish)
- Pull those results through as documents
- If we don't have enough documents and there are some left, query again
- Keep going until either we have enough, or there are no more left

It's fairly simple, but what we can see in the method is

- I'm using 'loop', this usually indicates other non-functional problems
- I'm effectively building 'take', 'skip', etc myself

One thing I've learned so far in Clojure that if you have a abnormal construct (in this case my lucene reader), is that we should try and get that into either a *map* or a *sequence* as early on as possible so as to be able to leverage all the Clojure built-ins on it.

### What I want here, is a lazy sequence built up with recursion

I wrote a quick proof of concept of that sort of thing, it looks like this:

```clojure
(defn number-seq 
  ([] (number-seq 0))
  ([i]
  (cons (inc i) (lazy-seq (form-sequence (inc i))))))
```

What I have here is a function I can call with all the usual suspects like

```clojure
(take 100 (number-seq)) ;; Get me 0 to 100
(drop 10 (take 100 (number-seq)) ;; Get me 10 to 100
;; etc
```

And it's fully lazy and potentially infinite and all those things. The whole "cons a value onto a lazy sequence" trick is the solution to quite a few of the Clojure koans and had I done them perhaps I'd not have written the above loop.

### First step - reduce the arguments

Before ripping apart this loop, I thought it better to jump in the repl to create the constructs that go into the loop, then sit there and pull the loop apart into its constituent parts before using them in a recursive function.

Looking at the query method, I'm passing quite a lot in to it

- tx: the current transaction we're pulling data from
- reader: a wrapper around a lucene reader
- query: the query itself
- offset: the number of values to skip in the page
- amount: the number of values to pull from the reader
- sort-field: the field we're sorting on
- sort-order: the order of sorting

It is quite clear that only two of these values actually change during the recursive loop, and the rest are only used with a single object (reader) the same way over and over again.

My first step in C# would probably be to "create another object", in this case here though a producer function of some sort is what springs to mind

```clojure
(defn lucene-producer [tx reader query sort-field sort-order]

  (fn [offset amount]
    (->> 
      (lucene/query reader 
                    query 
                    (+ offset amount) 
                    sort-field 
                    sort-order) 
      (drop offset) 
      (convert-results-to-documents tx))))
```

I actually go a bit further and bring in the loading of the documents as well, now I can call this over again like so

```clojure
(producer 0 100) ;; Get me the first 100 items in the index
(producer 10 100) ;; Get me the items 10-100 in the index
```

Now of course, this producer function might not actually return the requested number of results because the underlying code is doing de-duping and removing documents that have been deleted and not yet cleared from the index.

I slot this into my original loop function, verify it all still works and then look at the next step:

### Second step - split out the paging

What I really want is the ability to skip through pages of the index and not keep accumulators around in the loop itself, I can employ a similar trick and write a function that knows about the current page and how to get the next page. I'll store the results in a map and a function to get the next page in the map as well.


```clojure
(defn lucene-page 
  ([producer page-size] (lucene-page producer 0 page-size))
  ([producer current-offset page-size]
   {
    :results (producer current-offset page-size)
    :next (fn [] (lucene-page producer (+ current-offset page-size) page-size))
   }))
```

An alternative might be to just assoc the paging values into the map itself and create a next-page function like so

```
(defn lucene-page 
  ([producer page-size] (lucene-page producer 0 page-size))
  ([producer current-offset page-size]
   {
    :producer producer
    :offset current-offset
    :page-size page-size
    :results (producer current-offset page-size)
   }))

(defn next-page [{:keys [producer offset page-size]}]
  (lucene-page producer (+ offset page-size) page-size))
```

But I quite like the little function and it's smaller so I'm rolling with it until I'm told that it's a bad thing and I'm a naughty boy.

With this, I can consume the results I have and and simply call "next" to get the next lucene page without worrying about the accumulators in the loop function, this means I'm now left with quite a stripped down loop:


```clojure
(defn perform-query 
  [producer offset amount]
  (loop [results ()
         page (lucene-page producer amount)]
      (let [new-results (take amount (concat results (:results page)))
            new-total (count new-results)]

           (if (and (= (count (:results pager)) 0)
                    (not= new-total amount))
             (recur new-results 
                    ((:next page)))
             new-results))))
```

### To the recursive lazy sequence

Now I've pulled out the important bits of this code into two different stages (pulling data from lucene, paging data over that), it's quite trivial to convert the loop into a lazy sequence

```clojure
(defn lucene-seq 
  ([page] (lucene-seq page (:results page)))
  ([page src]
   (cond
     (empty? (:results page)) ()
     (empty? src) (lucene-seq ((:next page)))
     :else (cons (first src) (lazy-seq (lucene-seq page (rest src)))))))
```

I'm pretty pleased with this, tidying up the rest of the code around the rest of the file reduces my line count by 50% and leaves the complete solution looking like this:

```clojure
(defn lucene-producer [tx reader opts]
  (fn [offset amount]
    (->> 
      (lucene/query reader 
                    (:query opts) 
                    (+ offset amount) 
                    (:sort-by opts) 
                    (:sort-order opts)) 
      (drop offset) 
      (valid-documents tx))))

(defn lucene-page 
  ([producer page-size] (lucene-page producer 0 page-size))
  ([producer current-offset page-size]
   {
    :results (producer current-offset page-size)
    :next (fn [] (lucene-page producer (+ current-offset page-size) page-size))
   }))

(defn lucene-seq 
  ([page] (lucene-seq page (:results page)))
  ([page src]
   (cond
     (empty? (:results page)) ()
     (empty? src) (lucene-seq ((:next page)))
     :else (cons (first src) (lazy-seq (lucene-seq page (rest src)))))))
```

No doubt a seasoned Clojure developer would further reduce this (and there are probably a couple of snafus), but I'm pretty pleased that the interface I'm now exposing to the rest of my code is just a plain old sequence and even if I go and play golf with the underlying code the behaviour of that sequence shouldn't change. 






