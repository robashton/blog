As mentioned at the start of my little bit of Cascalog wandering, I've asked [Mastodon C](https://www.mastodonc.com/) to let me come and work with them for a couple of weeks in the assumption that they'll have some tasks I can get on and do and pick up information as I go along.

In preparation for this, I went through the Cascalog for the Impatient walkthrough (or at least the first 75%) while I was sat in a pub, and now I'm at the office and finding out what I am going to be getting on with!

- [For the impatient - Part 1](/entries/impatiently-learning-cascalog---part-1.html)
- [For the impatient - Part 2](/entries/impatiently-learning-cascalog---part-2.html)
- [For the impatient - Part 3](/entries/impatiently-learning-cascalog---part-3.html)
- [For the impatient - Part 4](/entries/impatiently-learning-cascalog---part-4.html)

I've been asked to look at the [CDEC Open Health Data Platform](http://openhealthdata.cdehub.org/) which takes data from the [HSCIC](http://www.hscic.gov.uk/) to work out a few things such as the cost to the health service for Diabetes, or breakdowns across regions for the costs/prevalence of diabetes.

It's a nice example of what can be done with [Cascalog](https://github.com/nathanmarz/cascalog), although diving into it can be daunting as you're immediately met with walls of Clojure that look like this

```clojure
(defn diabetes-spend-per-head-per-ccg-per-month [gp-spend gp-prevalence]
  (<- [?ccg ?year ?month ?ccg-registered-patients ?ccg-diabetes-patients ?ccg-total-net-ingredient-cost ?spend-per-head]
      (gp-spend :> ?ccg ?practice ?year ?month ?gp-total-net-ingredient-cost)
      (gp-prevalence :> ?practice ?gp-name ?gp-registered-patients ?gp-diabetes-patients ?gp-prevalence)
      (ops/sum ?gp-registered-patients :> ?ccg-registered-patients)
      (ops/sum ?gp-diabetes-patients :> ?ccg-diabetes-patients)
      (ops/sum ?gp-total-net-ingredient-cost :> ?ccg-total-net-ingredient-cost)
      (has-patients? ?ccg-diabetes-patients)
      (spend-per-head ?ccg-total-net-ingredient-cost ?ccg-diabetes-patients :> ?spend-per-head)))
```

This actually makes sense if you're familiar with logic-based programming or have followed the impatient tutorial so I'm relieved to be able to read through most of this code. If you've not been doing this, or it's 8am and you don't do this for a living then seeing a codebase of this causes insteant headaches and nausea. An application of coffee later and it all makes sense.

I am however, on running it met with a couple of problems:

- Where is the data supposed to come from?
- How do I run this stuff?

### Repl driven development

The way I've been writing Clojure is via tests and a tight feedback loop built up around those tests. I have however been told on numerous occasions that I should be using the repl more and this codebase is an example of where the author is clearly a big fan of the repl!

There is also little automation in place for getting the data or executing the code because if you're familiar with the repl and you have the data already, then throwing code at the repl is the fastest way to put this together and that is what has been done.

So what does this look like? Littered through the codebase are paragraphs of Clojure that look like this:

```clojure
;; month,total_spend
#_ (?- (hfs-delimited "./output/diabetes-total-spend-per-month-england" :delimiter "," :sinkmode :replace)
       (total-spend-per-month-england
        (diabetes-spend-per-ccg-per-month
         (diabetes-drugs
          (prescriptions/gp-prescriptions
           (hfs-delimited "./input/prescriptions/pdpi" :delimiter ","))
          (ods/current-practices (hfs-delimited "./input/ods/gppractice/epraccur.csv" :delimiter ","))))))
```

See that *#_* stuff? That's a comment which means this code isn't compiled at all normally, but it's there so a hardy Emacs user can just execute the code by sending it from the editor to the Repl.

**I'm a vim user, get me out of here**

So there's that.  Thankfully, a few people have written plug-ins to make Clojure development a bit more dreamy in Vim and a good starting point is the [vim-fireplace-easy](https://github.com/ctford/vim-fireplace-easy/) repository put together by [@ctford](https://twitter.com/ctford) which brings together a few plug-ins.

*vim-fireplace*

Vim-fireplace means that I get handy short-cuts for sending code to the repl for execution, I simply fire up a repl in one terminal, and vim in another terminal and then sticking my cursor over an expression and typing 'cpp' means that expression being evaluated in that repl.

```
  (+ 4 5)
```

So, sending this to the repl would mean I'd be given the answer '9', or putting the cursor over the big expression above would result in a query being sent through Cascalog.

*vim-clojure-static*

This just brings in lots of good behaviour when dealing with braces/highlighting/etc - a must for any developer witing clojure in vim.

*paredit*

This will automatically force my parens to be balanced, as well as vectors, sets and double quotes - it also brings in handy keyboard shortcuts such as (LEADER+W) for surrounding my current selection with parens, or (LEADER+w ") to surround it with double quotes.

### Doing it their way

So, over the next few days I'm going to do it *their* way, by building up a program bit by bit and sending it to the repl for evaluation. 

In the next entry, I'll look at my task and how I start going about it.
