I'm currently at [Mastodon C](https://www.mastodonc.com/) and hacking on their [Open Health Data Platform](https://github.com/CDECatapult/cdec.openhealthdata)

*Impatient Cascalog*

- [For the impatient - Part 1](/entries/impatiently-learning-cascalog---part-1.html)
- [For the impatient - Part 2](/entries/impatiently-learning-cascalog---part-2.html)
- [For the impatient - Part 3](/entries/impatiently-learning-cascalog---part-3.html)
- [For the impatient - Part 4](/entries/impatiently-learning-cascalog---part-4.html)

*The Open Health Care Data Series*

- [Repl driven development](/entries/an-introduction-to-the-ohd-platform-via-repl-driven-development.html)
- Getting running with the OHD platform

*So I've pulled down the OHD code - where do I begin?*

My initial confusion over "how do I run this stuff" was answered by the presence of the commented out blocks of code [designed for execution](/entries/an-introduction-to-the-ohd-platform-via-repl-driven-development.html) in the repl.

Great, but what do I run?

I settled for an innocuous looking expression

```clojure
#_(?- (hfs-delimited "./output/diabetes-per-head-per-gp-per-month" :delimiter "," :sinkmode :replace)
      (diabetes-spend-per-head-per-gp-per-month
       (diabetes-spend-per-gp-per-month
        (diabetes-drugs
         (prescriptions/gp-prescriptions
          (hfs-delimited "./input/prescriptions/pdpi" :delimiter ","))
         (ods/current-practices (hfs-delimited "./input/ods/gppractice/epraccur.csv" :delimiter ","))))
       (prevalence/diabetes-prevalence-gp
        (hfs-textline "./input/diabetes-prevalence/")))
      (:trap (stdout)))
```

This has a few input files that I don't have, so I decided to grep the files for anything beginning with 'http' or 'https' as the comments list the data required; then download the files specified manually. I also make a note to see about automating this process if it becomes a burden during my time on this task.

I can see why this hasn't been done as it's a faff, and once you have the data you're just iterating in the repl so you don't gain much personally by automation.  Moving on...

The files I found were as follows

    http://www.nice.org.uk/usingguidance/commissioningguides/adhd/adhdassumptionsusedinestimatingapopulationbenchmark.jsp
    http://systems.hscic.gov.uk/data/ods/supportinginfo/filedescriptions#_Toc350757591
    http://www.england.nhs.uk/resources/ccg-directory/
    http://indicators.ic.nhs.uk/webview/index.jsp?v=2&submode=ddi&study=http%3A%2F%2Fhg-l-app-472.ic.green.net%3A80%2Fobj%2FfStudy%2FP01121&mode=documentation&top=yes
    https://indicators.ic.nhs.uk/download/Demography/Data/QOF1011_Pracs_Prevalence_DiabetesMellitus.xls
    http://www.hscic.gov.uk/searchcatalogue?q=title:%22GP+Practice+Prescribing+Presentation-Level+Data%22&area=&size=100&sort=Relevance




