I ended up (because of jetlag) waking up at 4am and deciding to book myself in for that day's [Kotlin](http://kotlin.jetbrains.org/) workshop being ran by everybody's favourite sarcastic [Jetbrains](http://jetbrains.org/) employee [@hhariri](http://twitter.com/hhariri). (Much to his surprise and irritation no doubt :))

I made a few remarks as the day went by about things I didn't like, and as one might think I was being a bit negative, I decided to take notes and write up some of my thoughts on language decisions I'd found interesting for one reason or another.

**So what IS Kotlin**

Well this is the easiest bit really, it's another language that runs on the JVM that isn't Java. Any language that isn't Java is fine by me, and having already got the JVM installed for my (temporarily) abandoned Clojure efforts last year setting up wasn't too hard.


**What is its purpose?**

This is the first thing everybody (including myself) asks, and my understanding is that 

- Java is crap
- Clojure is too hard for the common developer to pick up and be effective in
- Ditto Scala
- Some of the ideas in the above languages are still appealing
- the JVM has a good eco-system and is appealing to work with
- Having a language that is easy to use but has some nice features and is on the JVM is also appealing

That's a simplification, it might just be that Jetbrains has a lot of spare R&D cash and is throwing stuff at the wall to see what sticks ;-)

**What did I use to play with it?**

I did what I normally do when pulling down something new, I opened vim and tried to write hello world in it.

Sadly, (and one might say as you'd expect from a tooling oriented company), the docs/support for the compiler is sub-optimal and I wasn't able to run any of the jars I created using their command line tools.

*One of the many attempts I had at doing command line kotlin*

  <img src="/img/command_line_kotlin.png" title="Command line failure">


The best advice for "getting started on the command line" was to "install ANT" and "write some XML" and I figured if I was going to go down that route then I should just get with the program and use IntelliJ. Hopefully this will change in the future, I hate being told what to use to do my work and I hate tooling-oriented eco-systems.

I installed IntelliJ which thankfully was a simple matter of downloading a tar.gz and executing a shell script (it's all written in Java). I got some warnings because I'm using OpenJDK out of principle but that doesn't seem to have caused me any issues.

Installing the Kotlin plug-in was easy (there is a nice plug-in manager), and I managed to install a vim plug-in too (although it's a bit dodgy at times, especially when trying to leave any of the editing modes)

**What did I find out when writing hello world**

Well, I had to get Hadi to fix my IntelliJ set-up because things weren't working right from the beginning (installing the Kotlin plug-in wasn't enough) and it seems some of the confusing cruft in the JVM is still present when it comes to working out what the program entry point is, and all that class/jar/nonsense that I never really understood.

*Getting started*

  <img src="/img/idea_start.png" title="confusing">

This is the stuff I'm talking about, I ask to create a "Java Module" (and I'm not really sure what this is) and then after setting up some Kotlin stuff I get a big folder full of all sorts of xml and jar files in it and at some point I get to create the single kotlin file I really wanted. This is one of the reasons I never really liked tooling like Visual Studio/etc - there is a big surface area of potential problems here and I don't know what is really going on. 


One of the great things like Clojure and its eco-system has done, is hide a lot of this crap away so I *can* just write some code in a file and compile/execute it. If we're going to make Java less sucky, this is a must.

Hello world is quite simple, once you get past the mess of files above:

    fun main(args : Array<String>) {
      println("Hello world")
    }


The syntax is quite explanatory, and the main(args/etc) stuff is just a convention that the tooling knows means this is an entry point. It takes about 15 seconds to compile on my MBA and in this lies another annoyance.

15 seconds to run hello world means 15 seconds to run my tests, means 15 seconds for a feedback loop *before even writing any code*.

Hosting the compiler so we haven't got JVM start-up time in this mess, and cutting this time down would make me a happier developer.

**My feedback to Jetbrains over this getting started bit**

- Make it easier for me to just write a standalone file and execute it as a program please
- Hide the legacy Java crap that I don't know anything about
- Give me a Kotlin Command Line app option in IntelliJ which doesn't give me all the crap in the project
- Keep the compiler in memory or do whatever else is needed so this feedback loop goes from a minimum of 15 seconds to a minimum of < 0.1 seconds

**Next up...**

I'll go through some of the language features I liked, and some of the language features I didn't like and give some (hopefully) constructive feedback over them.

