## In the old days

In the old days we had some really awful JS being written on pages in a pretty ad-hoc fashion and it caused us huge problems. Presumably most people still do this but they're the same people who aren't reading this blog post so we can pretend they don't exist unless we end up consulting at their companies and oh god oh god please no not that.

The great/awesome/amazing thing about JS is that nobody wanted to go near it and in enterprise organisations they just wanted to stay in their safe little world of well organised layers of abstraction ordered by factory and XML super injection frameworks.

That was great for people like me who wanted to get paid enterprise rates but didn't want to have to put up with layers of awful "best practises" and performance problems that came from the irrational fear of letting anybody who wasn't a DBA touch the database.

Even better, when those performance problems arose, we could save the day by writing a front-end in JS that pretended they didn't exist and gave the users a great experience despite the shoddy workmanship on the backend.

## We've hit peak JS

It was even better when jQuery came along as the world we inhabited could be bolted together out of small re-usable jQuery plug-ins. We finally hit peak JS when NPM turned up and we started using a half-decent module system for managing these self contained widgets.

I envisaged a future where I could work with great teams on great UIs with great codebases organised out of these little modules and organised around self-contained features/widgets. Perhaps we could slowly take back control of codebases from those enterprise-bound fiends with their beans, their orms and their patterns and practise based proxy factory factories.

We had some reasonable things going on, we were building great things out of great code and we had freed ourselves from the shackles of the burdensome enterprise frameworks.

## You ruined it

Now - I still do JS, but I'm mostly working in Erlang these days, building video streaming/encoding systems and *blah blah blah*, that's why I'm not really blogging at the moment but basically my backend is pretty sexy and my frontend is pretty sexy too (Facebook's React + NPM to fill the gaps). I came across this post on Stackoverflow that brought back all the flashbacks of those enterprise days and to my horror it has spread like a java-borne venereal disease to the front-end of the body of my internets.

### [Angular.js: service vs provider vs factory?](http://stackoverflow.com/questions/15666048/angular-js-service-vs-provider-vs-factory/)

Okay, that's not so bad, but let's go and look at the [top-voted answer](http://stackoverflow.com/questions/15666048/angular-js-service-vs-provider-vs-factory/20613879?stw=2#20613879) (okay, it's not anymore since I wrote this post) and take stock of this because apparently there are some very satisfied customers:

  <blockquote>Wow! Thanks for the detailed explanation. You made it easy and crystal clear mate. Well done!!</blockquote>

If I was being kind, I'd say that this comment is ironic and the whole thing is just a really good example of Poe's law, but on reading the whole thing I don't think this is the case and my frown has not turned upside down.

So... the first thing we see is a quote from the freaking Angular docs which looks like this

  <blockquote>An Angular "service" is a singleton object created by a "service factory".  These service factories are functions which, in turn, are created by a "service provider". "The service providers are constructor functions".  When instantiated they must contain a property called $get, which holds the service factory function.</blockquote>

What the actual fuck *is* this? I read this as *"in order to do a hello world, you must first create a hello world service to create the hello world factory to create the hello world sercice so you can print hello world on the screen."*

  <blockquote>Whaaaaaaat? Am I reading a thesis? It is very confusing.</blockquote>

No, you are not reading a thesis, you *are* reading the angular docs apparently.

If it were a thesis it would probably be trying to explain a solution to some sort of problem instead of describing a made up solution to a made up problem. (Actually, that's not strictly true because academics are in a world of their own too but close enough).

  <blockquote>The following is a real world example made up for this question.</blockquote>

Presumably most of the scenarios used for creating Angular are made up because it's the only reason we would ever need all these factories, proxies, services in the front-end. The kind of code and explanation we're about to go through comes straight from la la land and it's hard to believe that it's not a joke.

  <blockquote>Service, Factory, and Provider can be the same.</blockquote>

What? No of course they can be, they're all just functions that return a value but okay, let's carry on with this madness to see where it leads...

We get an example of "car instantiation", with the premise

  <blockquote>With service(a singleton), you cannot achieve this because service cannot be instantiated.</blockquote>

To justify the existance of providers, because

  <blockquote>To instantiate, you need Factory or Provider</blockquote>

No. Oh God. WHAT. *WHAT THE FUCK*.

    var car = new Car({ cylinders: 4 })

The freaking 'new' keyword.  We had these arguments in the enteprise back-end world so many times and to see the same bullshit repeated for JS galls me right to the fucking core. This is the same old shit in what used to be my go-to escape hatch from that hideous crap.

  <blockquote>Provider can be configured for your application</blockquote>

Of course we we can configure providers if we need to configure our applications. How else could we configure our applications and make them configurable for our enterprise scale configurable applications.

I love the code that comes next, it's almost a parody in itself. It doesn't even need a commentary to be fucking hilarious.

    app.service('CarService', function () {
        this.dealer = "Bad";
        this.numCylinder = 4;
    });

    app.factory('CarFactory', function () {
        return function (numCylinder) {
            this.dealer = "Bad";
            this.numCylinder = numCylinder
        };
    });

    app.provider('CarProvider', function () {
        this.dealerName = 'Bad';
        this.$get = function () {
            return function (numCylinder) {
                this.numCylinder = numCylinder;
                this.dealer = this.dealerName;
            }
        };
        this.setDealerName = function (str) {
            this.dealerName = str;
        }
    });

To configure the dealer, all we have to do is

    app.config(function (CarProviderProvider) {
        CarProviderProvider.setDealerName('Good');
    });

Hey, it's just config - no need to change any of the real code!!

I'd write a plain old JS equivalent but trying to wrap my head around all of the indirection in the above example is making me want to crawl under a desk and bang my head on the floor until the brainmeats come out so I don't have to subject myself to this madness any further.

  <blockquote>But, why CarProviderProviderinstead of CarProvider</blockquote>

Here's a tip. If you find yourself asking a question like this. If you find yourself asking a question which requires this sort of answer and then this sort of question to be asked *YOU'VE DONE IT WRONG*.

There is no inherent shame in doing it wrong, it's okay - we all make mistakes, but given the current trajectory of this bullshit we're far from realising what is being done and we'll be hiring Angular Consultants and sending our students on expensive Angular Training Courses for many years to come. Well done - you fell for it.

## What the fuck is wrong with you people?

We had a good thing, you ruined it. We had an escape route from that ridiculous enterprise hand-holding bullshit and instead of learning how to fucking code you've just brought your factory provider providers with you into what was once an okay place to get stuff done.

### Screw you guys, I'm going home

It's okay, I don't really do enterprise any more. I showed this question and answer to my colleagues and we all had a good laugh at your expense because this stupidity is fucking hilarious. But you know what? When you've all stopped digging this hole and you've realised what a bottomless pit really looks like, I'll be stood outside laughing at you because it's still your own fucking fault.

Start thinking for yourselves, ditch this shit before it's too late and learn how to write some actual fucking code. The information is out there, you can do it. If you need handholding with factories and providers and service provider factories then consider that the world doesn't need your bullshit code and go and get a job you're actually good at. Stop ruining it for the rest of us.


