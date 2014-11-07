I'm sat on a train going over the [finger tree paper]() and a simple explanation of how monoids can be used to effectively implement them for pretty much any purpose and I'm having another face palm moment about pretty much every piece of software I've touched over the last decade.

Coupled with my current feelings over our society's race to the bottom in creating another generation of factory-floor developers and the inevitable craptitude of Ruby (or similar) that will follow I've now pretty much hit the high score on my moroso-meter for this week.

TLDR: *Sigh*

On software as a paper pushing exercise
===

Every for loop, every index variable, every well meaning interface behind yet another factory class and every line of code written to transform arbitrary data structure into another seem increasingly obsolete as I continue on my journey into learning about the powerful abstractions that ship with Haskell and other such similar technologies. The (current) conclusions reached by some very clever people over the last few decades about what actually constitutes a common pattern and what they have decided needs to be tucked away as boilerplate continue to astound me as I slowly work my way through the exercises designed to teach me about them.

I was sat in a talk at JSConf a few weeks ago where it was discussed how many libraries and frameworks shipped with their own "forEach" method in JavaScript. To some people this was simply funny and to others it was just a simple fact of life accepted as part of the territory when working in a language that is as rushed and badly thought out as most of the software being written in it.

Listening to this induced a dizzying feeling as I realised that this was a problem that most Haskellers would never encounter (along with so many other repetitive tasks). Suddenly this mildly entertaining fact became a reeling horror with the dawning realisation that this was a tiny symptom of our willingness to spend our time writing the same code repeatedly in order to do the most minute of tasks for the creation of yet more software that probably didn't need to exist in the first place.

Every character of this code that ends up being written is another bug waiting to happen and another step in the typical enterprise product's life towards that point where the number of developers operating in "maintenance mode" start to outnumber the developers working on new features and the race to hire a greater number of junior-skilled developers starts. (And the costs spiral and the prices go up and the self-defeating spiral into mediocrity and expensive consultants gets tighter).

This has led to the misunderstanding that software is somehow an industry that we should all get involved in because of these buckets of money flying around between large enterprise shops and contractors for what is essentally meaningless busy-work as we rely on greater numbers of developers to manage the growing ball of mud at the heart of a growing number of real industry (and a lot of non-real industry too, the last generation of physical paper pushers).

These developers are then hired and given mind-numbing task after mind-numbing task in either TFS or Jira and rated on the number of these that they get through a week, the joy of software is gone because we have turned software in a paper-pushing exercise with no real end in sight (Unless you're one of the lucky one percent working on the latest failure that the next wave of ninety nine percenters will have to maintain for the next decade). It's okay though because high employment figures are important in the elections and bad software development is something that *creates* jobs because you always need more people to maintain software than to create it in the first place.

There could also be a whole section here about the hand-waving about the potential wins to be had from VC-funded start-up life and that as a motivation for getting people into software from school (truth: It's a lottery and you'll probably end up in the factory floor described above). It is shocking that instead of getting students to think creatively and embark on useful careers of learning and education that we're preparing them for a lifetime of servitude on the factory floor by lying to them about what goes on in the software industry.

Lowering the barrier to entry
===

We have whole technologies and conferences/training-academies over those technologies trying to lower the barrier to entry to software development under the misguided notion that by making the barrier to entry lower we'll somehow be able to keep up with the demand for software developers caused by having software developers by... creating yet more software developers.

The idea that by creating a whole generation of developers reared on languages like JavaScript and Ruby and their associated ecosystems we will somehow be in a better place is laughable at its best and terrifying at its worst. The parable about houses built on sand come to mind and yet we're unwilling to talk about this because it's fun to make robots dance to music at conferences whilst drinking beer, arguing about semicolons and boasting about the latest rounds of funding for our meaningless start-ups.

Manufacturing an entire ecosystem of trainers and educators around bad technology because those technologies are easier to teach is yet another step in this same race to the bottom and while it gives a lot of people warm fuzzies and further helps further employment statistics towards a more positive outlook it doesn't actually achieve anything other than help more people help create more bad sofware that we'll need more people to work on in the future.

Our attempts at professionalsm
===

So we've realised for a while that the cowboy attitude exuded by most young developers doesn't lead to well written software products (or indeed the middle-aged devs still cranking out classic ASP with no concern for the cost involved in maintaining their buggily ill-thought out systems) and we've started building *more* abstractions and concepts on top of the technologies that large companies have provided us with in the aim of making software development easier.

These seemingly helpful attempts at professional come in the form of acronyms that facilitate the marketing around the consulting for those lucky enough to claim some form of ownership around those terms; and some (for example TDD) seemingly stimmy the tide of code that is borderline impossible to maintain and makes our codebases simply annoying to maintain. By a lot of developers this could be seen as a win but when you realise the amount of secondary engineering that is going on to maintain these practises in these badly designed languages (IOC containers, most web frameworks, the generation of generic-laden interfaces, factories and factory factories) it all gets a bit much for my poor little head to deal with.

We call it professionalism because it's easy to make something that was easy to learn in the first place harder to develop and point at the quality improvements we make as a result of that; It might be possible to get a rube-goldberg machine to do something useful and reliably but sometimes it's easier to just butter your toast with a knife.

This self-perpetuating cycle continues because we have so many people jumping into programming via easy-to-learn languages and frameworks without ever stopping back to look at the basics. It's easy to sell snake oil and write blog entries and get consultancy and it's easy to feel good about doing something to make things better


It makes sense then to minimise that code churn and pull back into abstractions, and yet when talking about enterprise software and abstraction we commonly discuss the Gang of Four (The number of times I've been told by some well meaning commenter to "Go and learn basic software design patterns" in the last year is quite phenomenal). All of these design patterns are mere crutches or bandages for the problems caused by our attempts to make sofware development easier by allowing developers to bypass rungs on the educational ladder and start being "productive" sooner.

This

What should we actually do about it?
===





======= What are the points I want to make? ======

Current situation: We have an excess of software development jobs
Problem:

