Finding myself embroiled in yet another debate about IOC containers on Twitter I've decided to place my current thoughts here for posterity.

*I don't really like using IOC containers*

There, I said it. I really don't. I used to, I did - I thought they were an excellent way to manage those dependencies, to push the effort of lifetime and scope management into something that would automatically handle those things for me so I wouldn't have to think about them.

I thought they were an excellent way to bootstrap of my entire application from a single place, and have all the interfaces matched with their single implementations and pushed into the relevant consumers without having to think about it.

But you know what? Having your entire application sucked out of a black box and then writing rules for the exceptions to those wonderful conventions and then writing new conventions and interceptors and using all the "wonderful features" of the modern IOC container started to lead to developers spending more time debugging mysterious container issues and fighting odd/conflicting lifetime issues than writing code of real value.

Oh, you could easily dismiss this with "Oh, Rob doesn't know how to use a container properly", but you'd be missing the point, because even if I didn't (*and I do by the way*), it's irrelevant whether I do or not.

*Missing the point*? Missing the point because nearly every team using an IOC container **IS** doing it wrong, and they're doing it wrong because they *are* complicated and they give you a lot of "extensibility points" to make it *easy* to do things like interception, they make it *easy* to do things like per-request items, they make it *easy* to create singletons that aren't really singletons. and they make it *easy* to create lots of interfaces that get sucked into lots of classes (in the name of low coupling, and usually with the result of a total lack of any cohesiveness).

**It's putting the cart before the horse**

Learn to walk before you run, cart before horse, etc. The fundamental issue here, is that people are spending their times learning about IOC containers, gaining some level of test-ability because everything is an interface that talks to other interfaces via interfaces to interfaces. This is not to say that IOC containers cause this explicitly, because if you've already got a grasp of OO concepts then you aren't going to do too much damage here (except for hiding simple concepts like lifetime management up behind infrastructure that's a bit more future-career-proof).

My fundamental issue is that not enough time is being spent by developers learning how to just grow a testable and maintainable code-base. Throwing your lot in with a container with a centralized bootstrap process and claiming that's an advantage is missing out on a fundamental aspect of clean software development - that is, neat little packages that know how to bootstrap themselves and expose a sensible API for doing so - allowing them to be used across the code-base in an understandable and idiomatically crafted way.

Allowing your junior developers to "not worry about these things", because the almighty and all-knowing container will look after them and ensure that the code is testable, and that dependencies will just work automatically is simply shirking the responsibility of actually teaching those developers the useful and transferable skills that will help them deliver products across a multitude of languages and platforms. (EG. not just the two that come with a million IOC containers to choose from).

> *In GOOS we are extremely explicit about scope!  Java is a block*
> *structured language with lexical scoping, closures and objects.*
> *Blocks and objects are scopes. Variables declared in a block and*
> *instance variables declared in an object are in a scope. There is no*
> *need to re-implement (badly) what the language (compiler and VM)*
> *already provide.*

> Nat Friedman

And this, is what I believe that it all boils down to.

- You want per-request scoping? That's a "using statement" around the entry point to that request. 
- You want application-lifetime scoping? Just create the object on start-up and let it get cleaned up on application-close
- You want something more fine-grained? That's just another using statement around the code concerned.

Objects still don't need to know about their own scoping, of course not, and we realized that with containers early on with the removal of attributes from most frameworks. But why make all the effort of pushing scoping into a framework when it is such an intrinsic part of your application and it's relatively trivial to manage anyway? Lifetime management is not an implementation detail to be pushed away into central infrastructure code, and nothing but trouble will be had from trying to work that way *(nested sub-containers anyone? No - I thought not).*

You want to talk about writing masses of boilerplate code? I have very little in the applications I'm actively developing now - each abstraction developed is responsible for its own set-up, and only exposes to the outside world any configuration needs that it might require and the public interfaces required to do its job. That code is written and tested *as part of the code-base*, is compile safe and is fast to bootstrap because "it's just code". Abstractions are built on top of other abstractions and are tested against other abstractions with appropriate levels of isolation depending on the test concerned and there are no problems here at all.

This approach does not preclude the injection of dependencies into say, the subsystem which might be created as a consequence of its construction, it merely hides that detail behind an appropriate API because the consumers of this package don't typically care about that construction.

You want to talk about managing deep or complex object graphs? That's not a problem - each package is only ever going to have a shallow object graph, because that's sensible software design - I don't have complicated object graphs because complicated object graphs tend to show themselves during testing and are very quickly turned into simple object graphs.

It's just software, and we should be spending more time learning how to deliver software and less time learning how to manipulate favourite container X.
