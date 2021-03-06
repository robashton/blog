**Questions:**

- "But how do you maintain large JS codebases..."
- "How do I build Enterprise Scale JS applications"
- "How do I refactor across an entire project in JS"

**Answers:**

- You don't
- Lots of small things
- Build small things that do one thing well
- You shouldn't build large JS codebases

Etc

I almost started a drinking game at NodeDublin, "take a drink every time somebody mentions the Unix Philosophy or one of the above statements", I'm glad I did not - it was held in the Guinness Storehouse and I'd have not lasted beyond the first couple of sessions had I gone through with it.

These are sentiments uttered at conferences, in meeting rooms, in blog comments, in blog entries and pretty much whenever the subject of large code-bases in JS come up. Normally uttered with a self-satisfied smirk and with little to no further guidance on how exactly one might go about it.

Sure, you can always fall back to Typescript or Coffeesript or KoolaidScript, sure you can leverage awesome IDEs like Webstorm, Visual Studio, etc, but we've been building Enterprise Systems in C# and Java for years in this way, and the wheels of progress eventually stop turning because iteration becomes expensive, maintenance becomes 90% of the job and every meeting boils down to a question of "legacy", "compromise" and  "we are where we are".

So, this is what I am doing, this is my current recipe, a collection of the rules of thumb I am using for building an open source game engine (PrimoJS) in JavaScript in a way that doesn't make my brain run out of my ears whenever I want to write a new feature. (Caveat: I haven't "released" this engine, I am merely pulling it together out of code already written to help me do Ludum Dares faster)

This is client side JavaScript only in this project, all of this is equally applicable to JS in ASP.NET MVC projects, in J2EE-whatevs projects and probably your Ruby on Rails projects too.

TLDR: Browserify + npm + github = success

NodeJS
---------------

I don't care if you're not writing NodeJS for whatever reason, the command line tooling being built up in this area to help with JS is fantastic, and trying to use the copy-cat tooling being built up in your favourite Enterprise environment is just setting yourself up for frustration and trouble in the future. It's just an exe, get it and integrate it with your toolchain, make your life better today.

Browserify
------------------

Okay, I'm a convert - I've been talking AMD for a while now, and the idea of having to run a build script had put me off Browserify for a while - it turns out the development experience is pretty good and by the time you get to the browser to hit refresh, this is generally done already anyway. AMD is now dead to me.

    npm install -g browserify

Once you have it, if you write your JS with node-flavoured CommonJS style require calls, you can package JS up for the client by running Browserify as part of your build script.


**app.js**
    
    var windowing = require('./windowing')
      , messaging = require('./messaging')


    messaging.on('foo', function(message) {
      windowing.showMessage(message)
    })


**Then build**

    browserify site/app.js > site/app-built.js

This might be alarming at first, because it seems like concatenating these files might make the debug experience a thing of nightmares, but that's what the --debug flag is for:

    browserify site/app.js -o site/app-built.js --debug

This inserts special directives into the JS so the browser knows what the original files were and they can therefore be opened and debugged in their original state.

I have a Makefile I copied from VoxelJS - it looks like this

    all: concat
   
    concat:
      browserify site/app.js > site/app-built.js

    watch:
      browserify site/app.js -o site/app-built.js --watch --debug

During development of my application, I type

    make watch

and I don't have to think about this ever again.

Now, the cool part. With RequireJS I was always having to set up elaborate configuration scripts, "You can find underscore over here, you can find jQuery over here, you can find this other legacy JS over here, please do these things with them", this was, in short, a ball-ache.

Browserify eschues this way of thinking, and simply says "if it's a relative path, I'll use it, if it's a global path I'll look in node_modules"

NPM
----------

Here comes and important learning: *it is not enough* to simply use a require system, split your JS across multiple files in a folder and say "my files are small and therefore I am building small things", having all your code in a single project is not sufficient encouragement to think about the boundaries between those files and before long you will find yourself with 10,000 lines of spaghetti JS across 100 files in a folder with relative include paths and feel the pain it causes when trying to change anything at all inside of that.

**NPM has the answer**

By saying "I am going to build independent modules, and require them from my application as needed", you are making a statement of intent that those modules will have some fairly important qualities.

- A module should be re-writeable in a day without affecting the rest of the system
- A module should export a single function that has clearly described responsibilities
- A module is independently versioned & published to the registry as its own thing
- A module should therefore not be coupled to any of the modules that consume it, relationships should be uni-directional
- A module has its own git repository and history

For example, the biggest module in the PrimoJS eco-system is the core engine, at 700LOC, and that is *too big*, I am still harvesting that original codebase into smaller modules - this takes time because it is hard to go from big to small, it is hard to go from coupled to uncoupled and it is harder still when you are trying to be always making changes that roll forward, rather than backwards in terms of progress.

Nevertheless, PrimoJS is currently split across 16 modules, all of which fit the above list of requirements, they could all be used independently of each other, or more likely brought into a project that has pulled in the core engine and wants the additional functionality that these modules bring.

What does this look like in reality? This is the package.json of one of my 'example' projects, the actual application using PrimoJS

    {
      "name": "centipede2013",
      "version": "1.0.0",
      "dependencies": {
        "send": "~0.1.0",
        "express": "~3.0.3",
        "underscore": "~1.4.3"
      },
      "devDependencies": {
        "primo": "",
        "primo-animation": "",
        "primo-text": "",
        "primo-boundary": "",
        "primo-rigidbody": "",
        "primo-events": "",
        "primo-audio": ""
      },
      "engines": {
        "node": "0.8.x",
        "npm": "1.1.x"
      }
    }

The core engine is brought in, and then other components are brought in for use with that project, it is important to note that none of these components have a dependency on that core primo module, or vice versa - they are independent and could be used independently. 
What does this look like? Well, I can use NPM list to show me the dependency graph, which goes something like this:

    ├─┬ primo@0.1.1
    │ ├── primo-camera@0.0.0
    │ ├── primo-canvas@0.0.0
    │ ├── primo-counter@0.0.0
    │ ├── primo-events@1.0.2
    │ ├── primo-spritemap@0.0.0
    │ └── primo-timer@0.0.0
    ├─┬ primo-animation@0.0.1
    │ └─┬ primo-spritemap@0.0.1
    │   ├── primo-canvas@0.0.0
    │   └── primo-events@1.0.2
    ├── primo-boundary@0.0.0
    ├── primo-audio@0.0.6
    ├── primo-events@1.0.2
    ├─┬ primo-rigidbody@0.0.2
    │ └── primo-utils@0.0.0
    ├── primo-text@0.0.0
    
We'll notice that the spritemap is used several times, as is primo-events and primo-canvas. If I was using relative paths, this would be pretty messy and hard to trace, but with a package manager the relationship is clear and it is obvious that those modules are independent.

Fine, so what now? I have a folder on my hard drive that has a pile of other folders in them, each has its own package.json, its own git repository and can be developed independently - but this is hardly ideal from a development point of view is it?

An obvious question to be asked now, is "If you have a bug in primo-animation that impacts your centipede application, how do you fix, test, verify and iterate on that bug without copying JS around and making a mess of things. (How often have you downloaded the source of NHIbernate in .NET and had to debug an issue with it that happens in your code base? It's not fun is it)

That's where NPM really shines, we have NPM link - one of the finest commands in the history of package management.


    cd components/animation
      /usr/local/lib/node_modules/primo-animation -> /home/robashton/src/primo/components/animation
    cd ../../examples-centipede
    npm link primo-animation
    /home/robashton/src/primo/examples-centipede/node_modules/primo-animation -> /usr/local/lib/node_modules/primo-animation -> /home/robashton/src/primo/components/animation

NPM can set up symlinks, so editing the files in primo animation *in place*, will result in the changes being visible in the application as you make these changes.

This tells you a few things

- This is an explicit step, you have to conciously choose to edit across module boundaries
- You shouldn't be doing this often on a stable system
- Ideally, you'd have a test harness so you can iterate and develop fixes/etc independently of the usage of the module itself

Now, as the ever-pragmatic developer, I know that Primo right now is open to a lot of churn and I'm happy to be editing across these boundaries occasionally (especially with new modules), but as modules become mature I shouldn't be touching them any more - and later on I might choose to swap one out for another that does it better anyway.

What's even better about the use of NPM is that I can pretty much forgoe that whole thing where I have a folder called 'libs' with all sorts of crazy JS in it. That's a pretty sweet deal and something I can whole-heartedly get behind on.

The Caveats
--------------

**NPM**

It is worth pointing out that for client-side JS package management, the whole ecosystem is very much in the air -  we have Bower, Volo, Yeoman, Jam, etc. Each come with their own idea of a manifest, linking, dev etc.

For now, I don't see the point - NPM works really well and has already solved most of the problems inherent to a pure world of JS (We're not talking other kinds of deps such as HTML/CSS/etc - and for that Bower might still hold the answer)

The problem with NPM is that you're never sure which modules are client-side or server-side, not everything works everywhere - for example, the jQuery package in NPM is the server-side jQuery which makes life pretty confusing for now.

**Folder hell**

I have lots of folders, each with their own git repository, their own package.json, their own published state etc. This shouldn't bother me, because they're indepdendent and work done on these things should be independent - but that's not really the case when adding new features to a game that requires new code in primo-pritemap that requires new code in primo-canvas.

This shouldn't happen very often (and it doesn't, because having things that do one thing and needing it to do two things means you probably want two things, that second thing being a new thing), but when it does, and you're offline, you end up with folders that are out of date with regards to the published state of the package and the only way I can see to manage this is to go through all these folder and check manually whether I need to push and/or publish. This could be solved by tooling - any takers?

**Package hell**

I have lots of packages, each with their own git repo - but the primary use case is for usage within the PrimoJS eco-system - when you pull down primo for use in a new game you only get the core, and a lot of the time you might want animations, audio, physics etc.

    > npm install primo --save

There is nothing here to tell the developer that they might also want to do

    > npm install primo-animations primo-audio primo-rigidbody --save

I think this can be solved with documentation and tooling, maybe a command line script ala RoR for generating a new game's folder structure
 
    > primo-create mynewgame
    do you want all the default components available? (y/n)
    > y
    do you want a level editor (y/n)
    > n
    do you want a browserify build script? (y/n)
    > y

**Module discovery + documentation**

Again, lots of small modules - where does the documentation go? Where does the user go to find answers to his questions when he has them? where does the user go to work out which module is for what?

I think this varies across projects, are your modules all part of the same eco-system? Are they truly standalone and usable wherever? I think for each 'tree' of modules, a single *source* of information is useful - even if the sub-categories are just links to the documentation for the specific modules. Making the user go searching for the right module and the right documentation is just asking for trouble down the line. (So, in my case, it would be primojs.com and links to the available modules.

Summary
--------------------

This is just one way of tackling this, but it is real world and it does work quite nicely (apart from the solveable caveats above). It will be interesting to see which direction client side dependency management will go over time, but CommonJS is not going anywhere anytime soon and is a safe bet to be building your project on (regardless of whether it is Browserify or Stitch/etc doing the packaging for you)

The most important part of all of this is creating the the modules to be standalone so you don't *have* to traverse across them when building new features or fixing bugs. Replacing folders with modules is for nought if you don't actually make modules.

Links
------

- Browserify [http://github.com/substack/node-browserify](http://github.com/substack/node-browserify)
- NodeJS [http://nodejs.org](http://nodejs.org)
- NPM [http://npmjs.org](http://npmjs.org)
- PrimoJS (not "released" yet) [http://github.com/robashton/primojs](http://github.com/robashton/primojs)
- VoxelJS [http://github.com/magogden/voxel](http://github.com/maxogden/voxel)



