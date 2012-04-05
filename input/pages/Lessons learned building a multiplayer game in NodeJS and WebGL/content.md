I've uploaded Hoverbattles to its own server on EC2, and it has been running fine with an uptime of over 96 hours so far, and this is great!

[http://hoverbattles.com](http://hoverbattles.com)

I've wanted to share a few of the mistakes/lessons learned writing and deploying a multiplayer game built entirely with JavaScript on top of NodeJS and WebGL for a while and this represents an opportune moment to do so. 

I've gone with a brain-dump of various related learnings, as well as a couple of periphery items - first off, we'll go with the reason I couldn't keep Hoverbattles up on the old server.

**Deploy long-running, processor-intensive node apps to decent hardware**

If you're going to deploy a long running node application that is going to be running a constant load, don't deploy it to either:

- A micro instance of EC2
- A really low budget VPS

This might be plain old obvious to most people, but apparently not to me - I first deployed to my own VPS, and found that after it has been running for an hour or so it quickly consumed all the memory on the server and fell over.

I tried to find a memory leak in Hoverbattles itself, I changed to various versions of node and nothing seemed to work - I couldn't reproduce on my meaty laptop and I gave up for a while, as I was working on a new project.

Turns out, by running long running processes on a virtualised OS on top of oversubscribed hardware, the OS is lied to, or it lies to you and things don't work quite well. Some of the more knowledgeable types could probably tell us why - but the bottom line is don't do it.

Note: This probably doesn't go for simple websites built on top of node in express or whatever, I'm talking about long running applications that are doing processing almost constantly, like the server-side component to a 'realtime' multiplayer game.

**If you're really going to share code between client + server, plan ahead accordingly**

With Hoverbattles, it started off as a purely client-based game, with JavaScript files being included in the main HTML file and this worked great while I was experimenting with the WebGL and working out how everything was going to work.

I soon moved to having a server implementation running the logic, and ported everything across to CommonJS, using Stitch to package up all of the files so I could use them on the client.

This is actually problematic, as you don't want *all* the code on the server, and you don't want *all* the code on the server - with CommonJS you'll only get the code loaded on the server that is used there, but if you're stitching your entire /src folder, you're potentially also sending down code for your persistence, communication, 'secret sauce' stuff etc.

I ended up solving this in Hoverbattles by having a folder structure of:

     /shared
     /server
     /client

This is a bit hideous and arbitrary - and doesn't allow me to organise my codebase naturally along its logical borders, and it's for that reason in my latest projects I've switched across to using RequireJS.

By writing different entry points to the same code, and simply boot-strapping in various sub-systems and behaviours from those call-sites, you can naturally end up with a dependency chain that only includes code relevant to the platform for which it is targetted. 

**Avoid creating new objects in the main event loop**

This is obvious too if you've been developing any sort of large scale JavaScript application, or you come from an unmanaged background where this is something you learn not to do from very early on - but I've been developing in a mostly managed world for a few years (C#) and creating objects doesn't carry with it the same overhead so you become quite cavalier to it.

I started profiling Hoverbattles a few weeks in and was surprised to find out that 70% of my CPU time was spent in a single method, that is:

    vec3.create();

Vec3 is an object literal from glMatrix containing useful functions for manipulating and creating vectors on top of the typed arrays available in WebGL compatible browsers - these are cool for a number of reasons (performance oriented reasons mostly) and I didn't really think twice about my usage here.

Consider the following imaginary method called once a frame for each missile currently active in the scene.

    calculateDistanceToTarget: function() {	
      var difference = vec3.create();
      var targetDestination = this.target.position;
      vec3.subtract(targetDestination, this.position, difference);
      return vec3.length(difference);
    };

This is a really bad idea, and I had code lying around all over the place that would do this - create a temporary float array in order to perform some calculation and then carry on.

The answer was to create a buffer or two on start-up for each system that needed to do things like this, then the code looks something like this.

    calculateDistanceToTarget: function() {	
      var targetDestination = this.target.position;
      vec3.subtract(targetDestination, this.position, this.sharedVec3);
      return vec3.length(this.sharedVec3);
    };

This isn't too nice, but so long as you keep the use of this shared buffer to a single method (IE, write into it, then read out of it immediately) then there are no issues with multiple methods across the system using this data.

On that note though...

**Private state should remain private**

I got in the habit in Hoverbattles of being a bit cavalier about accessing state and simply doing direct property access across objects in order to perform calculations. In some cases this isn't a bad idea and keeps the code legible and fast. In most cases it's a lot of coupling added for little gain - especially if you start to write back to those fields later.

Just like in C#, property/field access is generally a bad idea, you should be asking objects questions which they can answer, and giving them extra information for those questions if they need it - and you should definitely be telling them to do things instead of taking that responsibility away from them.

The thing is, in most LOB apps this is really not that big a deal, CRUD is boring, the applications we build are boring, we can get away with this stuff. When you're dealing with a game world where dozens of things are going on 30 times a second, controlling access to state starts to become important. *Lesson learned.*

Consider instead our earlier example of:

    calculateDistanceToTarget: function() {	
      var targetDestination = this.target.position;
      vec3.subtract(targetDestination, this.position, this.sharedVec3);
      return vec3.length(this.sharedVec3);
    };

We could instead have:

    calculateDistanceToTarget: function() {
    	return this.target.distanceFrom(this.position);
    };

This subtle switch in logic means we're no longer accessing the supposedly private state of another object, and I'm free to change it without worrying about the rest of my code breaking.

I'm also able to far easier control this behaviour when writing tests (target can be a fake target if I feel it necessary to stub out the real logic).

In my current projects I am being a lot more strict about state access - all state is technically public due to the nature of my JavaScript objects, but I don't give into temptation and touch it (*that's will power yo*').

Encapsulation is really important in a project that has this much "business logic", and having a sensible object model is a big part of this.

**Push, don't pull - but sometimes pull**

I also made the mistake in Hoverbattles of trying to build components to control every aspect of a particular behaviour - this involved pulling state from various places to work out whether A or B should happen, or whether to display X on the screen or not.

This didn't scale, I ended up having to pull state from three sources (which means asking the world for the entities concerned), and writing methods to pull that state on the components that those entities were build out of.

Turns out I ended up with a system in places that looks like classic Event Sourcing; You look at high score tables, persistence, particle systems and the HUD as views on top of the single source of truth and consider that you can build those views from events being raised in the game world. Suddenly it makes sense that all state in areas with high view subscription should be built from events raised by those entities.

Once I had that realisation, development got a lot easier, *"Hey, I've been told to move, I'll  raise an event with the relevant data, subscribe to it myself to update my own state and let everybody else do the same".*

I didn't go overboard with this, in Hoverbattles there are only a few places where the above is true, and even then only just and in my latest code this is much more established pattern - the world has commands coming in via input or across the network, and raises events so everything that cares can be updated.

Sometimes it is just more appropriate to pull the state, especially if it's hard to raise an event without duplicating data (see above the cost of creating new objects), and the trick so far has been recognising that and trying not to overly homogenise.

**Don't let network-code take over your domain**

In Hoverbattles there was a real problem when it came to writing the network code, it ended up being far too pervasive and leaked into too many aspects of the logical entity code.

Some of this managed to be repaired before it became too much of a mess, the main realisation was that there were only a few classes of problem involved in most of the network code, that is:

- User input, sent as commands to both the world and the server
- Periodic sync, extracting state from objects and serializing across the wire to all clients
- Protected code that can only be executed on the server, but the results of which need executing on both client and server

These are actually ordered in terms of difficulty:

- User input can easily be sorted by sticking an intermediary between the actual input emitter and the game world (so, one object with some code in it). 
- Each component has the chance to serialize state and receive state by convention with methods called _in and _out
- This one caused a lot of issues, trying to attach different components to entities depending on whether they were created on the server/client, etc

That last one was a bit of a doosy, I ended up with about 20 objects trying to juggle only the responsibility specific to the server or to the client, it looked something like this.

    /client/
    /client/firingbehaviour.js
    /client/dyingbehaviour.js
    /client/lockingbehaviour.js
    /server/
    /server/firingbehaviour.js
    /server/dyingbehaviour.js
    /server/lockingbehaviour.js
    /shared/
    /shared/firingbehaviour.js
    /shared/dyingbehaviour.js
    /shared/lockingbehaviour.js

This gave me hard to debug errors because the responsibilities for an entity's behaviour were spread all over the place, state was being mutated all over the shop and it was becoming hard not to duplicate code across the different environments.

All of this because I was full of pride and didn't want to write into my code anywhere the line of code:

    if(Environment.IsServer)

As it felt wrong. I ended up with a compromise, which is I'd do that on the main entity object (which tends to bring together these different behavourial components).

Here's the thing - if I was using events to update my internal state, surely I could simply suppress the events on the client if the client didn't have permission to make that decision *(for example, player health loss is a decision only the server can make - I don't want craft blowing up and being removed from the scene if they didn't actually die, it's a horrible visual artifact).*

    applyDamage: function(amount) {
      var newHealth = this.health - amount;
      this.raiseServerEvent('CraftDamaged', newHealth);
    },
    onCraftDamaged: function(newHealth) {
      this.health = newHealth;
      if(this.health < 0)
       this.raiseEvent('CraftDestroyed');
    }

Regardless of client or server, this logic would get executed - but only on the server would the event actually get raised when a craft lost health, and that event would automatically be proxied to all the clients for the rest of the logic to be executed.

This leaves me with:

    /craft/
    /craft/firingbehaviour.js
    /craft/dyingbehaviour.js
    /craft/lockingbehaviour.js

Because this was back-patched in over a weaker system, there are some remnants of this change left over the Hoverbattles source, but it is a lesson I'm applying in the new game to really good effect. 

The network code in the latest project consists of about three objects primarily just routing commands and events and it is most likely going to stay that way.

**Arrays are mutable reference types**

Duh. we all know that, why bother including it? Well - remember I said that creating new objects is expensive so I was sharing them? Yeah - well that can bite too, and given that this was one of my recurring bugs (my own stupidity granted) it's worth documenting.

In C#, typically you don't expose mutable reference types, you'd return an IEnumerable<T> if you wanted to expose a collection from an object, which is a read-only collection fit for... well reading from the consuming code.

Can't quite pull off that trick in JavaScript (although the solution could exist in user-land it's a bit of a faff as it's not transparent). 

The problem is, in a game like Hoverbattles - half of our state is in fact arrays of either length of '3', or length of '16' (vectors and matrices) and we have to be careful when receiving a vector or matrix that we don't own. Consider the following simplified code, which is *reminiscent* of an actual bug I had in Hoverbattles.

*Player*

    moveLeft: function(amount) {
      this.position[0] -= amount;
      this.raiseEvent('Moved', this.position);
    }

*Enemy*

    onPlayerMoved: function(newPosition) {
      this.playerPosition = newPosition;
    },
    doSomeLogic: function() {
       // Some calculation that indirectly modifies the array
       this.playerPosition[0] += 5;
    }

Okay, the above is quite obvious, but this kind of thing happened (in substantially more convoluted scenarios) with the outrageous result of player craft ending up where they should not be. (Especially in the network code where objects are receiving new state a lot of the time).

The answer is, if you're receiving an array from an event or command, to copy it over to your own internal value if you want to keep the state around for any length of time for future processing. (Ignore this at your peril unless you're smart and/or have lots of tests).

**Push it to the GPU**

Hoverbattles first particle system was written on the CPU, and looked something like this:

    var ParticleEngine = function(count) {
      this.particles = new Array(count);
      for(var x = 0; x < count ; x++) {
        this.particles[x] = {
          x: 0,
          y: 0
          velx: 0,
          vely: 0
        }
      }
    };
    
    ParticleEngine.prototype = {
      update: function() {
        for(var x = 0; x < count ; x++) {
          this.particles[x].x += this.particles[x].velx;
          // etc
        }
      }
    };

Yeah, this didn't go too well - I wanted... no, I needed many thousands of particles, and blocking the event loop on the browser by looping through large collections of objects is a big no no.

If you can push processing from the CPU on the browser, to the GPU using shaders, then you should, JavaScript is slow and not a suitable place to be playing with large loops of data.

Besides, GLSL is quite a pretty language to do it in:

    void main(void){
    
        float age = (time - aCreationTime);
        vec3 position = aVertexPosition + (aVelocity * age);
        vColour = aColour;
    
        vec3 vectorToPoint = (position - vCamera);
        float distanceSquared = abs(dot(vectorToPoint, vectorToPoint));
        float scale = clamp(distanceSquared, 1.0, 10000.0);      
    
        life = 1.0 - (age / aLifetime);
        life = clamp(life, 0.0, 1.0);
    
        gl_PointSize = (aSize * maxsize) / (scale / 100.0);
        gl_Position =  uProjection * uView * vec4(position, 1.0);
    }

**Relax**

Finally - something I covered previously - relax, there is no problem you cannot solve with a bit of patience, re-factoring, profiling, and debugging. :-)
