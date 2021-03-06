Now I've got myself up to speed in OMeta (which took me an embarassingly difficult couple of days!), I can begin work on a "programming task" for the rest of my stay at Rulemotion's office in Athens.

**First off, the context**

Rulemotion is doing some pretty cool stuff with SBVR relating to Model First Development. It goes a little bit like this:

- Write about your system in SBVR
- Use OMeta to parse this information
- Generate a database with validation/constraints
- Generate an OData API over the top of this system

It's a little *more* than that, but the discussion of "what the products are" would take a few blog posts in themselves and the task of going over this topic is more related to the other half of my job whilst here at Rulemotion (plus, I'm neither employed or qualified to write marketing materials!).

What I *can* say is that the general trend at Rulemotion seems to be "Use OMeta for parsing things", and in doing this they automatically get a lot of things for free (chiefly, the editor that they're using/creating for supporting development gets its auto-completion and highlighting from OMeta, which is pretty super.)

What we actually have at the moment is a situation that has evolved like so:

- There was a custom HTTP API
- They moved to OData
- They wrote a partial OData parser on top of a lot of their other code
- This OData parser is dependent on the underlying model generated by SBVR
- It would be preferable if the OData parser was independent and more complete in itself
- Then the model generated by the parser can be consumed by their code and related to the model

There is also hope to open source the OData parser too, which is also super cool.

**Where do I come in?**

- Well, my first task was to get up to speed on OMeta, I can tick that one off well enough!
- My next task is to add some features to the existing OData parser, I manage this after stepping through the generated JS to understand more about what is going on and writing some tests around what seems to be desired
- At this point there is a decision to make about how to proceed

It is going to start being annoying building up a pile of OData parsing code that is coupled to the underlying model, so I'm going to give writing the OData parser a go, and at least try to reach feature parity with what they have now (I can re-use quite a lot of the code).

There is a little bit of discussion based around a semi-facetious remark I made about using OMeta to parse the ABNF for OData into OMeta, but while this would provide a more "complete" solution, would still involve having to write the semantic output code and might not necessarily be the best use of time given they only need a subset of OData.

So I've decided to give writing an OData parser a go over the weekend and see how far I get, I'll be holidaying it up on a Greek island off the coast of Athens and applying Mojito Driven Development so I make no promises about productivity...
