I'm writing an OData parser in OMeta, and [yesterday](/entries/writing-an-odata-parser---starting-at-the-beginning.html) I stopped with the realisation that I'd have to deal with paths of resources, each with their own potential identifier.

First off, I come back to this code and look up the rules for Resource Names in OData as I left that hole open yesterday and it is an itch I want to scratch.

I read the [Uri conventions](http://www.odata.org/documentation/uri-conventions#ResourcePath), the [ABNF](http://www.odata.org/media/30002/OData%20ABNF.html) and this leads me to [RFC3229](http://www.rfc-editor.org/rfc/rfc3229.txt) and I fell asleep.

I'll see if this has been covered in Rulemotion's codebase when I get back into the office on Tuesday as I'd prefer to do this properly.

**Recursing through resource paths**

Back to the matter at hand, I want to deal with arbitrary paths like so

    /model(1)/children(1)/otherchildren(1)/field

This shouldn't be *too hard*, although I am practising Mojito-driven-development this weekend so it does tax the brain cells that are still functioning.

What I want to achieve:

- I want to be able to parse multiple resource paths separated by a '/'
- I want to build up a model of this path (each with a name and a specified id)
- I want to once this is done, carry on parsing the rest of the path

Hmm, well simplest thing for this is to say that this is recursive and not change a thing at all about the initial expression

    OData = (
      (
        PathSegment:model -> model
      )
      | '/'
    ) 


    PathSegment = 
          '/'
          ResourceName:resource
          (
            ("(" Number:key ")")?
            (PathSegment: next)?
          ) -> { resource: resource, key: key, next: next }
        ,


All we do here is say 

- We expect a forward-slash, followed by the name of the addressed resource
- We then optionally expect a specified Id for that resource
- We then optionally expect another part of the path component

In this way, we'll recursively build up the path to the addressed resource in the model and end up being able to follow this down when working out what to do with our OData request.

**Links**

There is another part to this, which is that rather than addressing paths, we can address links, which are expressed similarly:

    /model(1)/$links/children

This can actually be dealt with in very much the same way:

    PathSegment = 
          '/'
          ResourceName:resource
          (
            ("(" Number:key ")")?
            (
              (seq("/$links") PathSegment:link)
            | PathSegment: next
            )?
          ) -> { resource: resource, key: key, link: link, property: next }
        ,


- We expect a forward-slash, followed by the name of the addressed resource
- We then optionally expect a specified Id for that resource
- We then expect a sequence of characters (/$links) followed by another path segment
- OR we expect another path segment
- But the above two are optional

The model is then built up recursively.

I think that actually I shouldn't be allowing further addressing once we've followed a link - but I'm not so sure about that so I'll ask when I get back into the office.
