I'm going off what is specified in both the OData spec and the OData URI conventions document.

This is a bit annoying, because it seems like URI conventions are just that, conventions - and people are free to do what they want (I haven't looked at the metadata spec yet so I'm not sure how discoverable this customisability is, I guess I'll get there during my time on this task)

What I think I can start with, is parsing the following basics

- The service root itself (http://example.com/service/odata.svc for example)
- An entity at this root ( /model )
- An entity with a key ( /model(1) )


**How I'll develop this**

This is yet another task I'll probably write tests for as I go so I can document how far I've gotten and have a safety net as I no doubt make lots of mistakes.

I'll copy and paste code from the old OData parser as I need it and as I write the tests to support it, in this even the legacy code will end up with coverage.

In this way, I hope to be able to hand this over to Rulemotion in its semi-complete state but with a nice document (the tests) explaining what is covered so far.

**My first few tests**

I'll not bother covering the order in which I do this, as it's pretty similar to how I did the JSON parser, except I can make a few more assumptions because I know a but more about how OMeta works.

    function test(input, entry, expectation) {
      describe("Parsing " + input, function() {
        var parser = ODataParser.createInstance()
        var result = parser.matchAll(input, entry)
        expectation(result)
      });
    }

    test("/", "OData", function(result) {
      it("Service root should have no model", function() {
         assert.equal(result.resource, null)
      })
    })

    test("/model", "OData", function(result) {
      it("should have the resource specified", function() {
         assert.equal(result.resource, 'model')
      })
    })

    test("/model(1)", "OData", function(result) {
      it("should have the resource specified", function() {
         assert.equal(result.resource, 'model')
      })
      it("should have the key specified for the source", function() {
         assert.equal(result.key, '1')
      })
    })

After the first couple of tests, setting up the parser etc was a ball-ache so I fixed it.

I'm missing out the bit where I specify what the service root is, I'll come back to it later as I'm more interested in parsing the path itself.


This is what I wrote to support the above tests:

    ometa ODataParser 
      Number = <digit+>:d -> parseInt(d, 10),

      OData = (
          PathSegment
        | '/'
      ) 
      ,

      PathSegment = 
          '/'
            ResourceName:resource
            (
              ("(" Number:key ")")?
            ) -> { resource: resource, key: key }
          ,

      ResourcePart =
        <	(	letter
          |	'_'
          )+
        >:resourcePart
        -> resourcePart.replace(new RegExp('_', 'g'), ' '),

      ResourceName =
        <	ResourcePart
          (	'-'
            ResourcePart
          )*
        >
    }

Things of note

- I'm currently returning the model from PathSegment as { resource: resource, key: key }, I'll end up making something else for this I think
- The "key" is optional, if it's not there then it will simply be undefined, this is what that question mark is for after those braces
- The ResourcePart and ResourceName are copied from the old code and simply convert underscores into spaces, I haven't bothered writing tests for this as I've not checked what ODatas rules are for entity names yet (It's likely to be a bit more complicated than "any text at all")


This is all very rudimentary, now - looking at the URI conventions, they seem to support arbitrary paths into object relationships like so:

    /model(1)/children(1)/otherchildren(1)/field

This suggests I probably want to recurse in order to build up this sequence

    test("/model(1)/child", "OData", function(result) {
      it("should have the resource specified", function() {
         assert.equal(result.resource, 'model')
      })
      it("should have the key specified for the resource", function() {
         assert.equal(result.key, '1')
      })
      it("should have the child specified", function() {
         assert.equal(result.resource.next, 'child')
      })
    })

Not sure if this is an appropriate representation, but it'll do for now until I find out how we're going to be consuming this model.

Having just arrived at the hotel and written this all on the boat, I'll defer having a look at how to do this until tomorrow, I've already passed the balmers peak.

