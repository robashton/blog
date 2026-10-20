I've got [basic resource parsing](/entries/writing-an-odata-parser---starting-at-the-beginning.html), [path parsing](/entries/parsing-odata---nested-resource-paths.html), and [service operations](/entries/parsing-odata---service-operations.html) sussed, now I need to begin the grand delving into parsing all of the query options that can be applied to an OData request.

There is quite the myriad of query string options that can be applied in a conventional OData request, and they all begin with $ and have some sort of expression after it.

- $orderby (order by some sort of field)
- $top (paging support)
- $skip (paging support)
- $filter (expressions passed in to do filtering - basically LINQ support and SQL server support &lt;/cynicism&gt;)

These all appear at the very end of the query string like so

    /resource/child?$orderby=Rating asc
    /resource?$top=10
    /resource?$orderby=Rating,Category/Name desc
    /resource?$filter=Rating eq 5

As can be seen, the complexity of these can grow quite easily but in reality they're pretty much a combination of

 - the query option itself
 - an expression that is unique to that query option

Also, we can re-use any rules we already have about parsing resource names (except we don't want to allow id specification, etc)
I imagine when trying to re-use those rules I'll probably sort out some of the niggles I've overlooked so far too!

So I guess what we need to do is parse the resource path and then apply any of these modifiers to it. There is actually a fair amount of support for this already in the Rulemotion code that I'm looking to replace so I'll likely be able to copy most of it out as I write the tests for it.

**$orderby**

OrderBy seems quite trivial so we'll start with that

    test("/resource?$orderby=Property", "OData", function(result) {
      it("sort options are present on the result", function() {
         assert.notEqual(result.options.$orderby, null)
      })
      it("sort options have the property specified", function() {
         assert.equal(result.options.$orderby.property, "Property")
      })
    })

I'm quite excited about this as how this model presents itself will probably help me sort out the main model I'm creating to support the OData request.

I'll start off with:

    OData = (
      (
        PathSegment:model 
        (
          '?'
          ( listOf(`QueryOption, '&'):options
          )
        )?
      ) -> { 
              if(options) {
               model.options = {}
               for(var i in options)
                 model.options[options[i].name] = options[i].value;
              }
             return model
           }
      | '/'
    ) 

I'll simply look for a list of query options, and actually merge the notion of service parameters and known query options (and in fact, custom options too - which are simply any unrecognised options - without a dollar sign in front of them)

What does this look like? 

    QueryOption = 
      SortOption
    | OperationParam
    ,

    SortOption = 
      seq("$orderby=")
      ResourceName:property -> { name: '$orderby', value = { property: property }}

Well we'll start off with the simplest one, which is just supporting a single property and no options. I don't want to get too ahead of myself after all.

This works but of course I've broken my tests for service parameters now, so I'll fix those

    test("/method(1)/child?foo=bar", "OData", function(result) {
        console.log(result)
      it("should have the resource specified", function() {
         assert.equal(result.resource, 'method')
      })
      it("The result should be addressed", function() {
         assert.equal(result.key, '1')
      })

      it("should have the path specified", function() {
         assert.equal(result.property.resource, 'child')
      })

      it("should have the argument specified", function() {
         assert.equal(result.options.foo, 'bar')
      })
    })

**A bit more $orderby**

Not done by a long-shot, we have several aspects to $orderby

- We can order by a single property
- We can order by several properties
- A property can be specified by a path to that property
- We can order ASC or DESC

First off, let's deal with multiple properties - it seems this will be easiest

    test("/resource?$orderby=PropertyOne,PropertyTwo", "OData", function(result) {
      it("sort options are present on the result", function() {
         assert.notEqual(result.options.$orderby, null)
      })
      it("sort options have the first property specified", function() {
         assert.equal(result.options.$orderby.properties[0].name, "PropertyOne")
      })
      it("sort options have the second property specified", function() {
         assert.equal(result.options.$orderby.properties[1].name, "PropertyTwo")
      })
    })

Yeugh, our data model is getting quite convoluted - I'll be working on that for sure.

But first...

    SortOption = 
      seq("$orderby=")
      listOf(`SortProperty, ','):properties -> { name: '$orderby', value: { properties: properties }  }
    ,

    SortProperty = 
      ResourceName:property -> { name: property }

    ,

Pretty tidy, and this opens up the avenue of being able to specify ASC or DESC very easily

*DESC*

    test("/resource?$orderby=PropertyOne desc", "OData", function(result) {
      it("sort options are present on the result", function() {
         assert.notEqual(result.options.$orderby, null)
      })
      it("sort options have the property specified", function() {
         assert.equal(result.options.$orderby.properties[0].name, "PropertyOne")
      })
      it("sort options have the property ordering specified", function() {
         assert.equal(result.options.$orderby.properties[0].order, "desc")
      })
    })

*ASC*

    test("/resource?$orderby=PropertyOne asc", "OData", function(result) {
      it("sort options are present on the result", function() {
         assert.notEqual(result.options.$orderby, null)
      })
      it("sort options have the property specified", function() {
         assert.equal(result.options.$orderby.properties[0].name, "PropertyOne")
      })
      it("sort options have the property ordering specified", function() {
         assert.equal(result.options.$orderby.properties[0].order, "asc")
      })
    })

*Double trouble*


    test("/resource?$orderby=PropertyOne asc,PropertyTwo desc", "OData", function(result) {
      it("sort options are present on the result", function() {
         assert.notEqual(result.options.$orderby, null)
      })
      it("sort options have property one name specified", function() {
         assert.equal(result.options.$orderby.properties[0].name, "PropertyOne")
      })
      it("sort options have property one ordering specified", function() {
         assert.equal(result.options.$orderby.properties[0].order, "asc")
      })
      it("sort options have the property two name specified", function() {
         assert.equal(result.options.$orderby.properties[1].name, "PropertyTwo")
      })
      it("sort options have the property two ordering specified", function() {
         assert.equal(result.options.$orderby.properties[1].order, "desc")
      })
    })

Adding this is *super* easy

    SortProperty = 
      ResourceName:property
      (
        seq(" asc") -> "asc"
      | seq(" desc") -> "desc"
      )?:order
      -> { name: property, order: order }

Simply check for the resource name, then optionally check for an "asc" or a "desc", before bundling this into an object.

**And the final bit - property paths**

I already technically have this written in the form of the expression "PathSegment", but that's a bit too coupled to the main query path, and un-coupling it would be tricky - so it's better off just to write a new expression for matching paths within query options.

First off, the test I want to pass



*Reminder: This is the PathSegment I put together last time*

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

We just need a subset of this, let's call it PropertyPath

    PropertyPath = 
          ResourceName:resource
          (
            '/'
            PropertyPath: next
          )? -> { name: resource, property: next}
    ,

And with a slight modification to our SortOptions to use this new expression:

    SortProperty = 
      PropertyPath:property
      (
        seq(" asc") -> "asc"
      | seq(" desc") -> "desc"
      )?:order
      -> {
           property.order = order;
           return property;
         }

And we're done.

Next up, I'll tackle the paging stuffs, as that should be simple.



