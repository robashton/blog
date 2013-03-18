As a reminder as to where we've gotten to so far, these are the tests that are currently passing:

    Parsing /
      ✓ Service root should have no model 

    Parsing /model
      ✓ should have the resource specified 

    Parsing /model(1)
      ✓ should have the resource specified 
      ✓ should have the key specified for the source 

    Parsing /model(1)/child
      ✓ should have the resource specified 
      ✓ should have the key specified for the resource 
      ✓ should have the child specified 

    Parsing /model(1)/$links/Child
      ✓ should have the resource specified 
      ✓ should have the key specified for the resource 
      ✓ should have the link specified 

    Parsing /method(1)/child?foo=bar
      ✓ should have the resource specified 
      ✓ The result should be addressed 
      ✓ should have the path specified 
      ✓ should have the argument specified 

    Parsing /resource?$orderby=Property
      ✓ sort options are present on the result 
      ✓ sort options have the property specified 

    Parsing /resource?$orderby=PropertyOne,PropertyTwo
      ✓ sort options are present on the result 
      ✓ sort options have the first property specified 
      ✓ sort options have the second property specified 

    Parsing /resource?$orderby=PropertyOne desc
      ✓ sort options are present on the result 
      ✓ sort options have the property specified 
      ✓ sort options have the property ordering specified 

    Parsing /resource?$orderby=PropertyOne asc
      ✓ sort options are present on the result 
      ✓ sort options have the property specified 
      ✓ sort options have the property ordering specified 

    Parsing /resource?$orderby=PropertyOne asc,PropertyTwo desc
      ✓ sort options are present on the result 
      ✓ sort options have property one name specified 
      ✓ sort options have property one ordering specified 
      ✓ sort options have the property two name specified 
      ✓ sort options have the property two ordering specified 

    Parsing /resource?$orderby=PropertyOne/SubProperty
      ✓ sort options are present on the result 
      ✓ sort options have property one name specified 
      ✓ sort options have property one's sub property specified 

Which is nice. Now I want to add paging support in the form of 

$top and $skip, while I'm at it I may as well add support for $inlinecount because it's pretty much the same thing.

This should be fairly easy, this is what I want to support:

    /some/path?$top=5&limit=skip=100
    /some/path?$inlinecount=allpages

**Top and Skip**

These are quite simple, just text and a number, let's write a couple of tests

    test("/some/resource?$top=5&$skip=100", "OData", function(result) {
      it("top should be specified", function() {
         assert.equal(result.options.$top, 5)
      })
      it("skip should be specified", function() {
         assert.equal(result.options.$skip, 100)
      })
    })

I just need to add these to the list of recognised query options

    QueryOption = 
        SortOption
      | TopOption
      | SkipOption
      | OperationParam
    ,

This is where the elegance of OMeta makes me really happy, being able to easily say what the options for something are in this way is really pretty.

So, TopOption

    TopOption = 
      seq("$top=") Number:value -> { name: "$top", value: value }
    ,

and SkipOption

    SkipOption = 
      seq("$skip=") Number:value -> { name: "$skip", value: value }
    ,

Can't say fairer than that!

**inlinecount**

This one is a bit more interesting, the only valid options are *none* and *allpages*, and we're supposed to return a 404 if we don't match. We're not currently doing anything with HTTP in this parser so what I'll actually do is accept "any text" and leave it up to the consumer to do this job for us. (Rather than throw a generic *I can't parse this* exception)


    test("/some/resource?$inlinecount=allpages", "OData", function(result) {
      it("inline should be specified", function() {
         assert.equal(result.options.$inlinecount, "allpages")
      })
    })

    test("/some/resource?$inlinecount=none", "OData", function(result) {
      it("inline should be specified", function() {
         assert.equal(result.options.$inlinecount, "none")
      })
    })

    test("/some/resource?$inlinecount=flibble", "OData", function(result) {
      it("inline should be specified", function() {
         assert.equal(result.options.$inlinecount, "")
      })
    })

I want explicit handling for this because it'll help with the highlighting efforts in the editor that  this will be used in.


We can add it like so

    QueryOption = 
        SortOption
      | TopOption
      | SkipOption
      | InlineCountOption
      | OperationParam
    ,

And handle our explcit decisions like so

    InlineCountOption =
      seq("$inlinecount=") 
      (
        seq("allpages") -> "allpages"
      | seq("none") -> "none"
      | Text -> ""
      ):value -> { name: "$inlinecount", value: value }
    ,

**Et voila**

So that's paging done and dusted, incredibly simple when you know how. Next up we'll explore the murky world of OData filtering.
