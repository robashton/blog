With expression parsing out of the way, there are just a few complications remaining, 

- [Learning OMeta through JSON](/entries/building-a-basic-json-parser-in-ometa.html)
- [Introduction to the OData Parser](/entries/building-an-odata-parser-in-ometa.html)
- [First steps in writing the OData Parser](/entries/writing-an-odata-parser---starting-at-the-beginning.html)
- [Nested resource paths in OData](/entries/parsing-odata---nested-resource-paths.html)
- [Service operations in OData](/entries/parsing-odata---service-operations.html)
- [Query options in OData](/entries/the-odata-parser---applying-modifiers-to-our-query.html)
- [Paging support in OData](/entries/paging-support-in-our-odata-parser.html)
- [Filtering support in OData](/entries/our-odata-parser---looking-at-filterby.html)
- [Recursive query support in OData](/entries/recursive-expression-parsing-in-our-odata-filter-parser.html)
- ['Not' support for OData](/entries/these-are-not-the-results-you-are-looking-for---odata-parser.html)
- [Arithmetic operator support in OData](/entries/adding-arithmetic-operators-to-our-odata-parser.html)

I'm still missing a few key parts of this $filter feature, the next one is precedence grouping.

**Do all this stuff before you do the other stuff**

I don't think this will be too hard to parse given our already built-up knowledge of how to do precedence.

To the test..

    test("/some/resource?$filterby=(Price div Price) mul 5 gt 10", "OData", function(result) {
      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'gt'", function() {
         assert.equal(result.options.$filterby[0], "gt")
      })
      var lexpr = result.options.$filterby[1] 

      it("should be {expr} mul 5", function() {
        assert.equal(lexpr[0], "mul")
        assert.equal(lexpr[2], 5)
      })

      it("should be {Price div Price}", function() {
        assert.equal(lexpr[1][0], "div")
        assert.equal(lexpr[1][1].name, "Price")
        assert.equal(lexpr[1][2].name, "Price" )
      })

      it("rhr should be 10", function() {
         assert.equal(result.options.$filterby[2], 10)
      })
    })

This is actually the same test as in our [Arithmetic Operators](/entries/adding-arithmetic-operators-to-our-odata-parser.html) post, only we've surrounded the div expression because we want that to happen all by itself.

Well, Brackets actually have the *highest* precedence, so they'll need to go at the very end of our parser.

    FilterByValue = 
      FilterNegateExpression
    | Number
    | QuotedText
    | PropertyPath
    | GroupedPrecedenceExpression
    ,

    GroupedPrecedenceExpression = 
      "(" spaces FilterByExpression:expr spaces ")" -> expr

    ,

Yes, it really is that simple. This is what comes of building up the definition of a parse-target out of little building blocks and then making a sequence of them.

If only all programs worked like this.
