A quick re-cap of where we are so far in our OData OMeta Adventure.

- [Learning OMeta through JSON](/entries/building-a-basic-json-parser-in-ometa.html)
- [Introduction to the OData Parser](/entries/building-an-odata-parser-in-ometa.html)
- [First steps in writing the OData Parser](/entries/writing-an-odata-parser---starting-at-the-beginning.html)
- [Nested resource paths in OData](/entries/parsing-odata---nested-resource-paths.html)
- [Service operations in OData](/entries/parsing-odata---service-operations.html)
- [Query options in OData](/entries/the-odata-parser---applying-modifiers-to-our-query.html)
- [Paging support in OData](/entries/paging-support-in-our-odata-parser.html)
- [Filtering support in OData](/entries/our-odata-parser---looking-at-filterby.html)

**CRIKEY**

Never thought I'd be writing this much about OMeta, but I've taken quite the shine to it.

I did basic expressions, but actually expressions can be made up of other expressions so I may as well bite that bullet and get on with it.

Let's look at what we might expect if we use 'and'

    test("/some/resource?$filter=Price gt 5 and Price lt 10", "OData", function(result) {
      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'and'", function() {
         assert.equal(result.options.$filterby[0], "and")
      })

      it("Left hand side should be Price gt 5", function() {
         var lhs = result.options.$filterby[1] 
         assert.equal(lhs[0], "gt")
         assert.equal(lhs[1].name, "Price")
         assert.equal(lhs[2], 5)
      })

      it("Right hand side should be less than 10", function() {
         var rs = result.options.$filterby[2] 
         assert.equal(rhs[0], "lt")
         assert.equal(rhs[1].name, "Price")
         assert.equal(rhs[2], 10)
      })
    })

We have a tree that looks like

    [ 'and', 
      [ 'gt', 'Price', 5 ],
      [ 'lt', 'Price', 10]
    ]

Our next step can walk this tree and generate SQL or something similar very easily.

**How do we generate such a thing?**

Well, 'and' is an operator with the lowest precedence (well, the same as 'or'), and therefore we want it to be first in the tree (it makes senseif you think about it!). 

The leaves should be the nodes with the highest precedence because we'll get their results first and them go up to the root node.

This suggests we need to cascade through the preferable options until we find something we like:

There is quite a wall here, so let's break it down

      FilterByOption = 
        seq("$filterby=")
        FilterByExpression:expr -> { name: "$filterby", value: expr }

*When we find $filterby, then parse the expression*


      FilterByExpression =
        FilterAndExpression


*The first thing we want to find is an 'and' expression*


      FilterAndExpression =
        FilterAndExpression:lhs
        FilterAndOperand:op
        FilterLogicalExpression:rhs -> [ op, lhs, rhs ]
      | FilterLogicalExpression


*try and find more AndExpressions*

*Else, let the right hand side be the next preferable thing (a plain old logical expression)*

*Else, just try to find a logical expression*


      FilterLogicalExpression =
        FilterLogicalExpression:lhs
        FilterByOperand:op
        FilterByValue:rhs -> [ op, lhs, rhs ]
      | FilterByValue


*Try to find more LogicalExpressions*

*Else, let the right hand side be the next preferable thing (a plain old value)*

*Else fall back to finding a value*


      FilterAndOperand =
        spaces
        (
          seq("and")
        | seq("or")
        ):op 
        spaces -> op


*And/or have the same precedence*


      FilterByOperand =
        spaces
        (
          seq("eq")
        | seq("ne")
        | seq("gt")
        | seq("ge")
        | seq("lt")
        | seq("le")
        ):op 
        spaces -> op


*These are unchanged*


      FilterByValue = 
        Number
      | QuotedText
      | PropertyPath


*As are these*




