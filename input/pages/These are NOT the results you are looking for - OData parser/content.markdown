A quick re-cap of where we are so far in our OData OMeta Adventure.

- [Learning OMeta through JSON](/entries/building-a-basic-json-parser-in-ometa.html)
- [Introduction to the OData Parser](/entries/building-an-odata-parser-in-ometa.html)
- [First steps in writing the OData Parser](/entries/writing-an-odata-parser---starting-at-the-beginning.html)
- [Nested resource paths in OData](/entries/parsing-odata---nested-resource-paths.html)
- [Service operations in OData](/entries/parsing-odata---service-operations.html)
- [Query options in OData](/entries/the-odata-parser---applying-modifiers-to-our-query.html)
- [Paging support in OData](/entries/paging-support-in-our-odata-parser.html)
- [Filtering support in OData](/entries/our-odata-parser---looking-at-filterby.html)
- [Recursive query support in OData](/entries/recursive-expression-parsing-in-our-odata-filter-parser.html)

Before we get onto some more fuzzy stuff, it would be good to deal with 'not'

Not is an interesting case because it's not really  the same as the rest of the expressions so far

    Not  Logical negation  /Products?$filter=not endswith(Description,'milk')


It can't apply in situations like this however
    
    /Products?$filter=not Product eq 5

But it can probably apply in situations like this

    /Products?$filter=not Published

This tells us something, which is that not isn't going to be expecting an expression unless it's in braces, otherwise it's going to be looking for a value.

I'm not supporting methods yet, so I'll ignore that requirement for now.

    test("/some/resource?$filterby=not Published", "OData", function(result) {

      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'not'", function() {
         assert.equal(result.options.$filterby[0], "not")
      })

      it("value should be 'Published'", function() {
        assert.equal(result.options.$filterby[1].name, "Published")
      })
    })

First off, this one

    FilterByValue = 
      FilterNegateExpression
    |  Number
    | QuotedText
    | PropertyPath
    ,

    FilterNegateExpression = 
      spaces
      seq("not")
      spaces
      FilterByValue:value       ->  [ "not", value ]
    ,

Quite simple, if we get as far as checking for a value, it means we haven't found an expression, so we'll check for "Not" - I guess this is where we'll probably stick methods later too. 

A "NegateExpression" is simply a "not" followed by a value, we'll allow double negatives too because there is no harm in this.

Next up, we'll check for braces, and inside braces we'll allow further expressions (recurse recurse recurse)

    test("/some/resource?$filterby=not (Price gt 5)", "OData", function(result) {

      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'not'", function() {
         assert.equal(result.options.$filterby[0], "not")
      })
      it("Value should be Price gt 5", function() {
         var rhs = result.options.$filterby[1] 
         assert.equal(rhs[0], "gt")
         assert.equal(rhs[1].name, "Price")
         assert.equal(rhs[2], 5)
      })
    })

Solved by

    FilterNegateExpression = 
      spaces
      seq("not")
      spaces
      (
        FilterByValue
      | '(' spaces FilterByExpression:expr spaces ')' -> expr
      ):value ->  [ "not", value ]
    ,

Dead easy, now it's time to get our heads stuck into Arithmetic Operators.
