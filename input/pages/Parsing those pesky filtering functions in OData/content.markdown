The final bit of our $filter feature is the ability to invoke a special function and compare the result of that to the rest of an expression.

Again a reminder of where we are so far:


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
- [Precedence grouping in OData](/entries/precedence-grouping,-you-first..-no-you-odata-parser.html)

**Function calls look like this**

    /Customers?$filter=substringof('Alfreds', CompanyName) eq true

In other words, they're a special piece of the expression which can take a list of expressions separated by commas

I could hack this and just allow any function call, or I can explicitly name them all - which I'll do because it'll help with the highlighting in the editor that'll use this parser.

I'll do the first one here, and then go and do the rest in a similar fashion because they're just the same thing over and over again!

Our test for 'substringof'

    test("/resource?$filterby=substringof('alfred', Product) eq 'cake'", "OData", function(result) {
      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'eq'", function() {
         assert.equal(result.options.$filterby[0], "eq")
      })
      it("lhs should be a function call", function() {
         assert.equal(result.options.$filterby[1][0], "call")
      })
      it("lhs should be substringof with correct args", function() {
         assert.equal(result.options.$filterby[1][1].method, 'substringof')
         assert.equal(result.options.$filterby[1][1].args[0], 'alfred')
         assert.equal(result.options.$filterby[1][1].args[1].name, 'Product')
      })
      it("rhs should be cake", function() {
         assert.equal(result.options.$filterby[2], "cake")
      }) 
    })

This kinda thing will do, and getting the method out is a simple  matter of adding the MethodExpression to the values possible in an expression:

    FilterByValue = 
      FilterMethodCallExpression
    | FilterNegateExpression
    | Number
    | QuotedText
    | PropertyPath
    | GroupedPrecedenceExpression
    ,

Now, it is tempting to be lazy and just write a  generic method recogniser with variable lists of args, but we're building for highlighting so it would be nice to know what the recognised methods are, and what args they expect, so what I'll do is this

    FilterMethodCallExpression = 
      (
        FilterSubstringOf
      | OtherMethod
      | AnotherMethod
      ) -> [ "call", methodcall ]
    ,

And write a definition for each method (tedious, but I'll automate a pile of that with VIM macros)

    FilterSubstringOf = 
      seq('substringof'):method 
      '(' 
      spaces 
      FilterByExpression:one 
      spaces 
      ',' 
      spaces 
      FilterByExpression:two 
      spaces 
      ')' -> { args: [ one, two ], method: method }

And they'll all look like that.

With this done, $filter is now fully supported and I can get on with mopping up the final recognised pieces of OData. I'll try and do that all in a single post.


