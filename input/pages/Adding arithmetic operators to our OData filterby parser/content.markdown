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

I want to add this stuff to my $filterby parser now.

    Add        Addition                /Products?$filter=Price add 5 gt 10
    Sub        Subtraction             /Products?$filter=Price sub 5 gt 10
    Mul        Multiplication          /Products?$filter=Price mul 2 gt 2000
    Div        Division                /Products?$filter=Price div 2 gt 4
    Mod        Modulo                  /Products?$filter=Price mod 2 eq 0

Now here's the thing, the way we look at this *actually*, is that we have two expressions in each of these, and there is a precedence here too which needs respecting.


Anyway, what do we expect to see when we do this?

    test("/some/resource?$filter=Price add 5 gt 10", "OData", function(result) {
      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'add'", function() {
         assert.equal(result.options.$filterby[0], "add")
      })

      it("lhr should be Price", function() {
         assert.equal(result.options.$filterby[1].name, "Price")
      })

      it("rhr should be lhs ", function() {
         assert.equal(result.options.$filterby[2], 2)
      })
    })




