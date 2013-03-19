I'm finally reaching the point where I can parse most of the OData conventions for Uris, which is nice!

A re-cap of where we are so far.

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

Wowsers, talk about an accidental blog series...

**Arithmetic operators**

What were they again?


    Add        Addition                /Products?$filter=Price add 5 gt 10
    Sub        Subtraction             /Products?$filter=Price sub 5 gt 10
    Mul        Multiplication          /Products?$filter=Price mul 2 gt 2000
    Div        Division                /Products?$filter=Price div 2 gt 4
    Mod        Modulo                  /Products?$filter=Price mod 2 eq 0

Ah yes,

Now, these are all the same, but operator precedence is important so the order in which we want to go through them is:

- Add/Sub
- Mul/Div/Mod

This is very similar to how we implemented And/Or although I'll write a few tests to make sure I get it right.

    test("/some/resource?$filterby=Price add 5 gt 10", "OData", function(result) {

      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'gt'", function() {
         assert.equal(result.options.$filterby[0], "gt")
      })
      it("lhr should be Price add 5", function() {
         var rhs = result.options.$filterby[1] 
         assert.equal(rhs[0], "add")
         assert.equal(rhs[1].name, "Price")
         assert.equal(rhs[2], 5)
      })
      it("rhr should be 10", function() {
         assert.equal(result.options.$filterby[2], 10)
      })
    })

This tells us that our 'add' operator has higher precedence than the comparisons (which makes sense). This'll mean we want to sneak it in somewhere after those comparisons. (Assuming in this scheme that And/Or have a higher precedence than add, and it seems to be that way)

    FilterLogicalExpression =
      FilterLogicalExpression:lhs
      FilterByOperand:op
      FilterAddExpression:rhs -> [op, lhs, rhs ]
    | FilterAddExpression
    ,

    FilterAddExpression =
      FilterAddExpression:lhs
      FilterAddOperand:op
      FilterByValue:rhs -> [ op, lhs, rhs ]
    | FilterByValue
    ,
    FilterAddOperand =
      spaces
      (
        seq("add")
      | seq("sub")
      ):op 
      spaces -> op
    ,

Simples, we insert it in the pipeline between "LogicalExpression" and "Checking the value" (Literal values have the highest precedence because they don't require any work)

And because Mul/etc have a higher precedence than Add, this exactly the same

    test("/some/resource?$filterby=Price mul 5 gt 10", "OData", function(result) {

      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'gt'", function() {
         assert.equal(result.options.$filterby[0], "gt")
      })
      it("lhr should be Price add 5", function() {
         var lhs = result.options.$filterby[1] 
         assert.equal(lhs[0], "mul")
         assert.equal(lhs[1].name, "Price")
         assert.equal(lhs[2], 5)
      })
      it("rhr should be 10", function() {
         assert.equal(result.options.$filterby[2], 10)
      })
    })

Like so

    FilterAddExpression =
      FilterAddExpression:lhs
      FilterAddOperand:op
      FilterMulExpression:rhs -> [ op, lhs, rhs ]
    | FilterMulExpression
    ,

    FilterMulExpression =
      FilterMulExpression:lhs
      FilterMulOperand:op
      FilterByValue:rhs -> [ op, lhs, rhs ]
    | FilterByValue
    ,


Now what I actually have to do is define operator precedence for mul/div etc independently. So I can't actually cheat and do

    FilterMulOperand =
      spaces
      (
        seq("mul")
      | seq("div")
      | seq("mod")
      ):op 
      spaces -> op
    ,

Like I have been doing, or when I write the following test, it will fail.

    test("/some/resource?$filterby=Price div Price mul 5 gt 10", "OData", function(result) {
        console.log(JSON.stringify(result))

      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'gt'", function() {
         assert.equal(result.options.$filterby[0], "gt")
      })
      var lexpr = result.options.$filterby[1] 

      it("should be Price div {expr}", function() {
        assert.equal(lexpr[0], "div")
        assert.equal(lexpr[1].name, "Price")
      })

      it("should be Price mul 5", function() {
        assert.equal(lexpr[2][0], "mul")
        assert.equal(lexpr[2][1].name, "Price")
        assert.equal(lexpr[2][2], 5)
      })

      it("rhr should be 10", function() {
         assert.equal(result.options.$filterby[2], 10)
      })
    })

What will happen here is we'll get

    [
      'gt',
      [
        'mul',
        [
          'div', 'Price', 'Price'
        ],
        5
      ],
      10
    ]

When what we clearly want is

    [
      'gt',
      [
        'div',
        'Price',
        [
          'mul', 'Price', '5'
        ]
      ],
      10
    ]


Or if you like

    ( (price / price) * 5 ) > 10

Instead of

    ( Price / (price * 5)  ) > 10

Which is a little bit different to say the least!

So, explicit operation order is what we want, and here is how get it:


**One massively explicit set of operator precedences...**

    FilterByOption = 
      seq("$filterby=")
      FilterByExpression:expr -> { name: "$filterby", value: expr }
    ,

    FilterByExpression =
      FilterAndExpression
    ,

*And is the least important in our hierarchy*

    FilterAndExpression =
      FilterAndExpression:lhs
      FilterAndOperand:op
      FilterLogicalExpression:rhs -> [ op, lhs, rhs ]
    | FilterLogicalExpression
    ,

*Followed by any logical expression*

    FilterLogicalExpression =
      FilterLogicalExpression:lhs
      FilterByOperand:op
      FilterAddExpression:rhs -> [op, lhs, rhs ]
    | FilterAddExpression
    ,

*Then we descend through our mathematical operators in reverse precedence order*

    FilterSubExpression =
      FilterSubExpression:lhs
      spaces seq("sub") spaces
      FilterAddExpression:rhs -> [ "sub", lhs, rhs ]
    | FilterAddExpression
    ,

    FilterAddExpression =
      FilterAddExpression:lhs
      spaces seq("add") spaces
      FilterModExpression:rhs -> [ "add", lhs, rhs ]
    | FilterModExpression
    ,

    FilterModExpression =
      FilterModExpression:lhs
      spaces seq("mod") spaces
      FilterDivExpression:rhs -> [ "mod", lhs, rhs ]
    | FilterDivExpression
    ,
    FilterDivExpression =
      FilterDivExpression:lhs
      spaces seq("div") spaces
      FilterMulExpression:rhs -> [ "div", lhs, rhs ]
    | FilterMulExpression
    ,

    FilterMulExpression =
      FilterMulExpression:lhs
      spaces seq("mul") spaces
      FilterByValue:rhs -> [ "mul", lhs, rhs ]
    | FilterByValue
    ,

    FilterByValue = 
      FilterNegateExpression
    | Number
    | QuotedText
    | PropertyPath
    ,

    FilterNegateExpression = 
      spaces
      seq("not")
      spaces
      (
        FilterByValue
      | '(' spaces FilterByExpression:expr spaces ')' -> expr
      ):value ->  [ "not", value ]
    ,


How cool is that??!!? That's pretty much the whole shebang wrapped up as far as expressing parsing goes, and now I can go trigger mad with nested and/or/sub/mul/etc - with the exception of the precedence operators which I'll add next!
