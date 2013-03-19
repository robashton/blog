A quick re-cap of where we are so far:

- [Learning OMeta through JSON](/entries/building-a-basic-json-parser-in-ometa.html)
- [Introduction to the OData Parser](/entries/building-an-odata-parser-in-ometa.html)
- [First steps in writing the OData Parser](/entries/writing-an-odata-parser---starting-at-the-beginning.html)
- [Nested resource paths in OData](/entries/parsing-odata---nested-resource-paths.html)
- [Service operations in OData](/entries/parsing-odata---service-operations.html)
- [Query options in OData](/entries/the-odata-parser---applying-modifiers-to-our-query.html)
- [Paging support in OData](/entries/paging-support-in-our-odata-parser.html)

Now I need to tackle $filterby, which is bit of a mammoth, as can be seen from the description from the OData Uri conventions

    Eq         Equal                   /Suppliers?$filter=Address/City eq 'Redmond'
    Ne         Not equal               /Suppliers?$filter=Address/City ne 'London'
    Gt         Greater than            /Products?$filter=Price gt 20
    Ge         Greater than or equal   /Products?$filter=Price ge 10
    Lt         Less than               /Products?$filter=Price lt 20
    Le         Less than or equal      /Products?$filter=Price le 100
    And        Logical and             /Products?$filter=Price le 200 and Price gt 3.5
    Or         Logical or              /Products?$filter=Price le 3.5 or Price gt 200
    Not        Logical negation        /Products?$filter=not endswith(Description,'milk')
    Add        Addition                /Products?$filter=Price add 5 gt 10
    Sub        Subtraction             /Products?$filter=Price sub 5 gt 10
    Mul        Multiplication          /Products?$filter=Price mul 2 gt 2000
    Div        Division                /Products?$filter=Price div 2 gt 4
    Mod        Modulo                  /Products?$filter=Price mod 2 eq 0
    ( )        Precedence grouping     /Products?$filter=(Price sub 5) gt 10

And this is before we even have a look at the supported *"functions"* (we'll leave these until the next entry I think!)

Thankfully this is all pretty much the same deal and boils down to simple recursive expression parsing. 

**Implementing Eq**

I'll not do this for all of them, but you can assume I've just implemented them the same way only with "Ge, etc" substituted for whatever...

    test("/some/resource?$filterby=Foo eq 2", "OData", function(result) {
      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'eq'", function() {
         assert.equal(result.options.$filterby[0], "eq")
      })
      it("lhr should be Foo", function() {
         assert.equal(result.options.$filterby[1].name, "Foo")
      })
      it("rhr should be 2", function() {
         assert.equal(result.options.$filterby[2], 2)
      })
    })

The idea for this stuff is that I want to generate an AST for further processing by say, a SQL generator. The easiest way to do this is to generate arrays for consumption. This can be ran through a further OMeta processing step to generate SQL later on.

I'm not so comfortable with the bit where I address the filterby[1].name, it feels as addressing down a path should be dealt with in the same way as the rest of the AST (perhaps everywhere else I should be generating an array instead of those nested objects).

I actually have some other ideas about how I'd do this so I'll park that as well (as I'm having a conversation and review of this code tomorrow in the office)

**The implementation**

    FilterByOption = 
      seq("$filterby=")
      FilterByExpression:expr -> { name: "$filterby", value: expr }
    ,
    FilterByExpression =
      PropertyPath:lhs
      seq(" eq ")
      Number:rhs           -> [ "eq", lhs, rhs ]
    ,

So I'm keeping it simple by making some assumptions that'll get proved wrong in a sec

- Only accepting number for rhs
- Only accepting 'eq as the operand
- PropertyPath is probably not the way to go for this in its current incarnation as mentioned above


**Adding not equals**

    test("/some/resource?$filterby=Foo ne 2", "OData", function(result) {
      it("A filter should be present", function() {
         assert.notEqual(result.options.$filterby, null)
      })
      it("Filter should be an instance of 'ne'", function() {
         assert.equal(result.options.$filterby[0], "ne")
      })
      it("lhr should be Foo", function() {
         assert.equal(result.options.$filterby[1].name, "Foo")
      })
      it("rhr should be 2", function() {
         assert.equal(result.options.$filterby[2], 2)
      })
    })


Can be dealt with by saying that our Operand is a choice

    FilterByOption = 
      seq("$filterby=")
      FilterByExpression:expr -> { name: "$filterby", value: expr }
    ,

    FilterByExpression =
      PropertyPath:lhs
      FilterByOperand:op
      Number:rhs           -> [ op, lhs, rhs ]
    ,

    FilterByOperand =
      seq(" eq ") -> "eq"
    | seq(" ne ") -> "ne"


Can now do the same for 

- Gt
- Ge
- Lt
- Le

Like so

    FilterByOperand =
      seq(" eq ") -> "eq"
    | seq(" ne ") -> "ne"
    | seq(" gt ") -> "gt"
    | seq(" lt ") -> "lt"
    | seq(" le ") -> "le"


And I'll parameterise the test to get this covered easily and document my progress

    function operandTest(op) {
      test("/some/resource?$filterby=Foo " + op + " 2", "OData", function(result) {
        it("A filter should be present", function() {
           assert.notEqual(result.options.$filterby, null)
        })
        it("Filter should be an instance of '" + op + "'", function() {
           assert.equal(result.options.$filterby[0], op)
        })
        it("lhr should be Foo", function() {
           assert.equal(result.options.$filterby[1].name, "Foo")
        })
        it("rhr should be 2", function() {
           assert.equal(result.options.$filterby[2], 2)
        })
      })
    }
    operandTest("eq")
    operandTest("ne")
    operandTest("gt")
    operandTest("lt")
    operandTest("le")


**Not everything is a number**

Now for the next thing, what can we have as that Rhs? Well, let's go with

- Number (eg 2, 3, 5 )
- QuotedText (eg 'foo', 'bar')

As that's what I can think of from the docs

Here is a test for the quoted string:

      test("/some/resource?$filterby=Foo eq 'bar'", "OData", function(result) {
        it("A filter should be present", function() {
           assert.notEqual(result.options.$filterby, null)
        })
        it("Filter should be an instance of 'eq'", function() {
           assert.equal(result.options.$filterby[0], op)
        })
        it("lhr should be Foo", function() {
           assert.equal(result.options.$filterby[1].name, "Foo")
        })
        it("rhr should be 2", function() {
           assert.equal(result.options.$filterby[2], 'bar')
        })
      })

Same deal again, let's make this extendable

    FilterByExpression =
      PropertyPath:lhs
      FilterByOperand:op
      FilterByValue:rhs           -> [ op, lhs, rhs ]
    ,

With

    FilterByValue = 
      Number
    | QuotedText

Where QuotedText looks something like this:

    QuotedText =
      '\''
      Text:t 
      '\'' -> t
    ,

Imaginative.

Next up we'll have to think about what else we can expect to see in our expressions - thinking about the Arithmetic operators and grouping operators. *shudder*


