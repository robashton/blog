So yes, we **are** nearly there, in fact we only have a few query options remaining, which I'll cover entirely here because they're all pretty miniscule.

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
- [Filter query methods in OData](/entries/parsing-those-pesky-filtering-functions-in-odata.html)

**Expand**

Expand allows the expansion of a particular property path in OData, like so


*Expand the path Products/Suppliers*

    /Categories?$expand=Products/Suppliers

*Expand the path Suppliers AND expand the path Products*

    /Categories?$expand=Suppliers,Products
    
So this is quite easy, $expand expects a list of ResourcePath, separated by a comma.

I'll not show the tests for this, you can assume I have some though, with the appropriate data appearing on the model..

    ExpandOption = 
      seq("$expand=")
      listOf(`PropertyPath, ','):properties -> { name: "$expand", value: { properties: properties }}
    ,
    
Doesn't take a genius to work that one out does it :)

**Format**

This one is a doozy, the docs pretty much say it accepts

- application/atom+xml
- application/xml
- application/json
- Any other valid IANA content type

So what we're saying here is that we'll parse any content type, what I'll do is just parse the general pattern to make sure it doesn't contain garbage and leave it at that.

    FormatOption = 
      seq("$format=")
      ContentType:type -> { name: "$format", value: type }
    ,

    ContentType = 
      < letter+
        '/' 
        letter+
        (
          '+' letter+
        )?
      >

There are probably more rules than that but it's easily improved later

**Select**

Select tells us what is going to be brought back from a query, this can either be a property path, a collection of property paths or an asterisk.

An asterisk means bring back EVERYTHING. Nothing special.

    SelectOption =
      seq("$select=")
      (
        "*"                                 -> '*' 
      | listOf(`PropertyPath, ','):properties  -> { properties: properties }
      ):value -> { name: "$select", value: value }
    ,


**Highlighting a problem or three**

That's pretty much the entire spec sorted out, and we have a few tidy ups on our hand

- The model we're building isn't meaningful enough
- I've done some messy OMeta, it needs tidying
- I'm not handling primitive types properly (ResourceNames, ResourceComponents, Numbers etc)

I'll sort all these out in the next entry (I imagine that there will have been some comments made about these already in the future... in the past now, I wrote all this a month ago after all) and then we'll be finished and onto something new.


