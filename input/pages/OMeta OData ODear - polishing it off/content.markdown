Now I've gotten most of this work done, I left a few pieces of work outstanding and after review, I can make some of the OMeta cleaner and nicer. I also need to be a bit better about interpreting the various primitives in OData.

I've also got a more [qualified person](https://github.com/Page-/) to review my OMeta as I go along and give me feedback on my work, so this is where I integrate a lot of that.

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
- [The rest of OData](/entries/the-final-odata-query-bits,-yes-were-nearly-there.html)


OMeta
----------

**Un-needed semantic actions**

In a few places in my OMeta I have expressions that look like this:

    (
      seq("allpages") -> "allpages"
    | seq("none") -> "none"
    )


This is quite wasteful and can be written much more simply as 

    (
      seq("allpages") 
    | seq("none")
    )

This is because by default the last expression will be returned anyway

The same goes for this (ignoring that the Text primitive still needs some work)

    Text =
      <	(	~'\''
          (	'\\' anything
          |	letter
          )
        )*
      >:text
      -> text

Is much tidier if we get rid of the un-need semantic action because text will be returned anyway

    Text =
      <	(	~'\''
          (	'\\' anything
          |	letter
          )
        )*
      >:text

And

    SelectOption =
      seq("$select=")
      (
        "*"                                 -> '*' 
      | listOf(`PropertyPath, ','):properties  -> { properties: properties }
      ):value -> { name: "$select", value: value }

Is much better off without too

    SelectOption =
      seq("$select=")
      (
        "*"
      | listOf(`PropertyPath, ','):properties  -> { properties: properties }
      ):value -> { name: "$select", value: value }


**Stop repeating yourself!**

OMeta uses memoisation so this isn't a big deal, but repeating yourself is annoying anyway and we can be far more expressive if we think about commonly matches constructs in our code.

    (
      seq(" asc") -> "asc"
    | seq(" desc") -> "desc"
    )?:order

Here I'm looking for a sequence of characters with 'space' 'asc' 'space' and this would be far better written as

    spaces
    (
      seq("asc")
    | seq("desc")
    | -> 'desc'
    )?:order

And to boot I've added in the default, which is 'desc'


And how about this little one?

    listOf(`PropertyPath, ',')

I use that in quite a few places in the code, better split it off into its own rule

    PropertyPathList = 
      listOf(`PropertyPath, ',')

And use that around the place instead!

**Un-needed brackets**

      PathSegment:model 
      (
        '?'
        ( listOf(`QueryOption, '&'):options
        )
      )?

I do this in a few places, and while it causes no harm, OMeta is hard enough to read to the un-initiated without throwing brackets in all of the place

      PathSegment:model 
      (
        '?'
        listOf(`QueryOption, '&'):options
      )?

Not rocket science!

**Custom matching methods**

Admittedly I hacked this together in a rum-bar at 10pm, but I have this floating around in the code for dealing with [filter methods](/entries/parsing-those-pesky-filtering-functions-in-odata.html)

    SingleArgMethodCall :name =
      seq(name) 
      '(' 
      spaces 
      FilterByExpression:one 
      spaces
      ')' -> { args: [ one ], method: name }
    ,

    TwoArgMethodCall :name = 
      seq(name)
      '(' 
      spaces 
      FilterByExpression:one 
      spaces 
      ',' 
      spaces 
      FilterByExpression:two 
      spaces 
      ')' -> { args: [ one, two ], method: name }
    , 

    ThreeArgMethodCall :name = 
      seq(name)
      '(' 
      spaces 
      FilterByExpression:one 
      spaces 
      ',' 
      spaces 
      FilterByExpression:two 
      spaces 
      ','
      spaces
      FilterByExpression:three 
      spaces 
      ')' -> { args: [ one, two, three ], method: name }
    , 


This is still better than doing the above individually for *every single supported method*, but it would be nice if we could do

    MethodCall(name, arity)

Instead of having three different expressions in a non-expandable manner

Well, first off - the beginning of this will looke lik

    MethodCall :name :arity =
      seq(name)
      '('
        numberOf(`FilterByExpression, arity):args
      ')' -> { args: args, method: name }
    ,

Except there is no function called numberOf.

Extending our OMeta parser with custom functions is really easy though.

    ODataParser.numberOf = function(rule, count, seperator) {
      var ret = [];
      for(var i = 1; i < count; i++) {
        ret.push(this._apply(rule));
        this._apply("spaces");
        this._applyWithArgs('exactly', seperator)
        this._apply("spaces");
      }
      ret.push(this._apply(rule));
      return ret;
    }

These '\_apply' methods are simply what the rules are converted into when the OMeta is transpiled into JS, and we're skipping that bit and patching our parser with the raw JS. Simples!

OData, ODear
---------

Well, I skipped a few steps here certainly - especially with regard to the following rules:

    Number = <digit+>:d -> parseInt(d, 10),
      Number = <digit+>:d -> parseInt(d, 10),
      Text =
        <	(	~'\''
            (	'\\' anything
            |	letter
            )
          )*
        >:text
      ,

      QuotedText =
        '\''
        Text:t 
        '\'' -> t
      ,
    Text =
      <	(	~'\''
          (	'\\' anything
          |	letter
          )
        )*
      >:text
    ,


and
    
    ResourcePart =
      <	(	letter
        |	'_'
        )+
      >:resourcePart
      -> resourcePart.replace(new RegExp('_', 'g'), ' ')
    ,

    ResourceName =
      <	ResourcePart
        (	'-'
          ResourcePart
        )*
      >

These are our primitives in the OData space, everything else is built up off of them and I've been a bad person and not done them properly. (If anybody has bothered reading all the way up to here, you probably thought this at the time and maybe even commented about it)

**Text and QuotedText**

Just what *is* Text? 

*As part of the query string*

    OperationParam = 
      Text:name '=' Text:value -> { name: name, value: value }

*And inside quotes as a string literal*

    QuotedText =
      '\''
      Text:t 
      '\'' -> t
    ,

*What are the rules?*

Well, if it's part of the query string, let's say it's the name of a parameter, it can be anything at all (except reserved characters from the Uri - these should be encoded). To solve this we need to read the [RFC](http://tools.ietf.org/html/rfc3986)

     reserved    = gen-delims / sub-delims
     gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
     sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
                             / "*" / "+" / "," / ";" / "="

We should recognise most of these, although some of these are explicitly allowed in some uri schemes and according to this RFC that's okay. 

Still, an implementation of this can look something like:

    ReservedUriComponent  =
      GenDelim
    | SubDelim
    ,

    GenDelim = 
      ":" | "/" | "?" | "#" | "[" | "]" | "@"
    ,
    
    SubDelim = 
      "!" | '$' | '*' | "'" | "&" | "(" | ")" | "+" | "," | ";" | "="
    ,

    Text =
      <
        ~ReservedUriComponent*
      >:text
    ,

What I'll do, is explicitly deny all of these characters except in cases where I explicitly allow them (for example, the dollar symbol is allowed in built-in query params, brackets are allowed in expressions, quotes are allowed to denote string literals, etc)

I can use this rule safely for quoted text as that rule explicitly allows quoted text:
    
    QuotedText =
      '\''
      Text:t 
      '\'' -> t
    
 **Resource paths**

 Same thing now goes here, and I can say that each part of a resource path is a UriComponent, explicitly disallowing spaces, separated by a '/', so
 
    ResourceName =
      <(	
        ~(ReservedUriComponent | ' ')
        anything
      )+
      >:resourceName

Much happier about all of this.


**Decoding as we go**

If somebody does give us some text that looks like this

    foo='hello%20world'

It would be nice if it was decoded for output

    Text =
      <
       (~ReservedUriComponent
       anything)*
      >:text -> decodeURIComponent(text)
    ,

We can indeed call arbitrary JS methods in our semantic output, good for us.

We'll do the same for resource names too

    ResourceName =
      <(	
        ~(ReservedUriComponent | ' ')
        anything
      )+
      >:resourceName -> decodeURIComponent(resourceName)

I think if I was to go and do some of this again, I'd have been explicit about Uri conformance from the start, but it hasn't caused too much damage so we're okay.


**Supporting further primitives**

At the moment we can parse integers with

    Number = <digit+>:d -> parseInt(d, 10)

But this is only half the story, we actually need to explicitly support decimals too

    Number = Decimal | Integer
    ,

    Decimal = 
      <
        digit+
        '.'
        digit+
      >:d     -> new Number(d)
    , 

    Integer = <digit+>:d -> parseInt(d, 10)
    ,

**Semantic output**

I'm now much happier that we have our bases covered with the types that we support, and that I'm not doing anything nefarious with OMeta, that leaves me with a final tidy-up task.

In some cases, we're outputting to an array that states 'this is what you have, so now you know how to interpret it', this is quite a standard way of doing things in OMeta and particularly in the expression parsing space.

By outputting to an array in this manner, it becomes very easy to write a further OMeta processing step to convert the output of the OMeta parsing step into another format (compilation).

This is useful for say, generating SQL based on the model that these chaps have defined in SBVR.

Let's look at a tangible example where I've gotten this weird:

    SelectOption =
      seq("$select=")
      (
        "*"                                 -> '*' 
      | PropertyPathList:properties  -> { properties: properties }
      ):value -> { name: "$select", value: value }
    ,

In one case, our semantic action is to return a string containing a single character \*, and other case I return an object literal with a list of properties in it.

I kinda want to go through and sort this out, but without using it in anger (say, for generating SQL), it's hard to say what a useful model will look like.

I've also made some mistakes in that I didn't refactor my tests as I went to eliminate duplication, so they're a bit coupled to the structure of the model.

I've decided that as I only have a day left at the client, that the best thing I can do at this point is raise my hands in the air and point out very publicly that:

- The tests are brittle *because* and this is how you'd improve them
- The model probably isn't that easy to consume, and will need changing, which will mean the tests need changing, *sorry*

With this said and done, I've decided the final bit of work I can do is to run some fuzzy testing against the parser and start trying to make it *really* complete, as this is a harder problem. (and maybe I can refactor the tests as I do this, so I only leave one problem..):w
o

**Fuzzy testing**

So I found a great tool (or at least something that sounds like a great tool), which [generates output based on an ABNF](http://www.quut.com/abnfgen/), and OData conveniently has an [ABNF specified for it](http://www.odata.org/media/30002/OData%20ABNF.html) so let's go!

What I'll do to get started is download and compile the abnfgen package, and run a single test case to make sure that this crazy idea is going to work, then I'll automate it and tell it to dump failed cases to a list so I can re-run them and work out why they've failed.

*edit*

Scratch that, the ABNF is incomplete and buggy and crap, what is the actual point - this concludes the end of my series.





