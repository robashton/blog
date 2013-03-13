This week I'm in Athens, working for a company that is uses these things

- OMeta
- SVRB
- JS
- CoffeeScript
- NodeJS
- OData

My job in Athens is two fold::::

- Write some OMeta stuff somewhere (They have a specific project/task - don't worry)
- Look at the overal project and give my honest feedback

Well, giving honest feedback is something I can do - however my relationship with OMeta and SVRB is that I've never heard of them.

**It looks like I have some learning to do**

So what IS OMeta? It turns out that I do have a little experience in this area because OMeta is a expression parsing language, and like most people I've written a few parsers and compilers in my few years as a software developer.

OMeta is a bit different in that it had a specific goal - chiefly that of making it fast to prototype new languages/parsing constructs, and indeed it can be used to do pretty much the whole chain in the compilation process (parsing, intepreting and compilation).

*I'm not going to do a blog series on this, just wanted to throw up some stuff as I learned it :)*

**What I'm using**

I have [OMeta-JS](https://github.com/Page-/ometa-js), and I'm doing most of my playing in the web browser with [An OMeta parsing Ometa demo](https://github.com/Page-/ometa-js/tree/highlighting/examples/highlighting).

If I make some OMeta in the textbox and then go to the other textbox, I can copy and paste the JS into a repl and play around, it's not the most super effective way of working but I suspect this demo will be improved on to make it easier.

**So again, what is OMeta?**

I told you, it's a parsing language, check out the following:

    ometa MyParser {
      greeting = "Hello"
    }


If I compile this, I'll get the following:

    var MyParser = OMeta._extend({
      greeting: function() {
          var $elf = this, _fromIdx = this.input.idx;
          return this._applyWithArgs("token", "Hello");
      }
    });


Which I can use in some code

    MyParser.matchAll("Hello", "greeting")    : Success
    MyParser.matchAll("Goodbye", "greeting")  : Failure

What I can also do is transform these expressions into other expressions

    ometa MyParser {
      greeting = "Hello" -> "Howdy"
    }

    MyParser.matchAll("Hello", "greeting")    : "Howdy"

And I can also build up matches out of other matches

    ometa MyParser {
      greeting = "Hello",
      bob      = "Bob",
      sentence = greeting bob
    }

    MyParser.matchAll("Hello Bob", "sentence")  : Success
    MyParser.matchAll("Hello James", "sentence")  : Failure
    MyParser.matchAll("Bob", "bob")  : Success

And this means I can build up transformations from simple expressions:

    ometa MyParser {
      greeting = "Hello" -> "Howdy ",
      bob      = "Bob"   -> "Bobby"
      sentence = greeting:g bob:b -> (g + b)
    }

    MyParser.matchAll("Hello Bob", "sentence")  : "Howdy Bobby"
    MyParser.matchAll("Hello James", "sentence")  : Failure

Now obviously we don't use this language for parsing daft sentences like the above, what we do is use it to build up expectations around structures such as program code.

    ometa CParser {
      type    = identifier,
      argList = listOf(arg, ","),
      methodBody = "{" statementList "}"
      program = type "main("  argList ")" methodBody
    }

And so on...

Neat. I can see me using OMeta in the future rather than hand rolling parsers, my first effort on the other hand - a basic JSON to XML converter...

