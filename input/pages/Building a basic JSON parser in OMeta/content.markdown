I learn by doing, not by reading books, this is bit of a disability when first encountering something *completely* new because it's hard to start until you know something but learning something when you can't learn by reading is a real pain!

Nevertheless, I decided the best way to crack the nut of OMeta would be to write something to read JSON (it has already been done so I've got something to refer to if I get stuck), and I don't need to make it complete - just functional enough to say "okay, I can do something more real in OMeta now"

Oh, and because this is something new and different, my spike into it will involve feeling my way through the problem with tests. I can't do it all in-line hardcore TDD-style in the test functions and extract because of the OMeta compilation phase - but it should be good enough for fast feedback while learning.

So first off, I want to know how to match an empty object

    {}


That's my empty object there, the simplest possible blob of JSON I can come up with :-)

    describe("matching an empty object", function() {
      var input = "{}"
      var result = JsonReader.matchAll(input, "obj")
      assert.deepEqual(result, {})
    })

So how do I match it?

    ometa JsonReader {
      obj = "{}" -> {}
    }

How about matching a single numerical value?

    describe("matching a single numerical value", function() {
      var input = "400"
      var result = jsonreader.matchall(input, "num")
      assert.equal(result, 400)
    })

This won't work for long, but it's the easiest solution - match *anything* over and over again, then parse the result as an integer

    ometa JsonReader {
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

How about matching some text? we need to match quoted text as it can be both a value or a key.

    describe("Matching a single string value", function() {
      var input = '"hello"'
      var result = JsonReader.matchAll(input, "str")
      assert.equal(result, "hello")
    })

Opting for the simplest I can think of, *match anything and them remove the quotes.*

    ometa JsonReader {
      str = anything+:x -> x.join('').replace(/\"/g, ''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

This will come back to bite me I'm sure - how about matching a basic key value pair?

    describe("Matching a key value pair with a numerical value", function() {
      var input = '"foo":1337'
      var result = JsonReader.matchAll(input, "kvp")
      assert.deepEqual(result, [ "foo", 1337 ])
    })

Can we try this? *Match a string, then a colon, then a number*

    ometa JsonReader {
      kvp = str:k ":" num:v -> [ k, v ],
      str = anything+:x -> x.join('').replace(/\"/g, ''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

Well we can, but it won't pass because we're being lazy and matching "anything" for both strings and numbers. (We're being greedy and it'll try and match quotes and braces and then fail) 

Back to the drawing board, can we specify what we mean by an expected character in a string?

    describe("Matching the character 'a'", function() {
      var input = 'a'
      var result = JsonReader.matchAll(input, "char")
      assert.deepEqual('a', result)
    })

Well yes, there is only one! So I'll cheat and just match *anything* again :P
  
    ometa JsonReader {
      char = anything:x -> x,
      kvp = str:k ":" num:v -> [ k, v ],
      str = anything+:x -> x.join('').replace(/\"/g, ''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

How about not managing to map something that isn't a character? *anything* isn't going to work remember!

    describe("Character matching doesn't match quotes", function() {
      var input = '"'
        , thrown = null
      try {
        JsonReader.matchAll(input, "char")
      } catch(ex) {
        thrown = ex
      }
      assert.notEqual(thrown, null)
    })

Not too hard, just *check that we haven't got a double quote and then match anything else*:

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      kvp = str:k ":" num:v -> [ k, v ],
      str = anything+:x -> x.join('').replace(/\"/g, ''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

How about going back to that original test with the string matching?

    describe("Matching a single string value", function() {
      var input = '"hello"'
      var result = JsonReader.matchAll(input, "str")
      assert.equal(result, "hello")
    })

What happens if we come to a quote in the middle of the data?

    describe("Matching a string value with pre-mature quote", function() {
      var input = '"hel"lo"'
      var result = JsonReader.matchAll(input, "str")
      assert.equal(result, "hel")
    })

If we say that rather than matching "anything" for str, we want to match *only a collection of "char" with quotes around it* then..

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      kvp = str:k ":" num:v -> [ k, v ],
      str = '"' char+:x '"' -> x.join(''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

Now can we run that key-value one again?

    describe("Matching a key value pair with a numerical value", function() {
      var input = '"foo":1337'
      var result = JsonReader.matchAll(input, "kvp")
      assert.deepEqual(result, [ "foo", 1337 ])
    })

Huzzah. Functional.

How about a key-value with a string value?

    describe("Matching a key value pair with a string value", function() {
      var input = '"foo":"bar"'
      var result = JsonReader.matchAll(input, "kvp")
      assert.deepEqual(result, [ "foo", "bar" ])
    })

Well this is where the our parsing language shines, as we can specify *"any valid value", where "value -> num | str"*

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      value = (
                num
              | str
              ):x -> x,
      kvp = str:k ":" value:v -> [ k, v ],
      str = '"' char+:x '"' -> x.join(''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

Actually, this probably means that matching objects shouldn't be too hard.

    describe("Matching a key value pair with an object value", function() {
      var input = '"foo":{}'
      var result = JsonReader.matchAll(input, "kvp")
      assert.deepEqual(result, [ "foo", {}])
    }) 

Just add the obj to the list of possible values expected (*note it comes before number because number is still really greedy!*)

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      value = (
                str
              | obj
              | num
              ):x -> x,
      kvp = str:k ":" value:v -> [ k, v ],
      str = '"' char+:x '"' -> x.join(''),
      num = anything+:x -> parseInt(x.join(''), 10),
      obj = "{}" -> {}
    }

Now, this obj definition isn't actually correct - because objects contain one or more key value pairs.

    describe("Matching an object with a single key value pair", function() {
      var input = '{"foo":1337}'
      var result = JsonReader.matchAll(input, "obj")
      assert.deepEqual(result, { foo: 1337 })
    })

This won't actually work because as mentioned above, 'num' is still greedy and needs changing so it only matches numerical digits.

    describe("Matching a number with an early termination", function() {
      var input = '133"7'
      var result = JsonReader.matchAll(input, "num")
      assert.equal(result, 133)
    })

Turns out that there is a built-in called 'digit' to help us with this ( there is probably a built-in for strings too, but documentation for OMeta is not very good and I didn't see it)

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      value = (
                str
              | obj
              | num
              ):x -> x,
      kvp = str:k ":" value:v -> [ k, v ],
      str = '"' char+:x '"' -> x.join(''),
      num = <digit+>:x -> parseInt(x, 10),
      obj = "{}" -> {}
    }

Groovy, now perhaps we can try that test again:

    describe("Matching an object with a single key value pair", function() {
      var input = '{"foo":1337}'
      var result = JsonReader.matchAll(input, "obj")
      assert.deepEqual(result, { foo: 1337 })
    })

I decide to call an external function to help build up the object which has key/value pairs - that's the *makeObject(args):output* bit:

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      value = (
                str
              | obj
              | num
              ):x -> x,
      kvp = str:k ":" value:v -> [ k, v ],
      str = '"' char+:x '"' -> x.join(''),
      num = <digit+>:x -> parseInt(x, 10),
      obj = "{}" -> {}
          | "{" (
                  kvp:kv 
                  makeObject(kv):output
                ):output
            "}" -> output
    }

    JsonReader.makeObject = function(kv) {
      var obj = {}
      obj[kv[0]] = kv[1]
      return obj
    }

Finally, how about lists of arguments?

    describe("Matching an object with multiple key value pairs", function() {
      var input = '{"foo":43,"bar":343}'
      var result = JsonReader.matchAll(input, "obj")
      assert.deepEqual(result, { foo: 1337, bar: 343 })
    })

Turns out that there is another construct in OMeta for this scenario (The best way to find these constructs is to read the ometa-js source - sorry.) *listOf*

    ometa JsonReader {
      char =  (
                ~'"'
                anything
              ):x -> x,
      value = (
                str
              | obj
              | num
              ):x -> x,
      kvp = str:k ":" value:v -> [ k, v ],
      str = '"' char+:x '"' -> x.join(''),
      num = <digit+>:x -> parseInt(x, 10),
      obj = "{}" -> {}
          | "{" (
                  listOf(`kvp, ','):kvs
                  makeObject(kvs):output
                )
            "}" -> output
    }

    JsonReader.makeObject = function(kvs) {
      var obj = {}
      for(var i = 0 ; i < kvs.length; i++) {
        var kv = kvs[i]
        obj[kv[0]] = kv[1]
      }
      return obj
    }

**Summary**

It's pretty trivial to build up a parser for something if you built up a test suite as you go and have some way of

- Seeing examples for the syntax that OMeta supports
- Seeing examples for the built-ins inside OMeta

With this behind me, I can actually go and look at the source code of this company and see about doing something useful from this learning!

