Previous entries in this series: 

- [Purerl Updates - Processes and Self](/entries/purerl-updates---processes-and-self.html)
- [Purerl Updates - Subscriptions](/entries/purerl-updates---subscriptions.html)
- [Purerl Updates - Untagged Unions](/entries/purerl-updates---untagged-unions.html)
- [Purerl Updates - Maxing out Pinto OTP](/entries/purerl-updates---maxing-out-pinto-otp.html)
- [Purerl Updates - GenStatem](/entries/purerl-updates---genstatem.htm)

One of the big advantages of using an existing language like Purescript when building out typed support for Erlang, is the presence of existing tooling and libraries for the language (such as language servers, package management, syntax highlighting, etc - as well as libraries for all the normal maths, string manipulation, date times, testing and all that).

That's not to say you get those things for free however, in a perfect world we could add libraries to our package set directly from the original repos - the litmus test for whether this is possible is "Is there any Javascript in the original repo?". If there isn't then the library will work out of the box but if it has other dependencies that aren't yet in the package set then they will need adding to the package set too.

On top of that, one of the things we 'lack' in Purerl is the Aff monad. We (as far as we can tell so far) don't need it because we're just not writing code that way - or rather with the presence of various means of performing async actions in Erlang it simply isn't necessary. This does mean that any library that brings in Aff (such as some of the popular testing frameworks) will need porting to use sometihng else unless somebody feels like porting [Aff](https://github.com/purescript-contrib/purescript-aff/blob/main/src/Effect/Aff.js#L152)? No I thought not.

Consider the library [purescript-parsing](https://github.com/purescript-contrib/purescript-parsing) which was needed recently to parse [RFC8216](https://datatracker.ietf.org/doc/html/rfc8216).

On the face of it, there is nothing but Purescript in [the repo](https://github.com/purescript-contrib/purescript-parsing/tree/main/src/Text/Parsing/Parser) and the tests are incredibly simple, however it has a dependency on [purescript-unicode](https://github.com/purescript-contrib/purescript-unicode) which whilst being entirely Purescript also, not only has a dependency on [purescript-quickcheck](https://github.com/id3as/purescript-quickcheck) which absolutely does have some Erlang in it, but also generates Erlang that breaks the Erlang compiler because of 'complexity'. (hard coded lists of unicode characters meant nested function calls all the way down).

Quickcheck needs the JS re-writing as Erlang, and then if you want to build unicode/parser independently and run *their* tests then they need their packages updating so they can pull the fork of quickcheck. Thankfully these libraries all (sensibly) have their tests written as a *main :: Effect Unit* rather than using a fancy testing library so the changes are very fast and easy to implement. As a result of running the test suite of course, some missing features were found in [purescript-strings](https://github.com/id3as/purescript-strings) for which, thankfully built-in functions exist in Erlang so no hard work was required.

We also needed [purescript-datetime](https://github.com/purescript/purescript-datetime) which has a lot of the underlying implementation built off of Javascript's date system and needed rewriting to use Erlangs. Since then this has become embedded in a lot of other packages because we consider things like Seconds and Milliseconds to be important primitives worth representing as something other than *Int* and defining our own newtypes everywhere doesn't make a lot of sense. If you're going to be using Datetime heavily then you probably also want to format dates and times so that means bringing in [purescript-formatters](https://github.com/purescript-contrib/purescript-formatters) which has a dependency on parsing (cool, already ported!), but tests written against *Aff* for no good reason (That's fine, replaced them with good ol' [purescript-erl-test-eunit](https://github.com/id3as/purescript-erl-test-eunit).

I guess what I'm trying to say is that there are yaks all the way down, and that all of these libraries kindly written by other people don't come for *free*. On the bright side because we've been actively writing Purescript for three years we've brought across a lot of them ourselves already. The complete list of packages ported to the id3as organisation at this time of writing is:

- [purescript-these](https://github.com/id3as/purescript-these)
- [purescript-parsing](https://github.com/id3as/purescript-parsing)
- [purescript-pathy](https://github.com/id3as/purescript-pathy)
- [purescript-formatters](https://github.com/id3as/purescript-formatters)
- [purescript-unicode](https://github.com/id3as/purescript-unicode)
- [purescript-datetime](https://github.com/id3as/purescript-datetime)
- [purescript-quickcheck](https://github.com/id3as/purescript-quickcheck)
- [purescript-uri](https://github.com/id3as/purescript-uri)
- [purescript-sequences](https://github.com/id3as/purescript-sequences)
- [purescript-dagre](https://github.com/id3as/purescript-dagre)
- [purescript-longs](https://github.com/id3as/purescript-longs)

These will end up in the default package set over time so they can be referred to without needing to know where they are - but if you're venturing down the route of using Purescript and needing a package that doesn't exist, then check the [Purerl org](https://github.com/purerl) and the [Id3as](https://github.com/id3as) org to see if somebody hasn't already done the leg work!

