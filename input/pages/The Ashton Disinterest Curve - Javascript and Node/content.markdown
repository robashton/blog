So far on the "making a point that I'm not all bitterness and hatred" over things tried in the past, I have managed to pretty much write the same [negative diatribe about .NET](/entries/the-ashton-disinterest-curve---c.html) that everybody else seems to write after an age of bashing their head against the wall. (Note to naysayers: Maybe there is something to this after all, and no, the decision to OSS a bunch of .NET changes very little about the root problems found in most shops that choose that technology)

I digress
==

Aaaanyway - when the crippling pain in my wrists got too bad, the first place I sought refuge was a combination of Linux + Vim + JavaScript.

I had always used JS and I had written some pretty cool things in it; It seemed like a natural progression if I wanted to carry on being paid decent money for developing software without having to immediately just drop everything and develop a new career from scratch. Sadly the need to integrate with the Visual Studio users (JS in project files, seriously piss off) eventually put an end to that attempt and I ended up doing node.js at actual node.js based development shops and accidentally ditching Windows forever.

I got a *lot* done in JS (both on client and server) and for a while really enjoyed writing software in it. A large part of this was because I was used to the type systems in C# and Java style languages and being able to develop software without needless layers of interfaces impeding my progress made feel very warm and fuzzy.

Obviously this came with the price of requiring many more tests, develop everything in self contained modules (not necessarily a bad thing) and dealing with some wildly varying opinions on what constituted "readable JS".

The saving grace of this language was that it was still pretty untamed and the language had very few features to abuse; maintaining explicitness at the expense of terseness meant that you could fling together some relatively comprehensible systems rather quickly without too much forethought or trouble. Obviously my feelings on steps to "improve" JS are well known, Typescript, Dart and frameworks like Angular trying to treat JS as an actually sensible platform were akin to biblical efforts to build houses on sand or whatever parable you prefer for that purpose here.

Where I now stand
==

Since starting to learn languages like Haskell and realising that type systems don't have to be burdemsone, since developing with Erlang/OTP and seeing first-hand what a sensible networking platform should look like, since learning Clojure and realising terseness doesn't have to come at the expense of understandability - JS has lost a lot of that initial appeal. Since acknowledging the value of "hammock time" and how certain languages can encourage the use of up-front reasoning I can see that the "*just ship some code*" mentality  found in the JS world is probably harmful to our long term survival as a professional industry.

It is a language that as an profession we should be pretty ashamed for letting become so popular; it is for cowboy developers writing cowboy systems and anybody setting out to build new applications on top of this (with the intention of longevity) should strongly reconsider. I think that the motivation of using JS to teach programming is understandable because of how accessible it can be to new people but it is also a mistake because it teaches the wrong message about our industry - it is the antithesis to professionalism. If we were ever in a "race to the bottom" then JS can be considered to have won; it has lowered the barrier to entry so far that we'll be dealing with the consequences of this mistake for decades to come.

For low value UIs, for hackdays, for gamejams, for rapidly building that proof of concept, for that minimal viable product I think that both JS and Node probably have their place. JS is an incredibly fun language to get shit done in providing that you don't plan on being responsible for that code two <s>years</s> weeks down the line. Every Ludum Dare that I write games for I pick up JS, put on a Stetson and arm myself with a whip and yell YEEHHAAWWW because I'm a cowboy and I like to ride my horse without a saddle.

I *love* JS for this - despite its contradictions (and mine in this post), I can't think of any other language or platform where I can simply arm myself with a text editor and see results on my screen immediately in the form of shiny graphics and interactivity. While it is true that more professional languages would aid me in writing something that would be more formal and surviveable - just like writing tests, in the short term it would slow you down and diminish that immediate fun factor.

When we're are only dealing with the short term or we're happy that what we are writing is disposable (or just incredibly dull) then JS is a great language because it doesn't have any of the safeguards that any of the more professional languages or platforms do and as we all know - safeguards are for wusses.


Position on the curve: *Shrodingeresque - until directly observed you won't know if I love it or hate it.*
