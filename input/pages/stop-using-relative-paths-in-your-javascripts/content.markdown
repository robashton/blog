I saw a post to the NodeJS mailing list the other day which went along the lines of 

  <blockquote>
    I've created a simple prototype tool for re-factoring and re-organization of projects which heavily use require("./relativePath") .
  </blockquote>

Without wishing to put the chap off from releasing OSS efforts (because this is nearly *always* an excellent idea) I responded with a sentence explaining why I thought this kind of thing was a bad idea. 

I promised myself I'd write a blog entry with some loose thoughts in it too.

This is the classic example of a "tooling oriented fix" for a "problem of our own creation", after primarily making my living in the enterprise space where this sort of thing is rife I'm quite sensitive to such things when they arise when there is a better solution available.

*If using relative paths in your JS project is painful, stop using relative paths in your JS projects*

**Relative paths require cognitive effort**

When opening a project for the first time and encountering a large folder structure, I don't know where to begin. 

Even when I'm told where to go, I then have to trawl through the dependencies manually to find the code I want to change. When I want to write tests, it's hard to work out where the files are that I want/need to bring in. 

Compare this to a project which is comprised of modules, where each has a package.json which clearly describes where its git repo is, what its purpose is - and more importantly its dependencies. This is easy to understand and traverse and requires minimum new understanding.

**Relative paths lead to brittle coupling decisions**

If you're changing that code file, how do you know what is going to be impacted by that change? You don't - and this is why we get badly researched articles like [this one](http://techcrunch.com/2013/03/15/the-future-of-javascript/) written about how hard JS is to maintain.

Better to make these dependencies explicit, and version them seperately so that upgrading to new versions of these dependencies is a conscious decision. Better still, applying [a versioning strategy](http://semver.org/) so that breaking changes become obvious will make life much easier.

**Relative paths make tests harder to write**

    var sut = require('../src/company/lib/server/helpers/util.js')

Having a test suite that looks like the above over and over again is monstrous, you'll find yourself copying and pasting relative paths all over the suite and that'll in turn make you un-willing to re-factor or move things around for fear of breaking all the tests.

Compare this to instead a module-based approach where the tests we write for a module are simply covering that module. The effort required when decide to move things around is much smaller and we're not having to be dependenent on the organisation of files on your spinny disk or solid state storage.

**Relative paths are indicative of modules wanting to get out**

Take a look at your relative paths, look at commonly accessed files and consider that perhaps there is a module. Can you describe to me what that shared file does? What its purpose is? If so - you've passed the module test -get it in there.

If you can't tell me what that shared file does, then why does it exist? Is it just a "bag of stuff"? Don't create "bags of stuff", create "modules" with clearly defined purpose so the rest of us can have a clue of what is going on.

**So please, stop**

If you're not using some sort of package system to help you with your JavaScripts, then please start doing so. Preferably use NPM because it is one of the best designed package managers out there, but feel free to use Bower or something like that too, just stop presenting me with large codebases with piles of JS in them, it's costing you money to hire me and you don't want that money to be spent with me trying to work out how your folder structure works.




