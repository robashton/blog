Not gonna make a big deal or fanfare about this, but I'm going to jot down my thoughts as the company I work for starts moving into using Purescript for more of our day to day work instead of Erlang.

### WHAT?

Indeed, our codebases are starting to get pretty unwieldy with their sizes and as much as we keep a tight ship with Dialyzer and such, writing all of our code in a language like Erlang is just asking for easily avoidable bugs to enter the room.

So, what is there to look at in this space?

- The project has a [Github Org](https://github.com/purerl)
- There are a [pile of packages](https://github.com/purerl/package-sets) for getting started with
- Purescript itself has a ton of users and [documentation](http://www.purescript.org/) so we're not working in unfamiliar lands
- There has been some [experimentation with OTP](https://github.com/purerl/purerl_otp_sandbox) already, with a few different strategies

Our strategy going forward is to take a greenfield app, and start to write it in Purescript, accepting that there will be marginal gains if any when writing the periphery code in Purescript as opposed to the meaty stuff that already exists. (Not to mention all the interop required). This will at least give us in-roads into the language and platform, and highlight any core-work required in the Purerl space to support our needs as we go forwards.

The first targets therefore for our attack, and I'll do these posts fairly bite-size over the coming weeks to keep things easily digestible..

- Writing a gen server in Purescript 
- Persisting data from Purescript
- Serving a JS frontend (presumably interop with Cowboy)

I imagine there will be a lot of stumbling blocks and "This isn't quite right yet" comments along the way, nonetheless, it's going to be fun to write and hopefully to read as well - enjoy.





