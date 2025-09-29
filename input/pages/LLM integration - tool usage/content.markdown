So we've got some basic integration going and some loops set up and the LLM is able to service our requests.

- [Intro + Basics](/entries/an-adventure-in-llm+agent-development.html)
- [Designing my LLM Tools](/entries/llm-integration---designing-llm-tools.html)
- [The basic execution loop](/entries/llm-integration---a-basic-execution-loop.html)

I got here pretty quickly, and I could do things like ask Claude (not OpenAI or Gemini because they're not quite as eager/creative/whatever who cares), "Build a workflow that does X, Y, and Z, then outputs A, B and C" and it would sit there thinking and slowly build me a workflow but if it wasn't like watching paint dry then I don't know anything (disclaimer: I've never watched paint dry).

The defaults
--

I've asked Claude to do something, and it sets about using the tools to best understand *how* it can do that something. 

I know Claude doesn't think, it's just statistical number generation and all that, but essentially it thinks "Oh, X, Y and Z, there are some keywords here, I'll do"

- search_components: keywords related to X
*thinks*
- search_components: keywords related to Y
*thinks*
- search_components: keywords related to Z
*thinks*

- add_component: X
*thinks*
- add_component: Y
*thinks*
- add_component: Z
*thinks*

This works, it gets where it needs to be, but each of those thinking cycles is a 'turn' in LLM parlance where the LLM is having to generate output based on input so far. It's inefficient (even with cacheing, another topic).

There is no reason for it.

Batching
--

What is old is new is old. Batching operations is something as developers we're well versed in and it ends up being no different here except we need to persuade the random number generator to batch tool usage and it's not something we can force.

Instead of "Analyse the user's request, and using the tools at your disposal, edit the workflow to service their needs" (simplifying), what we actually need is

MANDATORY: You MUST work out all of the keywords for components before calling search_components a single time to find ALL components relevant to the user's request. Subsequent calls can be made if not all needs are satisfied in the initial request.

MANDATORY: You MUST plan in advance EVERY component you are going to add, and send all add_component tool requests in a single turn.

- search_components: keywords related to X, Y, and Z
- add_components: X, Y, Z

The speed up from this is unreal, something like 5x/10x.

It's important to note that the speed-up here is not the cost of calling the tools (which is neglible, at 2ms per tool), but the repeated load on the LLM to receive the results of a single tool call and generate the need for the next tool repeatedly.






