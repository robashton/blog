Continuing on

- [Intro + Basics](/entries/an-adventure-in-llm+agent-development.html)
- [Designing my LLM Tools](/entries/llm-integration---designing-llm-tools.html)

Now I've got some tools, I need to design a basic interaction loop between the user and the LLM.

What does this look like?
--

- User enters their request
- We feed request to LLM along with a systemPrompt
- ???
- Profit

It turns out that ??? is the magic sauce, and again - at time of writing there are various libraries about to try and do this for you, and by the time of reading there will be one which is a winner, at time of writing I'm donning a commando hat and going on alone. I need to support multiple LLMs and their nuances and I need this solution to work specifically in our domain.

What needs to happen here? 

The LLM has a *goal*, which is to service the user request. Our system prompt does its best to describe to the LLM how it might achieve any goal that the user might ask for as well as constrain (or attempt to constrain) those requests.

In order to achieve that goal, we need to

- Send the message to the LLM
- Allow the LLM to execute tools
- Send the tool usage to the LLM
- Allow the LLM to execute more tools 
- Extract the result

A small problem with this, is that the three providers I'm integrating with this have very different behaviours when the LLM gets 'confused', or the LLM gets 'stuck'. 

- Empty responses when the LLM decides it is 'done' (it is not done)
- Infinite tool loops when the LLM gets 'confused' and tries to be helpful by trying over and over (say goodbye to your credit folks) 
- Error responses from the LLM because of similar reasons
- The LLM 'decides' to respond in a way which you can't parse
- The LLM 'forgets' about native tools, and includes tool calls in the body of the response

This is not an exhaustive list, just various things I've encountered when integrating against the 'big three'.


A basic loop with constraints
--

Essentially what we end up needing to do is 

- Write a systemPrompt that specifies the return format (usually JSON for me, I like to use both typescript 'type' annotations *And* examples of what this looks like. Sprinkle in the words MUST and MANDATORY a few times.
- Send the request to the LLM alongside the available tools
- Execute all the tool requests (if any)
- Send all the tool responses to the LLM
- Repeat until tool_count = 0

- Check the response against our desired prompt response format (check every field, run it against a schema if you can), if the LLM responded with something daft, then add a new message to the conversation history 'You did bad, you are bad, you should feel bad because it is bad, now do it good or I will throw you in the sea'

If at any point any of the error conditions mentioned above crops up, we send a new message to the LLM re-iterating what we want, and repeat until success.

Now, that's the LLM equivalent of writing for(;;), so in reality what I do is

- Count the tokens being used
- Count the loops for the current request
- Count how many tools have been used 

And set up some arbitrary numbers (depending on the use case) for bailing out and asking the user "*Are you sure you want to spend all of your AI credit and burn down a rainforest for just this request??*"


An easy mistake to make
--

If you contradict yourself at any point in any of your prompts, the LLM can end up just returning an empty response (A classic example is that I told it it MUST respond in THIS JSON format, and then in further messaging, gave it a slightly different format with another MUST by mistake, no bueno).

If you think you have a termination condition but don't realise you've reached it because you're checking for the wrong value, the LLM will really do its best to service your request even though it already did. It will increasingly grow frantic and go off the rails doing everything under the sun to make you happy before turning an empty/weird result and ceasing responding to further requests. Ask me how I know.


In general, I've realised you really need to be specific about the output format, not give them too many choices and only have a single termination/response expected. 
If you reach tool_count=0, you NEED another message to be sent as a new request, because the LLM is done doing what you asked it to be done.

Once more with feeling
--

This is going to end up being abstracted sensibly over the next couple of years assuming LLM mania survives that long and we won't have to think about it much longer. Some things exist for this now, they're just nascent and too much is changing to throw our lot in with anything in particular.
