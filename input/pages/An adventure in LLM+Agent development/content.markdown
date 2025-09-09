I'm an AI sceptic, but it doesn't mean I'm not going to try out this stuff in our products if there is the chance that I'm wrong and these things can be persuaded to do useful things for our customers. 
I have found some value in using Claude to generate 'obvious' code when constrained to a very detailed plan (that Claude has also written), and its clear that when the output is constrained that correct answers can be arrived at. 

Experiments then, but first I'll just do a quick recap on what the product *is* because it's not something I ever really talk about outside of .. well work.

[Norsk](https://norsk.video/)
==

Make Live Easy and all that. We have a pretty great programmable media server with an [SDK](https://docs.norsk.video/media-sdk/1.0.400/norsk-sdk.html) that lets you create media nodes that subscribe to each other across a graph and manipulate that media in all sorts of interesting ways complete with hardware integration and all the stuff one expects from one of these things.

[Norsk Studio](https://norsk.video/norsk-studio-live-media-workflow-server/)
==

<img src="/img/studio.png" width="400px" />

My baby, originally created in a weekend with a goal of "Windows forms for media workflows", it's come on a long way since then but essentially it's a drag and drop media workflow builder, where each component automatically presents

- swagger/http control surface
- UIs around that control surface
- metrics/alerts/etc

I guess an interesting thing about this whole setup is that these components are dynamically loaded into the workspace, a workflow will only contain a subset of these (generating a unique set of HTTP APIs for that workflow), and all of this comes with schemas for

- State
- Config
- Commands
- Events
- HTTP 


Schemas, well given how prone to hallucinating and generating nonsense LLMs are that is something that could come in handy.

The obvious integrations
==

*Workflow Chat Assistant*: 

It's a bit dull on the surface, stick a chat interface into the main design UI and give it the ability to query the current workflow, add/remove components/subscriptions edit config, run the workflow, etc.
Building a workflow in Norsk Studio is very easy, so I don't know how much value there is in this, however perhaps there is an opportunity for discovery if you're not sure how to do something in Studio, and perhaps a chat interface offers an interesting accessibility option for those unable to use a graphical drag and drop interface? I don't know.

We have 

- schemas for configurations
- extensive validation for subscriptions
- extensive validation *for* that configuration 

It's quite possible that this will be an easy integration for an LLM.

*Dashboard generation*: 
 
Related to the Chat assistant (and possibly integrated into it), given a workflow that the user has built, it should be easily possible to generate a UI specifically for this workflow

- Clearly defined HTTP APIs 
- Clearly defined commands
- Clearly defined websocket events for live updates
- Clearly defined metrics

*AI Operators (agents)*

Possibly really interesting if I can get it right, can we embed a decision making agent into a workflow and have it operate an event automatically?

We have

- As above, well defined control surfaces for each component
- well defined events being raised from components

It's entirely possible for a component to pull data from other components (or subscribe to events) and use them to decision on.

One of my other colleagues is currently integrating more focused models upstream to do object detection and raise events related to the actual media
Another of of my colleagues has recently integrated CMSD into Norsk to flow quality metrics through with the media, again decisioning on this could be interesting

*Targeted advice*

Got validation errors? Magic 'fix this' button? This could be really easy given the above.

Step one, supporting all of this
==

- I want to use Anthropic, OpenAI, and Gemini
- I want multiple aspects of Norsk Studio to be able to integrate/use these LLMs

This calls for a common LLM abstraction. I shopped around and found a few of them but the scene is fast moving and this represents quite the potential pain point. 

No doubt in a few years we'll look back on this wild west era and either be doing something completely differently, or one of these common abstractions will have won and we'll switch over to it, but for now I'll just write my own because I want MCP intgration and custom tools and boy is that a moving target at the moment.

Essentially a chat with an LLM in this world involves

- Sending a 'system prompt'
- Sending the 'history' of the chat *to* the LLM
- Sending a list of the tools that the LLM can invoke alongside that

The history contains (effectively)

- Interleaved messages from the user/LLM
- Tool requests from the LLM
- Tool results given back to the LLM

This history being sent is what represents our 'context', which is the thing that the LLMs have limits on. The result of sending this to the LLM (as far as I understand it) is just a statistical stream of text deemed most likely to be the response.  
There is no innate understanding, no actual 'thinking' - this is one of the reasons why LLMs are kinda shit and by all accounts, a dead end for AGI but I digress, it is something to be managed.
Effectively the actual interface with the LLM is stateless and we are in charge of looking after the state.

So a starter for ten really is

```typescript
type ChatHistoryItem = {
  role: "user" | "assistant",
  content: string,
  toolCalls: ToolCall[],
  toolResults: ToolResults[]
}

type ChatOptions = {
  systemPrompt: string,
  tools: LLmTool[]
  }

interface LlmSession {
  chat(history: ChatHistoryItem[], options: ChatOptions);
}

```

Next up, I'll start to go through what it took to get a decent chat experience in Studio.



