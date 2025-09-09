Continuing on

- [Intro + Basics](/entries/an-adventure-in-llm+agent-development.html)

A bit of chat then, what can go wrong?

To re-iterate, we've got

- A dynamic library of available components
- Well defined subscription model (what produces what media, and what accepts it)
- Fully defined schemas for all component configurations
- Validation that can run across a workflow and tell you exactly what is wrong (and sometimes how to fix it)

I've set up ambient LLM integration in [Norsk Studio](https://norsk.video/norsk-studio-live-media-workflow-server/), and any code on the server can easily start a session against whatever is currently configured.


The basics
--

- Chat lives in the web browser
- The *model* effectively lives in the web browser too (the thing being edited, and the library it is using to do it)
- LLM integration lives on the server (it has to use tokens to talk to the APIs, you don't want them on the client)

The obvious thing to do is

- Set up a websocket which sets up a LLM session for this browser instance, it can tear it down when the WS goes away
- Store chat history in local storage so a refresh of the page doesn't lose context
- Create and supply tools to the LLM session that can query the workflow/library in the browser

A naive implementation
--

- *get_current_workflow*: Serialise the document and send it back
- *list_available_components*: Serialise the library and send it back
- *add_component*: Take an id/type/config and add a component to the workspace
- *remove_component*: Self explanatory
- *subscribe*: Take a source id and destination id, and configuration for a subscription and set up/replace a sub

Yeah... no. 

Context bloat is a real thing and presenting all of the information in the app to the LLM is one of the first mistakes I assume everybody makes because I certainly did on my first pass.

- the current workflow contains configuration for every component (that's a lot of data)
- components each have schemas attached (that's even more data)
- add_component.. takes config, meaning the LLM needs to generate the config
- subscribe - far too much context required potentially

With a library of potentially a hundred or so components, and documents containing upwards of a dozen or two components, we'd quickly exceed token limits with completely irrelevant information.
Worst case, the LLM API just throws rate limit errors at you, best case, the LLM does completely wacko operations because you've filled its context window with garbage.

Surgical precision
--

- *get_workflow_components*: Return just the ids, the type, and a brief summary of what the component does from the library 
- *get_workflow_graph*: Return the ids, subscriptions between them and basic configuration of each sub
- *search_components*: Given keywords, return just the types and summaries of matching components 
- *add_component*: Given a display name, location and type, add the component with default configuration, return any validation issues
- *get_component_schema*: Given a specific component type, return the schema for the configuration of that component
- *update_component_config*: Given a component id, and a *partial* config, update just the provided rows of the component, return any validation issues
- *add_subscription*: Given two component ids (target/source), create a default widest-possible subscription between them, return validation issues
- *get_subscription_options*: Given two component ids, detail what is actually possible (the UI already does this with dropdowns and different UIs depending on what is selected)
- *update_subscription*: Supplying a subset of the options and the two ids, update the subscription (return validation issues)

This is a key learning for anybody implementing LLM integration with custom tools, it's not too much of a shift.
If you have a good user experience (defaults, minimal interaction to perform happy path by default, etc), then that same experience can easily be provided to the LLM by tools and context bloat can be avoided.

- *Do this thing*: Do the thing with minimal information, set up defaults for everything, tell the LLM/user about anything that might be wrong
- *get some information*: What is the least amount of information that the user/LLM needs to perform a task?

In the Norsk Studio UI

- A user simply drags on components and for the most part can just hit 'save' immediately because defaults are always provided
- A user simply drags connections between components and the UI automatically creates a subscription that takes the most data allowed
- Validation is ran constantly, with instructions on how to fix things, common examples
  - Clashing ports on inputs/oututs
  - Clashing source names on inputs/outputs
  - Subscribing to video multiple times on outputs from different upstream sources when trying to subscribe to audio

There is no reason the LLM should have a different experience to the user, in fact the implementation of these tools in this case ends up using most of the same code. 
