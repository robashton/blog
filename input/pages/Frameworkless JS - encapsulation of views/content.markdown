So we have a mess. What to do so we can end up with [not a mess.](https://github.com/robashton/look-ma-no-frameworks/)?

- [Look ma no Frameworks](/entries/look-ma,-no-frameworks.html)
- [An example](/entries/frameworkless-js---an-example.html)
- [Getting started](/entries/starting-the-frameworkless-js-project.html)
- [Templating stuff](/entries/frameworkless-js---rendering-templates.html)
- [Demanding Knockout](/entries/frameworkless-js---but-we-*really*-want-to-use-knockout-for-that-bit.html)

*The state of play*

- We have a single file, *app.js*, with a pile of code in it.
- We have a single template, *customers.html* with some mustache and bindings in it
- Everytime we change an option, the entire view gets re-rendered
- Everytime we change an option, we have to re-hook all the events

Not so brilliant, something we can all probably agree on.

*What to do about it*

Encapsulation. 
