I'm going off what is specified in both the OData spec and the OData URI conventions document.

This is a bit annoying, because it seems like URI conventions are just that, conventions - and people are free to do what they want (I haven't looked at the metadata spec yet so I'm not sure how discoverable this customisability is, I guess I'll get there during my time on this task)

What I think I can start with, is parsing the following basics

- The service root itself (http://example.com/service/odata.svc for example)
- An entity at this root ( /model )
- An entity with a key ( /model(1) )


**How to develop this**

This is yet another task I'll probably write tests for as I go so I can document how far I've gotten and have a safety net as I no doubt make lots of mistakes.

I'll copy and paste code from the old OData parser as I need it and as I write the tests to support it, in this even the legacy code will end up with coverage.


