So yes, I just wrote a parser for [OData](http://www.odata.org/) queries using [OMeta](http://tinlizzie.org/ometa/), and I've already had a few people ask me why, or query my thoughts about OData itself.
 
First off, the company I was doing the work for have a valid use case for OData, that is they're generating a model from a descriptive language and then trying to expose this model over a common protocol (in this case OData).

- In some cases, this will make sense.

**In some cases, exposing your entire database to the client will make sense**

Are you creating an application with little workflow, editing of raw data (and basic validation over that data)? Are you building a prototype? Are you 
