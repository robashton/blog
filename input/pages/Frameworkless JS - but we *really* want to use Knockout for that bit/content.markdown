
Continuing then with the ["look no ma no frameworks"](https://github.com/robashton/look-ma-no-frameworks/) series...

- [Look ma no Frameworks](/entries/look-ma,-no-frameworks.html)
- [An example](/entries/frameworkless-js---an-example.html)
- [Getting started](/entries/starting-the-frameworkless-js-project.html)
- [Templating stuff](/entries/frameworkless-js---rendering-templates.html)

What about when we really want to use something like Knockout in our application...

In actual fact, the issue I was being specifically asked to address at this client was that they had chosen to use Knockout on their projects and were generally being quite successful with it until they ran into a situation where getting it to fit was causing issues.

*How can we build an application which uses Knockout except when it doesn't*

A slightly different question and on the surface really obvious but the temptation is there to treat KO as a framework rather than a library and let it rule our application.

*The difference between a framework and a library is that you use a library, whereas a framework uses you*

So okay then, let's pull in Knockout

    npm install knockout --save

*gasp*, yes indeed - it is shipped as a self-contained NPM module which is pretty pleasing if you're wanting to develop applications in this way.

    var ko = require('ko')

Being that we only had a couple of hours to complete this programming exercise we needed something simple to apply this to and went for the overly simplistic example of "binding to a select list". This is a bit unrealistic and I apologise for this - binding to the events of a single element isn't really the best excuse for importing a large library like KO into your application(!!).

# Building the customer list

First we can shove some test data into a file *testdata.js*. 

    module.exports = {
      banks: [ "spv", "fana", "lloyds" ],
      customers: [
        { bank: "spv", name: "bob" , desc: "bob is a late payer"},
        { bank: "fana", name: "alice", desc: "alice always pays on time"},
        { bank: "lloyds", name: "craig", desc: "craig is chillin on sunday"  },
        // etc
      ]
    }

And we can bind this to a template that looks like this

    <table>
      {{#customers}}
        <tr><td>{{name}}</td><td>{{bank}}</td></tr>
      {{/customers}}
    </table>

With the following application code in *app.js*

    var mustache = require('mustache')
      , domReady = require('domready')
      , testdata = require('./testdata')
      , fs = require('fs')

    var template = fs.readFileSync(__dirname + "/customers.html")

    domReady(function() {
      var container = document.getElementById('container')
      container.innerHtml = mustache.render(template, testdata)
    })

Notice that we use a relative path in our require statement to bring in whatever code was exported via *module.exports* in the testdata.js file. Other than that, there is nothing new here so moving on.

*Filtering the list of customers*

I want a dropdown to do this with and for now I'm going to add that to the customers template - we'll see shortly why that might be a bad idea but right now I'm going for the prize and don't know anything about future problems.

    <select name="bank" data-bind="options: banks, value: selectedBank">
    <table>
      {{#customers}}
        <tr><td>{{name}}</td><td>{{bank}}</td></tr>
      {{/customers}}
    </table>

Hooking this up in our *app.js* is just the standard Knockout code that we've all seen before, and that I *always* have to go onto the documentation website to remember how to do ;-)

    var mustache = require('mustache')
      , domReady = require('domready')
      , ko = require('knockout')
      , testdata = require('./testdata')
      , fs = require('fs')

    var template = fs.readFileSync(__dirname + "/customers.html")
      , container = null

    domReady(function() {
      container = document.getElementById('container')
      renderCustomers(customers)
    })

So I've broken this up a bit and I'm passing in a custom model

    function renderCustomers(customers) {
      container.innerHtml = mustache.render(template, { customers:  customers })
      bindBankSelection()
    }

And we bind to the bank selection in a standard way

    function bindBankSelection() {
      var model = {
        banks: ko.observableArray(testdata.banks),
        selectedBank = ko.observable()
      }
      ko.applyBindings(model, container)
      model.selectedValue.subscribe(onBankSelected)
    }

    function onBankSelected(bank) {
      ko.cleanNode(container)
      renderCustomers(filterCustomersByBank(bank))
    }

*this is a bit crap*

Yeah - so this is the spaghetti code we're talking about occuring if you're not using a framework. Re-rendering the whole page just to update a list of customers? Why re-render the select control at all? Why have we got two rendering systems slamming into what is effectively a global dom element with global data?

Clearly there has to be a better way to have our cake *and* eat it and we'll talk about that in the next blog entry about building composable widgets.

