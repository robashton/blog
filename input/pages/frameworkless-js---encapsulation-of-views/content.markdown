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

Well in a nutshell this is about ownership. If a bit of code wants to render something to an element, then it should be responsible for binding anything to that element or anything underneath that element. Touching anything outside of that should be strongly discouraged.

This is one of the ideas that a lot of frameworks tend to push for good reason - and there are a million and one ways to do it. In this entry, we'll just take the most straight forward route of keeping the code in this single file and demonstrating the principle of ownership.

This is the code we currently have:

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

    function renderCustomers(customers) {
      container.innerHTML = mustache.render(template, { customers:  customers })
      bindBankSelection()
    }

    function bindBankSelection() {
      var model = {
        banks: ko.observableArray(testdata.banks),
        selectedBank = ko.observable()
      }
      ko.applyBindings(model, container)
      model.selectedBank.subscribe(onBankSelected)
    }

    function onBankSelected(bank) {
      ko.cleanNode(container)
      renderCustomers(filterCustomersByBank(bank))
    }

The problem at the root of all this, is that both the customer list and the bank selection are hammering on the top level element (container) and it's not clear who owns what.

How about instead of this, we say that our top-level function takes in the top level container, and then gives a sub-element to both the customer list and the bank selection code?

    var model = {
      customers: testdata.customers,
      banks: testdata.customers
      selectedBank = ko.observable()
    }

    domReady(function() {
      banks(container.getElementsByClassName('banks')[0])
      customers(container.getElementsByClassName('customers')[0])
    })

    function banks(el) {
      el.innerHTML = mustache.render(bankstemplate, model)
      ko.applyBindings(model, el)
      model.selectedBank.subscribe(onBankSelected)
    }

    function onBankSelected(bank) {
      model.customers = filterByBank(testdata.customers)
      customers(container.getElementsByClassName('customers')[0])
    }

    function customers(el) {
      el.innerHTML = mustache.render(customerstemplate, model)
    }

Something like that.

*Yuck, still a mess*

Yep, we solved the main problem which was the ownership of elements but we're using a weird shared model and not really encapsulating anything else.

I want to approach the topic of encapsulation slowly and with the demonstration of the actual problems we're trying to solve. It's very easy to run into a "let's use this pattern everywhere" and I want to put across the notion of shared modules that don't have to have a homogeneous setup.

Next up, we'll take the next steps towards that by looking at the heady days of "*what jquery did right*".




