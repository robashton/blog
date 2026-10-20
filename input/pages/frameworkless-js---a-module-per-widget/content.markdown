So we've learned from jQuery that we can hide the details of interaction with the dom behind a single function and it *does* work and *has* worked for many years so...

- [Look ma no Frameworks](/entries/look-ma,-no-frameworks.html)
- [An example](/entries/frameworkless-js---an-example.html)
- [Getting started](/entries/starting-the-frameworkless-js-project.html)
- [Templating stuff](/entries/frameworkless-js---rendering-templates.html)
- [Demanding Knockout](/entries/frameworkless-js---but-we-*really*-want-to-use-knockout-for-that-bit.html)
- [Encapsulation of views](/entries/frameworkless-js---encapsulation-of-views.html)
- [A lesson from jQuery](/entries/frameworkless-js---what-jquery-did-right.html)

Let's do it ourselves.

### Creating our customer list as a module

I'm going to do things a bit differently in this blog series to the example on Github to make the point that it's entirely up to us to do things how feel like. In the example on Github the modules are exposed as an instantiable object in a presenter-ish pattern.

Instead, this module is going to be exposed as a single function like so

*customerlist/index.js*
```javascript
module.exports = function(element) {

}
```

In this, we need to render the customers, so our whole module will look something like this

*customerlist/index.js*
```javascript

var mustache = require('mustache')
  , testdata = require('../testdata')
  , fs = require('fs')

var template = fs.readFileSync(__dirname + "/template.html")

function render(element, customers) {
  element.innerHTML = mustache.render(template, { customers:  customers })
}

module.exports = function(element) {
  render(element, testdata.customers)
}
```

*customerlist/template.html*
```
  <table>
    {{#customers}}
      <tr><td>{{name}}</td><td>{{bank}}</td></tr>
    {{/customers}}
  </table>
```

One thing that is immediately apparent is that I've bundled both the presentation logic and the template together in a single folder and exposed only a single function to represent that.

### Creating our bank dropdown as a module

We can do the same for the bank dropdown and have that in its own little folder too.

*bankdropdown/index.js*
```javascript
var ko = require('knockout')
  , mustache = require('mustache')
  , testdata = require('../testdata')
  , fs = require('fs')
  , html = fs.readfileSync(__dirname + "/template.html")

module.exports = function(element) {
  var model = {
    banks: ko.observableArray(testdata.banks),
    selectedBank = ko.observable()
  }
  element.innerHTML = html
  ko.applyBindings(model, element)
  model.selectedBank.subscribe(function(bank) {
  
  })
}
```

*bankdropdown/template.html*
```
<select name="banks" data-bind="options: banks, value: selectedBank"></select>
```

### Using these modules

Ignoring that I'm not handling the above events yet, our *app.js* now looks a bit like this

```
var customerlist = require('./customerlist')
  , bankselection = require(./bankselection')
  , domReady = require('domready')

domReady(function() {
  customerlist(document.getElementById('customer-list'))
  bankselection(document.getElementById('bank-selection'))
})
```

Assuming in this case that those elements exist in our document somewhere on start-up. Of course, they could equally be rendered from a template, or I could be using a document fragment for this and doing it detached... well yes - options, we have options.

### Communication between modules

One thing we've not done yet is re-enabled that dropdown to re-render that customer list. One guideline I tend to follow (although not strictly) is that these little self contained widgets shouldn't  generally be nested too deeply or be too coupled to each other.

For the customer list, there is no reason why it should know about the bank selection dropdown, as there are number of reasons why you might choose to filter the data being displayed there. 

For the bank selection, there is no reason why it should know about the customer list, as we might have a bank selection widget anywhere else in our app.

So for now we'll write the code for this in our *app.js*

```javascript

var customerlist = require('./customerlist')
  , bankselection = require(./bankselection')
  , domReady = require('domready')

domReady(function() {
  var customers = customerlist(document.getElementById('customer-list'))
    , banks = bankselection(document.getElementById('bank-selection'))

  banks.on('bank-selected', function(bank) {
    customers.filterByBank(bank)
  })
})
```

I've not written the code to support this yet, but this acts as a statement of intent, that my bank selection widget is going to raise domain specific events and I'm going to issue commands to the customer list based on those events. This is pleasantly explicit and from app.js easy to navigate into the approprate widgets to see what they do and how they work.

To support that, I need to issue events from the bank widget

```javascript
var EventEmitter = require('events').EventEmitter

module.exports = function(element) {
  // Other stuff

  var events = new EventEmitter()
  model.selectedBank.subscribe(function(bank) {
    events.emit('bank-selected', bank)
  })
  return events
}
```

EventEmitter is just a built-in node thing which I don't like all that much but it'll do in this example. I could have gone and found an alternative module in NPM for doing events but I don't have a strong opinion on how this is supposed to work.

The filtering code in customerlist is pretty obvious and just re-renders the HTML inside the element

### We could go further

In the real world we might decide to package these up as their own modules and stick them in our own private NPM repository (or a git repository).  This would mean they'd come with their own package.json and their own README. We might leave them in their little folder and stick a README inside there instead - that works too, although more discipline might be required to keep things isolated from each other.

The documentation on what these widgets returned and the small surface area they expose could fit in a single markdown file in that repository and each widget would be easily re-writeable if we decided to do things differently. Consistency doesn't necessarily have to be enforced and each widget can be done in its own style quite happily. 

Next up let's see what we can do about perhaps supporting some client-side navigation, as that's all the rage these days apparently.







