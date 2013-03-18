As well as [addressing resources](/entries/writing-an-odata-parser---starting-at-the-beginning.html) and [resource paths](/entries/parsing-odata---nested-resource-paths.html), in OData we apparently have the ability to invoke "service operations" as well.

From what I see, these are addressed in a pretty much identical manner to actual resources like so

    /ProductsByColor?color='red'

Or

    /ProductsByColor(3)/Category/Name?color='red'

The invocation on the server is the same in each of these cases, however in the second case we're addressing into the collection returned by the invocation.

Now, a simple solution to this will be to carry on using the existing parser, and supply these custom params as part of the overall model:

So, with the following test:

    test("/method(1)/child?foo=bar", function(result) {
      it("should have the resource specified", function() {
         assert.equal(result.resource, 'method')
      })
      it("The result should be addressed", function() {
         assert.equal(result.key, '1')
      })

      it("should have the path specified", function() {
         assert.equal(result.next.resource, 'Child')
      })

      it("should have the argument specified", function() {
         assert.equal(result.args.foo, 'bar')
      })
    })

I might then solve it by simply adding this to the end of my main expression

    OData = (
      (
        PathSegment:model 
        (
          '?'
          OperationParam:param
        )?
        
Where

    OperationParam = 
      Text:name '=' Text:value -> { name: name, value: value }
    ,

(Text is a hack so I'm not putting that here until I've defined what it actually is - similar to my kerfuffle over resource names earlier)

To build up the model we can do some inline code like so:

    OData = (
      (
        PathSegment:model 
        (
          '?'
          OperationParam:param
        )?
      ) -> { 
              if(param) {
               model.args = {}
               model.args[param.name] = param.value;
              }
             return model
           }
      | '/'
    ) 

This will have the desired effect to an extent, of course it won't pass this test:

    test("/method(1)/child?foo=bar&foz=baz", function(result) {
      it("should have 'foo' specified", function() {
         assert.equal(result.args.foo, 'bar')
      })
      it("should have 'foz' specified", function() {
         assert.equal(result.args.foz, 'baz')
      })
    })


This is where the built-in 'listOf' comes in useful:

    OData = (
      (
        PathSegment:model 
        (
          '?'
          listOf(`OperationParam, '&'):params
        )?

With an appropriate loop to build up the list

    OData = (
      (
        PathSegment:model 
        (
          '?'
          listOf(`OperationParam, '&'):params
        )?
      ) -> { 
              if(params) {
               model.args = {}
               for(var i in params)
                 model.args[params[i].name] = params[i].value;
              }
             return model
           }
      | '/'
    ) 

Now, a few things getting wrong with this whole implementation

- There is a semantic difference between a Service Operation and a Resource look-up, I'm not expressing this in the model
- The code to build up the model is getting a bit mixed into my parsing expressions, I'll watch to see if this  gets un-manageable
- I've not done 'Text' implementation properly
- I've not done 'ResourceName' implementation properly

I can live most of this for now - but I've made a note that they're making me uncomfortable so when my progress is at a suitable point I can deal with them.
