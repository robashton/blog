In the [last entry](/entries/ravendb-resulttransformers---a-new-way-of-looking-at-things.html) I demonstrated a new artifact in RavenDB for transforming results from queries into view models with all the information attached you could think of.

Well, the primary use case (in my mind) for this, is the creation of view models for web clients without having to de-normalise data or perform multiple remote calls.

Well, actually - if we're viewing say, a single item by id, it doesn't make a lot of sense to have to do a query to get that single item by id. It doesn't make a lot of sense to have to create an index for this.

In the past, that's the best we could hope for, unless perhaps you used the 'include' functionality to load related documents and then created the view model in the client out of those related documents. This looks something like this:

    var order = session.Include("CustomerId")
                            .Load<Order>("orders/1")

    var customer = session.Load<Customer>(order.CustomerId)

    // Build view model here


This is okay and everything, but requires a bit of mental leg-work on the part of the client.

What you really want to do is just ask for the view model, sort of like making a single query in SQL to get all the information you want with one go.

Well, how about this?

      var orderViewModel = session.Load<OrderWithCustomerTransform, OrderWithCustomer>("orders/1")

That's a lot nicer, the code for this is as thus

    public class OrderWithCustomer 
    {
       public string OrderId { get; set; }
       public string OrderDate { get; set; }
       public string CustomerId { get; set; }
       public stirng CustomerName { get; set; }
    }

		public class OrderWithCustomerTransform : AbstractIndexCreationTask<Order>
		{
			 public OrderWithCustomerTransform()
			 {
					TransformResults = (database, orders) =>
							from order in orders
							let customer = database.Load<Pet>(order.CustomerId)
							select new {
                 OrderId = order.Id,
                 OrderDate = order.Date,
                 CustomerId = customer.Id,
                 CustomerName = customer.Name
							}
			 }
		}

Being able to compose view models on the server as part of the load process by pulling in related documents and only pull out the fields you need is awesome.

Being able to re-use these transforms across different indexes or load operations and get a consistent return shape is even awesomer.

Tomorrow I'll show a really cool edge case around this, and how we got rid of a lot of he confusion around transforms.
