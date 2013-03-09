I'm writing our [first feature](/entries/hotelier---writing-our-first-feature.html), and I'm going to write the code that means the un-authenticated customer can skip right through and start creating stuff with my total amazeballs system. 


Anyway, I'm going to create a view model for creating a hotel, inspiringly called HotelCreateViewModel

**View model**

    public class HotelCreateViewModel {
      public string Name { get; set ;}
      public string Description { get; set; }
    }

Why a separate view model? Because I'm going to use all of that crazy attribute stuff in ASP.NET MVC4 and I don't want that on my persistence model because from past experience it'll end up interfering with serialization and I don't want that.

**Controller**

    [HttpGet]
    public ActionResult Create() {
      return View();
    }

    [HttpPost]
    public ActionResult Create(HotelCreateViewModel model) {
      
    }


**View**

    @model Hotelier.ViewModels.HotelCreateViewModel
    @{
      ViewBag.Title = "Create";
    }

    <h2>Tell us about your hotel</h2>
    @using (Html.BeginForm()) {
      Html.EditorForModel();
      <input type="submit"/>
    }



Yes, I'm keeping things deliberately minimalistic, I just want a basic skeleton with everything in it so I can go and write a couple of basic tests to pull the whole thing together.

Let's do that now then, I'm going to write a test, that when the user hits that page and goes to the creation stuff that there is indeed a form there with the expected fields on it.

This will verify that I've managed to at least put together a working ASP.NET MVC skeleton.

**Writing my first test**

I've installed the [Mindscape Workbench](http://visualstudiogallery.msdn.microsoft.com/2b96d16a-c986-4501-8f97-8008f9db141a) which comes with Coffeescript highlighting so I can use Visual Studio to write my tests.


If I look at the files that [Zombify](http://github.com/robashton/zombify) created for me, I'll see that I have a client.coffee and a system.coffee with a couple of placeholders left for me to fill in.

The first one, system.coffee looks like this:

    class System
      constructor: ->
        @driver = new Driver '../{Replace this with your own website name'

      start: (done) =>
        @driver.start done

      #etc

I change this to

    class System
      constructor: ->
        @driver = new Driver '../Hotelier'

      start: (done) =>
        @driver.start done

      #etc

I'm then going to create a file in a new folder called "browsing_from_the_home_page.coffee"

My folder structure now looks like this:

<img src="/img/coffee-hotelier.png">

My first test is going to be quite simple, let's just dump it out for us to see

    System = require('../system')

    Scenario "A new customer wants to set up their hotel", ->
      system = new System()
      client = system.add_client()

      Given "A new customer visits our website", (done) ->
        system.start ->
          client.browse_to_website(done)
      
      When "the customer asks to set up their hotel", (done) ->
        client.ask_to_create_hotel(done)

      Then "a form should be displayed", ->
        client.can_see('form')

      And "the form asks for the name of the hotel", ->
        client.has_form_input('name')

      And "the form asks for the description of the hotel", ->
        client_has_form_input('description')

      after ->
        system.stop()
      
Now, a lot of the methods here don't actually exist yet, and I'll have to create them - to do this, we go to *client.coffee* and have a look at what we see there.

