**This post was written on Wednesday**

Now we have the [Admin functionality](/entries/this-week,-lets-create-a-start-up---day-1.html) and the [Public functionality](/entries/this-week,-lets-create-a-start-up---day-2.html) spiked properly into, it's time to look at file uploads properly.

Sam has a tool which will (client-side) be used to process audio files for upload in a very simple and stripped down manner (built specifically for his target demographic) - I quite like that.

I also have admin forms for this task and this is where our first bit of feature-confusion happens.

*Rob: Okay, so you have audio files attached to sermons, I've implemented that as a list of value types with a URI and display name*

Sam: "We probably want to have specific buckets for "High quality audio", "Low quality audio" and "Sermon notes"

*Rob: Wait what? Sermon notes, we can upload arbitrary files?*

Sam: "Not arbitrary, we have two types of audio file and often a PDF for the sermon notes"

*Rob: Okay, seems like these are going to be explicit UI concepts so I'll bake that into the actual domain*

Sam: "Yeah, that'll work... oh except sometimes they upload the entire sermon as well as pieces from the sermon"

*Rob: So we have explicit files to be uploaded and arbitrary files, I don't mind handling that, although the UI could be complicated*

Sam: "Hmm, I really want them to be told what to upload as that's user friendly, but I also want the flexibility of anything"

There was also the matter of workflow and how to expose this functionality to the uploading client he was building.

This was a bit of an ongoing conversation, and over the day I actually tried a few workflows out to see what would be easier, and we settled for being able to upload any files at all, but the server would work out what the files were and categorise them when displaying them to either the admin or the public. (Iterating has the advantage of fast feedback)

As for the workflow, I decided I'd use plain old HTML forms with re-directs and links for workflow, done in such a manner that his client can use the same API as the user does through the browser. I can't quite do it with the routes I'd want because I couldn't get ASP.NET MVC to play ball without a bit of faffing around- and I haven't got time to do that. The concepts are there and if we want to be purists later we can change the server without risk of the client breaking so there's that.

With regards to S3, I was surprised how easy this was, I pulled the Amazon SDK NuGet package, and in less than 10 minutes had files being sent to S3, using the Task libraries to manage multiple uploads at the same time. +1 point to the .NET eco-system at this point - definitely better than it used to be.

My experience with ASP.NET MVC is even happier today, making peace with it seems to be paying off, my most pleasing find today was the support for arrays:

    for(var i = 0; i < Model.Items.Length; i++ ) {
      Html.EditorFor(x=> x.Items[i].Title)
    }

That made me a happy person. (I last used MVC to build a full product in the days of MVC1 and this was not available)

I spent a bit of time making all the workflows consistent across the admin site and the public site, and polishing the styling and data being displayed across all the pages so I could "mark those features as being done" (After using them a bit and getting feedback they weren't going to change substantially so it was worth investing that time now so I didn't have to later).

We also get everything up onto Amazon so it worked, and verified all the admin functionality and public functionality existed in the way desired.

So that's that, in three days we have built a working product and gotten it deployed into the cloud - and on top of the .NET framework too, which is probably my biggest surprise.

The next two days are about adding data partitions per church, getting the desktop client to use the API properly and adding search/navigation functionality for the viewing public and hopefully even some profile pages for the different preachers along with their sermons. I'll probably nail some pegs into the ground and write some stabilising tests around this stuff so Sam can carry on moving forwards without me keeping the code in a reasonable shape. We'll see how much time we have.

I'm pumped - I *love* building stuff.
