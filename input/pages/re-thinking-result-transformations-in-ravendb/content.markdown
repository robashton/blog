After all the work on indexes, things started getting downright *dirty* at the Hibernating Rhinos offices as we looked at re-vamping TransformResults in RavenDB.


First, a re-cap on what TransformResults actually is; consider we have a couple of documents that look something like this:

*A pony*

		{
			id: "ponies/rainbowdash",
			name: "Rainbow Dash",
			colour: '#9EDBF9',      // I actually looked this up
			trampstamp: 'rainbow-lightning',
			petid: 'pets/tank'
		}

*a pet*

		{
			id: "pets/tank",
			name: "Tank",
			species: "Tortoise",
			colour: '#0F0'          // I didn't look this up
		}

Let's say we have an index that looks like this

		public class Ponies : AbstractIndexCreationTask<Pony>
		{
			 public Ponies()
			 {
				  Map = ponies =>
                from pony in ponies
								select new {
									pony.Name,
									pony.Colour,
									pony.Trampstamp
								}
			 }
		}
					

When querying for a list of blue ponies, we might actually decide we want to know what species their pets are, we have the following options

-	Include them from the client (This brings back a lot of information that we don't need)
- Add a results transformer to the index

We mostly end up doing the last one and so we do that and it looks like this:

		public class Ponies : AbstractIndexCreationTask<Pony>
		{
			 public Ponies()
			 {
				  Map = ponies =>
                from pony in ponies
								select new {
									pony.Name,
									pony.Colour,
									pony.Trampstamp
								}
					TransformResults = (database, results) =>
							from result in results
							let pet = database.Load<Pet>(result.PetId)
							select new {
								PonyId = pony.Id
								Name = pony.Name,
								PetName = pet.Name,
								PetSpecies = pet.Species
							}
			 }
		}

This gives us the ability to send only the information we want to the client as well as pull in information from other documents, that's pretty neat, but now it's in need of a bit of TLC, as some of its functionality has been [superceded by Referenced Documents in Map](http://ayende.com/blog/160545/feature-intersection-is-killing-me-referenced-document-indexing) and it's a bit awkward as it is.

I'll talk more about that tomorrow when I go into some of the issues that we're experiencing with this feature.
