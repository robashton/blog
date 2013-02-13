So, how do we solve a problem like TransformResults? Easy! We make ResultsTransfomers!

![Optimus Prime](/img/prime.jpg)

Okay, so what's in a name?

Well, rather than attach the function we want to transform the results with to the index like so


		public class Ponies : AbstractIndexCreationTask<Pony>
		{
			 public Ponies()
			 {
				  Map = from pony in ponies
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

How about defining it separately like this?


		public class Ponies : AbstractIndexCreationTask<Pony>
		{
			 public Ponies()
			 {
				  Map = from pony in ponies
								select new {
									pony.Name,
									pony.Colour,
									pony.Trampstamp
								}
			 }
		}
		
And

    public class PonyWithPetViewModel 
		{

			public string PonyId { get; set; }
			public string Name { get; set; }
			public string PetName { get; set; }
			public string PetSpecies = { get; set; }
		}

	
		public class PoniesWithPets : AbstractTransformerCreationTask<Pony>
		{
			 public PoniesWithPets()
			 {
				  TransformResults = ponies => 
							from pony in ponies
							let pet = database.Load<Pet>(pony.PetId)
							select new {
								PonyId = pony.Id
								Name = pony.Name,
								PetName = pet.Name,
								PetSpecies = pet.Species
							}
			 }
		}

Then, to get a list of ponies with pets in that view model format, we can simply supply the transformer want to use as part of the query


		session.Query<Pony>()
					 .Where(pony => pony.Name == "Pinkie Pie")
					 .TransformWith<PoniesWithPets, PonyWithPetViewModel>
					 .ToList()

This is much tidier, and allows us to use transformers to elegantly create view models across any index providing the input is sane enough, it also removes some of the complexity around indexes. Look - we didn't even specify an index for this query and yet we were able to use a transformer on it, dynamic index win!

This actually took a couple of days to implement, but I still have four days of work at Hibernating Rhinos so hopefully more coming soon!
