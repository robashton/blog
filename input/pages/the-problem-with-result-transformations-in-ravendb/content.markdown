I mentioned yesterday that there are some problems with how [result transformations work in RavenDB](/entries/re-thinking-result-transformations-in-ravendb.html)

In truth, there are no problems, but as [@ayende](http://twitter.com/ayende) mentioned himself, feature intersection is causing issues for development in RavenDB.

What is this feature intersection? Well, we have a *lot* of features around indexes and queries now, and they're all touching and overlapping in all sorts of strange ways - this is hardly surprising, it happens to many well-used software projects with feature requests flying in every day and so many awesome ideas to implement.

The problem that result transformations were initially trying to solve (and I know, I helped implement them in a hotel lobby with Ayende a couple of years ago), were the question of "joins" across documents. Well - this worked well, and had the added bonus of being able to define the exact result shape coming back from RavenDB (pre-forming entire view models in a single query as it were).

That's great, and now we have the ability to do what we did yesterday with yet another feature, load document in map, like so:

		public class Ponies : AbstractIndexCreationTask<Pony>
		{
			 public Ponies()
			 {
					Map = from pony in ponies
								let pet = LoadDocument(pony.id)
								select new {
									pony.Name,
									pony.Colour,
									pony.Trampstamp.
									PetName = pet.Name.
									PetSpecies = pet.Species
								}
			 }
		}

This is kinda cool, although it has implications in complexity when it comes to the indexing and re-indexing of documents that isn't what this blog entry is about.

We've ended up with information in the index which we might want as part of our output, and it's not immediately obvious how we're going to get that out, and this is also doing quite a bit of the work we might want to do with the result transformation phase. (Feature overlap)

- How about what happens if we have a standard view model that we want to return from a transform, but several different indexes we might query to do that?
- What about what happens we want to do a result transform when performing a dynamic query?
- And what if we want to use the stored data in index instead of transforming a loaded document?

We've actually got a few *niggles* around these functionalities too, existing as entries in the issue tracker (such as we don't ignore FieldsToFetch when performing a transform), and we've ended up boolean flags to "SkipTransformResults"!!

In essence, it's a bit messy and there is one immediate step we can take to fix this - any takers on what that might be?
