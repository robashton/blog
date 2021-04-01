Hot on the heels of my last post about [integrating Dotnet with Erlang](/entries/write-your-erlang-gen-servers-in-visual-basic.html), I was asked by a friend "Hey that looks really cool, we're stuck with Elixir at work currently - is there a migration path for us?

Well I'm glad you asked, [Erlang.NET](https://github.com/robashton/erlang.net) does indeed work with Elixir and to prove it, I've [built a skeleton application](https://github.com/robashton/elixir.net) over on Github.

What does it look like?
==

Well, first of all let me say that I've never worked with Elixir before, I don't generally like to get my hands dirty with legacy techologies, and I was never a huge fan of Ruby, so the idea of writing my Erlang with Ruby syntax never appealed either, but for science...

Let's say I've got an application in Elixir


```elixir
    { ok, result } = add_these_numbers(5,3)
    IO.puts "What a result: #{result}"
```

And I'm not satisfied with the way in which Elixirs adds these numbers together, but I do have some C# that does this much better?


Well, first off we add the Erlang.NET dependency to our mix.exs

```elixir
  defp deps do
    [
      {:dotnet, git: "https://github.com/robashton/erlang.net.git", branch: "master"}
    ]
```

And make sure that the dotnet application is going to start along with our own

```elixir
  def application do
      [
        extra_applications: [:logger, :dotnet],
        mod: {Eg, []},
      ]
  end
```

Rather than use Nuget and get Yet Another Package Manager on my machine, I chose to use Mix for this project to get my dependencies and just write some MSBuild because it's a really powerful way of defining builds in Dotnet and can do pretty much anything we ask of it. I created a dotnet project in the 'cs' directory of my Alixer project that looks like this.

```xml
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <TargetFramework>net5.0</TargetFramework>
    <EnableDynamicLoading>true</EnableDynamicLoading>
    <CopyLocalLockFileAssemblies>true</CopyLocalLockFileAssemblies>
  </PropertyGroup>

  <ItemGroup>
    <ProjectReference Include="../deps/dotnet/cslib/Erlang.csproj" />
    <Content Include="../deps/dotnet/priv/liberldotnet.so">
      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </Content>
  </ItemGroup>

</Project>
```

This means I need to configure the Erlang dotnet application to use the assemblies from the built location, no worries


```elixir
import Config

config :dotnet,
  runtime_config: to_charlist("cs/bin/Debug/net5.0/Eg.runtimeconfig.json"),
  runtime_dll: to_charlist("cs/bin/Debug/net5.0/Erlang.dll")
```

Whew, Elixering sure is *hard work* I can see why you'd want to move away from it! Almost there though.

Anyway, this means I can use my far better function in dotnet by writing a class


```csharp
    public class App : IApp<Tuple<int, int>>
    {
      public Object Start(Tuple<int, int> t) {
        return t.Item1 + t.Item2;
      }
    }
```

And call this from Elixir like so

```elixir
    { :ok, result } = :dotnet.run_app_from_assembly(to_charlist("cs/bin/Debug/net5.0/Eg.dll"), to_charlist("Eg.App"), { 5, 3 })
    IO.puts "What a result: #{result}"
```

Far better than the original, although for some reason Elixir likes to use byte arrays for its strings when dotnet wants char lists so we have to do some conversion to get from the legacy elixir.

This of course allows us to then use gen servers written in dotnet, and over time we can move away from alikser altogether.

```elixir
 :dotnet.run_app_from_assembly(to_charlist("cs/bin/Debug/net5.0/Eg.dll"), to_charlist("Eg.Gen"), %{ :foo => 1, :bar => 2 })
```

```csharp
    public record GenArgs {
        public int Foo { get; init; }
        public int Bar { get; init; }
    }

    public class MyGen : IHandleCall<Atom> { 
      GenArgs args;
      
      public MyGen(GenArgs args) {
        this.args = args;
      }

      public HandleCallResult HandleCall(HandleCallContext ctx, Atom picker) {
        switch(picker) {
          case "foo": 
            return ctx.Reply(this.args.Foo);
          case "bar": 
            return ctx.Reply(this.args.Bar);
          default:
            return ctx.Reply(new Atom("nope"));
        }
      }
    }


    public class Gen : IApp<GenArgs>
    {
      public Object Start(GenArgs args) {
        return GenServer.StartLink(() => new MyGen(args));
      }
    }
```

I hope this answers the question and helps with people struggling to move away from their legacy elixir projects, always happy to help.

