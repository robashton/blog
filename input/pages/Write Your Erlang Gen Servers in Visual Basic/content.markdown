TLDR: [Erlang.NET Released on Github](https://github.com/robashton/erlang.net)

I've been working in Erlang now for nearly a decade and for the last year or two we have been investigating what it would take to start writing a *lot* more of that Erlang in Purescript instead because in theory working in a sensibly typed language means fewer bugs means fewer support calls means more time spent working on features which can be sold and therefore this is good for profit margins.

Well let me tell you - it blows, we have lost weeks trying to write typeclasses around row types to confirm that 'this chunk of binary is good to go into this piece of code that just wants a chunk of binary' and months pontificating over whether an attribute should be declared at the type-level or merely at runtime and whilst progress has been made, it has been a slow and painful exercise.

*There has to be a better way*

I worked with one of my colleagues in a previous job many years ago - it was an honest living. We wrote VB.NET together in the heyday of .NETs 1 and 2 and by golly we were very productive indeed. It has types but it also has freedom, It has generics but it also reads really cleanly because so much of its syntax is just *words* - in other words it is almost the perfect language for writing expressive code. Since departing the world of dotnet however I have noticed its younger brother, C#, appearing in environments like Unity and other game engines as a scripting utility and that got me thinking - what if we could use host .NET in a similar way and use C# as a *real* language and build real software with it instead? (Leaving us to use VB.NET at our leisure whilst appealing to the masses that prefer C#). 

*Catching up with dotnet*

It has been a while since I touched base with dotnet, probably again about a decade. I largely quit because the default tooling was very mouse heavy and my wrists were starting to get sore (true story), not to mention most enterprise shops using the technology being [heavy with the derp](/entries/why-you-cant-be-a-good-.net-developer.html). (Some) things have apparently changed since then, tooling-wise everything seems to revolve around the *dotnet* CLI and with the language-server happenings being commonplace across most platforms, the support for writing .NET languages in Vim is not half bad out of the gate. Huzzah.

.NET Core 5.0.0 seemed the obvious candidate seeing as that seems to be a unification point for all the complicated versioning stories that Microsoft is so fond of creating. Version *5.0.0*, not just one zero - but two, that's how you know that software is ready for use!

*The solution*

We have a lot of legacy Erlang so if we're to start writing dotnet code to replace it we'll first need to find a way to interoperate - the *obvious* solution is to launch the dotnet code inside the existing Erlang Application side by side so they can talk to each other.

If we write the following app in dotnet (spin up a gen server and return the pid)

```csharp
  public class MyEnterpriseGradeApp : IApp {
    public Object Start() {
      return GenServer.StartLink(() => new MyCoolGenServer() );
    }
  }
```

We can embed it in a standard Erlang Gen Supervisor like so:

```erlang

  init([]) ->
    {ok, { #{ strategy => one_for_one }, 
           #{ start => { dotnet_shim
                       , start_link
                       , [ "priv/acme.dll", "Acme.MyEnterpriseGradeApp" ]
                       }
            , id => acme_app
            , type => worker
            }
       ]}}.

```

And this *just works* - Those of you who know both Erlang and dotnet at this point will hopefully be picking your jaw back off the floor - what on earth is going on here?

*The simplest example*

At its heart, an *IApp* is simply a class that has a Start method that'll return *something* back up to Erlang, these can be launched manually from Erlang to execute arbitrary code - for example, here is an app written in dotnet that returns a fixed string to us...

```csharp
  public class HelloApp : IApp {
    public Object Start() {
      return "Hello Joe";
    }
  }

```

Can be executed from Erlang like so

```erlang
  { ok, Result } = dotnet:run_app_from_assembly("priv/acme.dll", "Acme.HelloApp"),
  ?LOG(Result). %% Hello Joe
```

You will notice that the result from dotnet was automatically wrapped in an *{ok, Result}* tuple, as most Erlang APIs will expect that, if our dotnet code throws an exception then this will be automatically returned back as an *{ error, Reason }* instead - this seemed to be the cleanest way of expressing error conditions (and a special Exception (*TermException*) class is provided for explicitly providing that Reason to the Erlang host.

*Passing values in*

A generic implementation of *IApp* exists, *IApp<TArgs>*, for which an input can be requested by the *Start* method, for example adding two numbers together


```csharp
  public class AddApp : IApp<Tuple<Int,Int>> {
    public Object Start((int x, int y)) {
      return x + y;
    }
  }

```

```erlang
  { ok, Result } = dotnet:run_app_from_assembly("priv/acme.dll", "Acme.AddApp", { 5, 5 }),
  ?LOG(Result). %% 10
```

The observant reader will notice that we're using a dotnet *Tuple<Int, Int>* here and passing in an erlang tuple *{ integer(), integer }* and it just maps across automatically, how does this work? Ah well, go read the source code to find out or wait for the blog posts that I'll be writing on that subject, for now just accept that you can stick pretty much anything in your type signature and the equivalent Erlang types will get converted across. How about using a record instead of the tuple for example?


```csharp
  public record AddArgs (int X, int Y);

  public class AddApp : IApp<AddArgs> {
    public Object Start(AddArgs args) {
      return args.X + args.Y;
    }
  }

```

```erlang
  { ok, Result } = dotnet:run_app_from_assembly("priv/acme.dll", "Acme.HelloApp", #{ x => 5, y => 5 }),
  ?LOG(Result). %% 10
```

Not a problem. The observant reader will notice that not only is the casing automatically adjusted (Erlang is typically *snake_case*, and dotnet is typically *PascalCase*) but return type of Start is just a plain ol' object, and the same thing applies there, return anything you like and it'll get converted into something that Erlang will understand. Records on records? Not a problem.


```csharp

  public record ComplexReturnValue {
    public AnotherComplexValue Why { get; init; }
    public int Y { get; init; }
    public String Rye { get; init; }
  }

  public record AnotherComplexValue ( string Because );

  public class ComplexApp : IApp {
    public Object Start() {
      return new ComplexReturnValue {
        Why = new () { 
          Because = "We can"
        },
        Y = 1337,
        Rye = "Rittenhouse"
      };
    }
  }
```

```erlang
  { ok, #{ why := #{ because := Reason }
         , y := Number
         , rye := Whisky
  }} = dotnet:run_app_from_assembly("priv/acme.dll", "Acme.ComplexApp"),

  %% Why: "We can", y: 1337, rye: "Rittenhouse"
  ?LOG("Why: ~p, y: ~p, rye: ~p ~n", [Reason, Number, Whisky ]). 

```

*Back to that Dotnet GenServer then*

We've established that we can invoke arbitary dotnet code from Erlang, which is already probably one of the most useful things I've created this year - the Erlang eco system is almost non-existent and being able to access all of that open source technology in Nuget is going to save us a lot of future development time. Going back to the start of this blog post our very first example was one of spinning up an actual genserver in dotnet - that is to say, code that is going to execute in its own Erlang process and in turn be able to execute its own arbitrary logic outside of the single function call that kicked that off.

That example again:

```csharp
  public class MyEnterpriseGradeApp : IApp {
    public Object Start() {
      return GenServer.StartLink(() => new MyCoolGenServer() );
    }
  }
```

A standard gen server in Erlang would look something like this

```erlang
start_link(Arg1, Arg2) ->
  %% Executed in host process
  gen_server:start_link({local, ?MODULE}, [Arg1, Arg2]).

init([Arg1, Arg2]) ->
  %% Executed in new gen process
  #state { one = Arg1, two = Arg2 }
```

In the dotnet above, initial StartLink call takes place in the host process, and the callback is what is invoked in the init call and is expected to return 'some state'. At its simplest, that state could just be an empty object:

```csharp
  public class MyCoolGenServer {}
```

In Erlang, if you want to invoke some code in the process that kicked off these shenanigans, you would invoke that code via a gen_server method (call/cast) or send a message via the pid using the bang (*!*) operator, and so long as the relevant callback was implemented that would result in something happening.


```erlang
start_link() ->
  gen_server:start_link({local, ?MODULE}, []).

init([]) ->
  #state{}

handle_info(Msg, State) ->
  ?LOG(Msg),
  {noreply, State}.
```

Used as thus

```erlang
  { ok, Pid } = my_cool_genserver:start_link(),

  %% Hello Robert
  Pid ! "Hello Robert".

```

The same thing works in dotnet (woah), we just need to implement an interface on the object that gets returned in that init call


```csharp
public class MyCoolGenServer : IHandleInfo<String> {
  public HandleInfoResult HandleInfo(HandleInfoContext ctx, String msg) {
    Console.WriteLine(msg);
    return ctx.NoReply();
  }
}

public class MyEnterpriseGradeApp : IApp {
  public Object Start() { 
    return GenServer.StartLink(() => new MyCoolGenServer());
  }
}

```

```erlang
  { ok, Pid } = dotnet_shim:start_link("priv/acme.dll", "Acme.MyEnterpriseGradeApp" ),

  %% Hello Robert
  Pid ! "Hello Robert".

```

Yes, this just works - arbitrary messages sent from Erlang to a process started (and running!) in dotnet arrive and get translated accordingly. How about doing something with state then?


```csharp
public class AdditionServer : IHandleCall<int> {
  private int total = 0;

  public HandleCall(HandleCallContext ctx, int msg) {
    this.total += msg;
    return ctx.Reply(total);
  }
}

public class AdditionApp : IApp {
  public Object Start() { 
    return GenServer.StartLink(() => new AdditionServer());
  }
}
```


```erlang
  { ok, Pid } = dotnet_shim:start_link("priv/acme.dll", "Acme.AdditionApp" ),

  %% 1
  ?LOG(gen_server:call(Pid, 1)),
  %% 5
  ?LOG(gen_server:call(Pid, 4)),
  %% 11
  ?LOG(gen_server:call(Pid, 6)),

```

In Erlang we'd have a state record that needs returning as part of the handle_info callback, but that is because Erlang doesn't have mutation. Dotnet is somewhat fuller featured in this regard and we can just store our state on a member variable and forgo the extra steps required to dance around immutable structures.

*Pattern Matching in Dotnet*

Now you'll notice that our IHandleInfo/IHandleCall interfaces are generic and take the input shape expected, but it's really common in Erlang for a handle_info call to take a multitude of shapes and perform pattern matching based on those shapes to perform the correct block of logic.

Consider this code written in Erlang

```erlang

handle_info({tell_me, Pid}, State = #state { value = Value }) -> 
  Pid ! Value,
  {noreply, State};
handle_info({op, {add, X}}), State = #state { value = Value }) -> 
  {noreply, State#state { value = Value + X }};
handle_info({op, {mul, X}}), State = #state { value = Value }) -> 
  {noreply, State#state { value = Value * X }}.

```

This code either receives a *{ atom(), pid() }*, or *{ atom(), { atom(), integer() }}*, we don't have union types in dotnet so can't map this across cleanly - or *can we*. C# has some semblance of pattern matching these days as any good scripting language should - what if we just request 'Object' as our message type instead?

```csharp
public class MyCoolGenServer : IHandleInfo<Object> {
  private int value = 0;

  public HandleInfoResult HandleInfo(HandleInfoContext ctx, Object msg) {
    switch(msg) {
      case Tuple<Atom, Pid> t when t.Item1 == "tell_me":
        Erlang.Send(t.Item2, this.value);
        break;
      case Tuple<Atom, Tuple<Atom, Int>> when t.Item1 == "op" 
                                         && t.Item2.Item1 == "add":
        this.value += t.Item2.Item2;
        break;
      case Tuple<Atom, Tuple<Atom, Int>> when t.Item1 == "op" 
                                         && t.Item2.Item1 == "mul":
        this.value *= t.Item2.Item2;
        break;
      default:
        throw new TermException("Unsupported message received");
    }
    return ctx.NoReply();
  }
}
```

Okay it's not the nicest example in the world but it *works*. *Atom* and *Pid* by the way are C# types that wrap the *concept* of an Atom (just a string really) and an instance of a *Pid* respectively. An Erlang static class exists for doing things like sending data to an arbitrary pid and that's what is invoked in that above code. 

Now that's interesting, *Erlang.Send*? That smells a lot like we're calling Erlang from dotnet and you know why? It's because we're calling Erlang from dotnet.

*Using Dynamic to invoke arbitrary Erlang code from Dotnet*

There is absolutely no point in writing gen servers in dotnet if we then go and use the dotnet File or Sockets API (or perform any other IO for that matter). One of the more glorious aspects of writing Erlang is that when you're opening handles to various IO, you're actually spinning up processes that are linked to an owner and a pile of excellent behaviour is there by default to ensure that if the host crashes that the supervision tree will handle that, restart appropriate children and those handles will get *closed cleanly*. This is *baked right into OTP* and is one of the reasons why implementions of actor models in platforms such as Java/Scala (Akka) are missing 100% of the shots they're taking. (This also goes double for implementing these patterns using [core.async in Clojure](http://codeofrob.com/entries/the-ashton-disinterest-curve---clojure.html) but I digress)

It makes sense therefore to just allow the execution of arbitrary Erlang code from dotnet and therefore open access to these APIs.

Consider *[file:write_file](https://erlang.org/doc/man/file.html#write_file-2)* for example

```csharp
  result = Erlang.Modules.File.WriteFile("foo.txt", Encoding.ASCII.GetBytes("please write me to that file));
  switch(result) {
    case Atom a when a == "ok": 
      return;
    default: 
      throw new TermException"That didn't work");
  }
```

Works out of the box. How about *[file:open](https://erlang.org/doc/man/file.html#open-2)*, *[file:write](https://erlang.org/doc/man/file.html#write-2)* and *[file:close](https://erlang.org/doc/man/file.html#close-1)*?

Like most of the Erlang APIs, file:open effectively returns a pid which is linked to the parent process. This also *just works* in Erlang.NET.

```csharp

  // Open the file handle and stash the pid for future use
  Tuple <Atom, Pid> success = (Tuple<Atom,Pid>)Erlang.Modules.File.Open("file.txt", new object[] { new Atom("write") } );
  this.pid = success.Item2;

  // And then do some of this
  Erlang.Modules.File.Write(this.pid, data);
  Erlang.Modules.File.Write(this.pid, data);
  Erlang.Modules.File.Write(this.pid, data);

  // And at some point
  Erlang.Modules.File.Close(this.pid);
  this.pid = ErlNifPid.Zero;


```

In a similar vein, that also means that APIs that involve messages being sent back to the parent process work as well - this can either be implemented in the IHandleInfo as above, or we can spin up an arbitrary process in dotnet to handle those specific messages. Doing this and wrapping this functionality in a dotnet class is a far more pleasant way of hiding the low level dynamic call taking place and exporting a nicer API to the dotnet client.

```csharp
  public static class GenUdp {

  public static Pid Open(Tuple<Int, Int, Int, Int> ip, int port) {
     Pid parent = Erlang.Self();
     return Process.Spawn((Process ctx) => {
      Tuple<Atom, Pid> success = (Tuple<Atom, Pid>)Erlang.Modules.GenUdp.Open(port, 
      new object[] { Tuple.Create(new Atom("ip"), ip)
                   , Tuple.Create(new Atom("active"), true)
                   }
      });
      return ReceiveLoop(parent, process);
    });
  }


  private static ProcessResult ReceiveLoop(Pid parent, Process process) {
    process.Receive((Process process, Object msg) => {
      switch(msg) {
        case Tuple<Atom, Pid, Tuple<Int,Int,Int,Int>, Int, Byte[]> data: 
          Erlang.send(parent, data.Item5);
          return ReceiveLoop(parent, process);
        default:
          return process.Finish(new Atom("ok"));
      }
    })
  }

  public static void Stop(Pid pid) {
    Erlang.Send(pid, new Atom("stop"));
  }
```

And this would allow us to write a gen server which connects to a udp socket and writes everything it receives to disk. (Assuming we've wrapped file:open/etc the same way as we just did with UDP)

```csharp

  public class UdpToFileServer : IHandleInfo<Byte[]>, IHandleTerminate {
    private Pid udp;
    private Pid file;

    public UdpToFileServer(String filename, Tuple<Int,Int,Int,Int> ip, int port) {
      this.udp = GenUdp.Open(Tuple.Create(ip, port));
      this.file = ErlangFile.Open(filename, new object[] { new Atom("write") } );
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, Byte[] bin) {
      ErlangFile.Write(this.file, bin);
    }

    // Not necessary, but be kind
    public void Terminate() {
      GenUdp.Stop(udp);
      ErlangFile.Close(file);
    }
  }
  
```

*Building more of the application in dotnet itself*

So far we've demonstrated how you can invoke arbitary code written in dotnet from Erlang, spin up processes written in dotnet (either as standalone processes or as gen servers), and invoke arbitrary Erlang code from dotnet as well as all the magic type back and forth nonsense we've managed to pack into this library.

What if we want to build more complex functionality in dotnet? It's one thing spinning up a single process which then spins up ad-hoc processes itself, but to build a proper reliable application in dotnet what we need is the ability to define supervision trees!

In Erlang, typically the application will spin up a top level supervision tree - which is effectively a list of ids, and then modules/args to invoke for those ids in order to get a process. If one of these processes crash then depending on how that supervision tree is configured, either the whole tree will be restarted, or just the child that crashed (and then repeated crashes might then cause the supervisor itself to restart). Some of these children can themselves be supervisors and thus with a little careful reasoning about how your application needs to interact, a tree can be built that maximises robustness whilst minimising any potential downtime.

In dotnet... Well we can just do all of this in a single file - here is a supervision tree from one of the tests written in the Erlang.NET project itself.


```csharp
    public class SimpleSupApp : IApp
    {
      public Object Start() {
        return Supervisor.StartLink("primary-sup", 
                  () => new SupervisorConfig(
                    SupervisionStrategy.OneForAll,
                    new SupervisorChild [] { 
                       new SupervisorWorker("c1", () => GenServer.StartLink("genserver-one", () => new WorkerGenServer1()))
                     , new SupervisorWorker("c2", () => GenServer.StartLink("genserver-two", () => new WorkerGenServer1()))
                     , new SupervisorWorker("c3", () => GenServer.StartLink("genserver-three", () => new WorkerGenServer1()))
                     , new Supervisor("s1", () => 
                         Supervisor.StartLink("secondary-sup", 
                         () => new SupervisorConfig(SupervisionStrategy.OneForOne, 
                           new [] { new SupervisorWorker("sc1", () => GenServer.StartLink("nested-one", () => new WorkerGenServer1()))
                                 ,  new SupervisorWorker("sc2", () => GenServer.StartLink("nested-two", () => new WorkerGenServer1()))
                                 ,  new SupervisorWorker("sc3", () => GenServer.StartLink("nested-three", () => new WorkerGenServer1()))
                                 })))
                     }));
      }
    }
```

The resultant tree is along the lines of:

- PrimarySup
  - c1 : WorkerGenServer1 (called genserver-one)
  - c2 : WorkerGenServer1 (called genserver-two)
  - c3 : WorkerGenServer1 (called genserver-three)
  - s1 : Supervisor (called secondary-sup)
    - sc1 : WorkerGenServer1 (called nested-one)
    - sc2 : WorkerGenServer1 (called nested-two)
    - sc3 : WorkerGenServer1 (called nested-three)

And then we have the following behaviours

- Invoking *sys:terminate(Pid, 'arse')* on c1,c2,c3 or even s1 will result in every single process in the tree being restarted
- Invoking *sys:terminate(Pid, 'arse')* on sc1, sc2, sc3 will only result in that single process being restarted

By restarted, we mean that callback above (the stubby lambda ()=>) being invoked once again. Args can be captured and passed in here if required, it's just closures all the way down.

*Conclusion* 

Now while all the examples in this blog entry are written in C# (as that appears to be the prevailing language in that ecosystem for the moment, yes I know there are F# developers too *waves at all five of them*), all of this results in our original aim which was to write our gen servers in Visual Basic .NET.


```vb
    Public Class MyGenServerVB
        Inherits IHandleInfo(Of Msg)
        Public Sub New()
        End Sub
        Public Function HandleInfo(ByVal ctx As HandleInfoContext, ByVal msg As Msg) As HandleInfoResult
          If msg.Item1 = "hello bob" Then
            Erlang.Send(msg.Item2, "hello joe")
          Else
            Erlang.Send(msg.Item2, "weeee")
          End If
          Return ctx.NoReply()
        End Function
    End Class
```

That's all it took, I'll be following this blog entry up with how any of this works so subscribe to the RSS (lol) if you want to read that, technical stuff that you don't need to know in order to appreciate the 'it just works' nature of this integration if that's all you wanted.

Take a look over at the Github for actual [Getting Started](https://github.com/robashton/erlang.net) notes, there will no doubt be tweaks required but it should be enough for us to make some progress at least.
