The [interface segregation principle](http://en.wikipedia.org/wiki/Interface_segregation_principle) is slightly more relevant to the code that I write day to day than [Liskov](/entries/my-relationship-with-solid---the-misunderstood-l.html).

  <blockquote>
  The interface-segregation principle (ISP) states that no client should be forced to depend on methods it does not use. 
  </blockquote>

I talked [yesterday](/entries/my-relationship-with-solid---the-misunderstood-l.html) about the Stream class, and showed how

    public class Stream {
      public virtual bool CanRead { get; }
      public virtual bool CanWrite { get; }
      public virtual bool CanSeek { get; }

      public virtual void Read(Byte[] buffer, int offset, int amount) {}
      public virtual void Write(Byte[] buffer) {}
      public virtual void Seek(int offset){}
    }

Wasn't necessarily a violation of Liskov because the variations in its behaviour were well described by those slightly uncomfortable properties.

However, those awkward properties definitely point towards a violation of the ISP. Why? Because we have an interface - (in this case, an implicit one dictated by the Stream base class) which looks like this:


    interface Stream {
      void Read(Byte[] buffer, int offset, int amount);
      void Write(Byte[] buffer);
      void Seek(int offset);
    }

And yet not all Streams can do all of those things, hence we resort to those rather opaque properties.

Perhaps another way we'll often see violations of this in code (let's say we didn't have those properties) is the checking for specific types in methods that use the interface such as:

    if(stream is FileStream)
      stream.Write(bytes, 0, bytes.Length)

*shudder*, this stuff be bad as not only do we open up ourselves for runtime crashes when a consumer passes in something we don't recognise but we're writing opaque behaviour into our code that'll confuse consumers of that code.

**Interface segregation to the rescue**

    public interface IRead {
      void Read(Byte[] buffer, int offset, int amount);
    }

    public interface IWrite {
      void Write(Byte[] buffer);
    }

    public interface ISeek {
      void Seek(int offset);
    }
  
When we have methods that require something that Reads, we can pass in IRead, when we have methods that require something that Writes can pass in IWrite, and this is great, what if we need something that Reads *and* Writes

    public interface IReadAndWrite : IRead, IWrite {}

Okay, maybe we can do this, but what about something that Reads Writes and Seeks?

    public interface IReadAndWriteAndSeek : IRead, IWrite, ISeek {}

Now this is a bit contrived, but this is one of the reasons the .NET team made the decision to go with the CanRead/CanWrite approach beacuse otherwise we'd either simply revert to checks like

    if(Stream is IRead)

or have to do stuff with generics like

    void WriteToFile<T>(T stream, string filename) where T : IRead, IWrite, ISeek

*shudder*

**Framework Engineering**

If you're writing a framework, first off stop and don't do that... but okay, if you're writing a framework these are the compromises that you'll sometimes have to make - and that's okay.

Well described behaviour that's a little bit awkward is better than having a pile of interfaces that we have to dance around if we want to achieve something meaningful.

As mentioned yesterday, I actually don't mind the .NET teams decision to break ISP here because the usage of these streams would be much harder with the number of variations in behaviour a stream can actually have.

Tomorrow we'll look at why ISP is irrelevant in the grand scheme of things however, as we reach the final entry in this little brain-dump and talk about DI.




