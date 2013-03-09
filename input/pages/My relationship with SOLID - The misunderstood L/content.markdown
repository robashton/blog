I imagine my statement yesterday that OCP is "dead" will be the big bomb out of all of these blog entries, but nevertheless we push forward and look at the [Liskov substitution principle](http://en.wikipedia.org/wiki/Liskov_substitution_principle)

  <blockquote>
   If S is a subtype of T, then objects of type T may be replaced with objects of type S (i.e., objects of type S may be substituted for objects of type T) without altering any of the desirable properties of that program (correctness, task performed, etc.)
  </blockquote>

This is one of those cases where things *just make sense*, and yet people always have a hard time describing exactly what it is. I'm probably not going to spend much time on it in this blog entry because it's really boring and I doubt I can do a better job of explaining it than anybody else.

*Instead, my relationship with it..*

Well, I'll start off by saying that day to day that Liskov means nothing to me, it's almost a rule that strictly applies itself to inheritance situations and because I'm primarily these days working in langauges that don't have any real native inheritance mechamisms (prototype chaining doesn't really count), this isn't something that affects me.

Hell, you know what? When I'm working in C# it is something that I don't run into because inheritance is generally something I don't use or take advantage of (because composition is usually simpler etc etc). You can't change the behaviour of an object through inheritance if you never use it.. right? :-)

Nevertheless, I want an example anyway, and I want one we're all familiar with so I'll hit up the .NET framework, and while I can remember vague instances of being annoyed about violations in UI frameworks like WinForms those days a long behind me and I can't remember any of them.

Indeed it's actually hard to think of any examples of this in the .NET framework which aren't actually a violation of our next guideline ("interface segregation"),  and throughout the "SOLID years" if you look at other people's writing on this subject, most writings about Liskov are actually about Interface Segregation.

So let's hit up a commonly quoted example that is almost a violation and talk about it a little bit.

*The oft-quoted Stream example*

First off - I don't believe Stream *is* a violation, why not? Because its behaviour is very clearly described and doesn't change across derived instances.

It's a little bit opaque, but it's not a violation (and if you look at the design principles that produced it, the reasoning is quite clear about the balanced simplicity and "good" software design)

Let's clarify, and use a simplified version of the Stream class


    public class Stream {
      public virtual void Read(Byte[] buffer, int offset, int amount) {}
      public virtual void Write(Byte[] buffer) {}
      public virtual void Seek(int offset){}
    }

Now, the default behaviour of this is to throw an exception on any of those methods, and derived instances can do proper implementations of these, like so

    public class FileStream : Stream {
      public override void Read(Byte[] buffer, int offset, int amount) { // Read from the file }
      public override void Write(Byte[] buffer) { // Write to the file }
      public override void Seek(int offset){ // Seek to a position within the file }
    }

And maybe an implementation that reads from a HTTP request

    public class HttpStream : Stream {
      public override void Read(Byte[] buffer, int offset, int amount) { // Read from the file }
      public override void Write(Byte[] buffer) { throw new NotSupportedException(); }
      public override void Seek(int offset){  throew new NotSupportedException(); }
    }

 Now, at this point if we were to pass around the Stream object to a method like this

    public void ReadStreamIntoFile(string filename, Stream stream);

We'd be demonstrating a violation of liskov, because the base class thows an exception on Read and the derived classes change this behaviour to actually do something, similarly if I was to have a method such as 

    public void WriteFileIntoStream(string filename, Stream stream);

The FileStream would function correctly, and the HttpStream would throw a NotSupportedException (and the base class would throw a NotSupportedException - another violation)

This is why the Stream class is often quoted as an example, the derived instances change the behaviour in program-breaking ways.

*However*

    public class Stream {
      public virtual bool CanRead { get; }
      public virtual bool CanWrite { get; }
      public virtual bool CanSeek { get; }

      public virtual void Read(Byte[] buffer, int offset, int amount) {}
      public virtual void Write(Byte[] buffer) {}
      public virtual void Seek(int offset){}
    }

The behaviour as described, is that if those properties return true, then the methods are safe to call, if they return false, they're not safe to call.

It's Opaque, and feels a bit wrong - but suddenly we don't have a violation of Liskov and we're happy on this front. This is a good example of where the pragmatics of developer usage have overidden the following of arbitrary software-design "rules".

**Back to my relationship with Liskov**

I do not have a relationship with the Liskov substitution principle because I don't generally write code that has any sort of inheritance chain within it, but if I did - sometimes I guess I'd end up in the situation like the above and that would be okay. I'm okay with that for the most part.

**Summary**

Liskov is ultimately pretty boring, and unless you're writing code with lots of inheritance it isn't really a problem. Don't write code with lots of inheritance and keep this problem away from you. Winning.

As a design principle, I totally agree with it - changing derived classes behaviour is annoying, don't do it. Okay, sorted.
