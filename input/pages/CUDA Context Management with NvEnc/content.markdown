This blog entry is part of my "[blog about mundane stuff](/entries/blogging-the-mundane.html)" series.

In the [previous blog entry](/entries/decoding-h264-with-nvidia.html) I touched on context management in CUDA being a set of choices with conflicting information from documentation to popular code samples and such.

What is the "problem"?
===

Most operations with CUDA expect there to be a CUDA context bound to the current thread. All operations against that CUDA context are serialized (unless those operations are bound to a specific stream, in which case they are serialized against that stream). That's it, that's the whole goal of "context" in CUDA, and we have more than one way of realising this in the API].


The API
===

I suspect this API has been subject to a bit of churn since CUDA was first realised and this would go some way to expaining the various ways of dealing with the context, starting off with the method that the samples tend to use..

```
  //
  // On startup
  //

  cuCtxCreate(&ctx, flags, dev);

  //
  // Repeat below until finished
  //

  cuCtxPushCurrent(ctx);

  // TODO: some operation 

  cuCtxPopCurrent(NULL);

  //
  //  When finished
  //

  cuCtxDestroy(ctx);


```

If we look at the API documentation for [cuCtxCreate](https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__CTX.html#group__CUDA__CTX_1g65dc0012348bc84810e2103a40d8e2cf), the very firist sentence we see is "In most cases it is recommended to use cuDevicePrimaryCtxRetain."

So immediately on looking up the documentation for the API that the code samples use, we are told to use something else. Now - the way I understand this, is that you can create contexts within a section of your code and use *Push* and *Pop* when using that context, and then code being invoked whilst this context is valid can do the same and you can end up with a stack of contexts that works happily together. Some searching around this reveals that there are performance penalties or even limitations over the number of active contexts in an application at the same time (operations are serialized anyway) and what we *can* do is simply get hold of the primary context with

```
  //
  // On startup
  //


  cuDevicePrimaryCtxRetain(&ctx, dev);

  //
  // Repeat below until finished
  //

  cuCtxSetCurrent(&ctx);

  // TODO: some operation
  
  cuCtxSetCurrent(NULL);

  //
  //  When finished
  //

  cuDevicePrimaryCtxRelease(ctx);


```


Now some more disparate info found in the recesses of Google/Stackoverflow/Nvidia forums

- Originally contexts weren't bindable to multiple threads at the same time
- Decode sessions cannot share contexts across threads by default
- Contexts take up a chunk of ram, buffers are not sharable across contexts
- A primary context is analogous to the device itself
- A failure on the primary context is going to cascade  into all users of that context
- contexts can be defined as 'floating' if they're not bound to a thread by default
- most operations against a context are async, and not complete until you call 'synchronise'

It turns out that the nvidia decode/encode API provides another mechanism on top of contexts - the *lock*, which actually means we can share the same context across multiple decode sessions without too much issue. These are the choices I made with the above information given the needs of our encode/decode work:


- We will be running multiple encode/decode *processes* that should be completely isolated, this implies that context *needs* creating per process.
- We can share the context throughout that process so long as we use locks
- Locks perform the same job as Push/Set/Pop, but with a mutex involved to make sure all work is serialized across threads


```
  //
  // On startup
  //


  cuCtxCreate(&ctx, flags, dev);
  cuvidCtxLockCreate(&lock, ctx);


  //
  // Repeat below until finished
  //

  cuvidCtxLock(lock, 0);

  // TODO: some operation
  
  cuvidCtxUnlock(lock, 0);

  //
  //  When finished
  //

  cuvidCtxLockDestroy(lock);
  cuCtxDestroy(ctx);


```

In my tests (spinning up multiple processes/tests), this seemed to be the route to getting a lowish resource usage, a good throughput and most importantly a lack of errors. If more throughput is required, then the concept of 'streams' can be utilised against this same context for further parallelisation (that seems to be a case of creating streams per ... well... stream of work and just passing that reference around as a synchronisation point into the various API calls).

Because the cuvidCtxLock is a cuvid concept and not a CUDA concept, we we can pass a pointer to this lock into the decoder instantiated in the last blog entry so it will automatically use that lock when performing operations against the bound context and play nicely with our code.

We can replace every instance of Push and Pop in that blog entry with Lock and Unlock, and add the lock to the decoder creation params to take advantage of this


```
  decode_create_info.vidLock = session->lock;
  cuvidCtxLock(session->lock, 0);
  cr = cuvidCreateDecoder(&session->decoder, &decode_create_info);
  cuvidCtxUnlock(session->lock, 0);

```

I couldn't work out if we actually wanted to use Lock/Unlock around decoder creation, but it didn't hurt so in it went.

I *think* that this is how these APIs should be used, I give no claims to actual correctness - the documentation is vague and contradictory in places with samples/such but I have stress tested this and I've also demonstrated the failure cases to myself (multiple threads, no locks) to hilarity so it's probably close enough to be right.

Note: The transform/encode side of the API doesn't provide config to use this locking mechanism and that feels a little bit like the left hand not knowing what the right hand is doing, but hey ho - use what we can, when we can.
