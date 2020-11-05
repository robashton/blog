This blog entry is part of my "[blog about mundane stuff](/entries/blogging-the-mundane.html)" series.

I covered my [decoding](/entries/decoding-h264-with-nvidia.html) test before having a look at [context management](/entries/cuda-context-management-with-nvenc.html), so now it's time to look at re-encoding the h264 and demonstrating a round trip through the nvidia hardware.

What we have already
==

We had this workflow for testing


```

  #workflow {
    generator = #read_from_ts { name = source, filename  = <<"foo.ts">> },
    processors = [
      #nvidia_decoder { name = decode, from = { source, ?video_frames_with_stream_id(256) } },
      #x264_encoder { name = encode, from = decode },
      #ts_writer { name = write, from = encode, filename  = <<"out.ts">> }
    ]
  }


```

and what I want is

```
  #workflow {
    generator = #read_from_ts { name = source, filename  = <<"foo.ts">> },
    processors = [
      #nvidia_decoder { name = decode, from = { source, ?video_frames_with_stream_id(256) } },
      #nvidia_encoder { name = encode, from = decode },
      #ts_writer { name = write, from = encode, filename  = <<"out.ts">> }
    ]
  }

```

In the decode entry we had a series of pointers to surfaces that we'd pulled out of the decoder along with the corresponding timestamps, these are in NV12 planar format and byte-aligned (and I'm storing all that information alongside the pointer to that surface in a struct so now we just need to see what the API looks like to turn this surface into h264 once again...)

This looks something like this (It actually looks nothing like this as most of this info is stored in a linked list tied to a central pool which contains the shared information), but this is at least representative of what we have.

```

  typedef struct _nvidia_allocated_surface  {
    uint8_t* data;
    int bpp;
    int byte_width;
    int byte_height;
    size_t pitch;
  } nvidia_allocated_surface;

```
This surface is allocated *on the GPU* via CUDA, and the GPU is where we want to do our encode so hopefully we can pretty much use this data directly.


The API
==

The encode API looks nothing like the decode API


