The SDK
==

There seems to be a single tar file available for the NVEnc/Dec stuff containing the headers, sample code and some PDFs that are semi-useful for learning how all this stuff ties together, so I unpacked that locally to the project I was working in and set up paths so I could include the header files from it. 

Step one in learning a new SDK or whatever is usually to

- A) RTFM
- B) Look at the samples
- C) Look at how others do it

For *A* we have some PDFs lying about with 101 instructions on how to tie stuff together (not sufficient by itself), in theory there are some full SDK references available (found via Google for older versions but not for latest, must only be for the the upper class developers signed into some portal or another that I don't have access to - or I'm terrible at Google), and we have the various header files with reasonably well documented functions lying about.

They do provide some good 'you will need to do these things in order to get a context with which to do...', but they're best read alongside the sample code to get some understanding of why it does what it does.

For *B* there is a full suite of samples, doing anything we pretty much might want to do with the SDK - but sadly (as is unfortunately common with SDKs such as this) rather than write samples directly against the APIs as would be convenient for the learner, they've built a whole abstraction in C++ and then used *that* for all the samples because it's  convenient for the author - that's *obviously* easier to learn from than just some sample code that does the bare minimum in a linear fashion - *rolleyes*.

For *C* we look towards [ffmpeg](https://github.com/FFmpeg/FFmpeg/), ([nvenc](https://github.com/FFmpeg/FFmpeg/blob/master/libavcodec/nvenc.c)/[nvdec](https://github.com/FFmpeg/FFmpeg/blob/master/libavcodec/nvdec.c)) because that *works* but acknowledge that usually when they implement various pipelines/codecs that they're often not done in the most efficient manner because it's a big pluggable abstract system and that's not the stated goal or necessarily even compatible with their architecture.

Irregardless of the quality of the samples (That's a real word as of 2020, sorry I don't make the rules), they are where I found the most value for learning how the NV stuff works, even if it meant tracing through step by step and pulling the relevant function calls and the order they're invoked in into my own test code.

From any perspective, it's pretty obvious from looking at the header files/pdfs that encode and decode are very much their own things done in their own way with their own enums and own flags and own API design, it makes sense from a hardware perspective that these are dedicated processes but that it bubbles up to the API itself so dramatically is amusing - I imagine this is the result of separate teams working largely in isolation and [Conway's Law](https://en.wikipedia.org/wiki/Conway%27s_law) holding steady...

Building the samples
==

This was a bit of a dance for me, having the wrong versions of GCC about for the purposes of building the rest of our stack initially, but essentially I found out where the right versions of GCC had ended up from my shell.nix and manually told cmake about them..

```

mkdir build && cd build

cmake -DCMAKE_LIBRARY_PATH=/run/opengl-driver/lib/ -DCMAKE_C_COMPILER=/nix/store/l2abq8hpgdjc4x7dwdps7zqcnxmjmjp4-gcc-wrapper-8.3.0/bin/gcc -DCMAKE_CXX_COMPILER=/nix/store/l2abq8hpgdjc4x7dwdps7zqcnxmjmjp4-gcc-wrapper-8.3.0/bin/g++ ..
make
make install

```

Not ideal but hey ho, this gave me a pile of binaries in my 'build' directory which I can invoke to find out what my hardware is capable of (in the case of decode)

```
~/nvidia/Samples/build]$ ./AppDec -h

Options:
-i             Input file path
-o             Output file path
-outplanar     Convert output to planar format
-gpu           Ordinal of GPU to use
-crop l,t,r,b  Crop rectangle in left,top,right,bottom (ignored for case 0)
-resize WxH    Resize to dimension W times H (ignored for case 0)

Decoder Capability

GPU in use: GeForce GTX 1050
Codec  JPEG   BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  32768  MaxHeight  16384  MaxMBCount  67108864  MinWidth  64   MinHeight  64   SurfaceFormat  NV12
Codec  MPEG1  BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  4080   MaxHeight  4080   MaxMBCount  65280     MinWidth  48   MinHeight  16   SurfaceFormat  NV12
Codec  MPEG2  BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  4080   MaxHeight  4080   MaxMBCount  65280     MinWidth  48   MinHeight  16   SurfaceFormat  NV12
Codec  MPEG4  BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  2032   MaxHeight  2032   MaxMBCount  8192      MinWidth  48   MinHeight  16   SurfaceFormat  NV12
Codec  H264   BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  4096   MaxHeight  4096   MaxMBCount  65536     MinWidth  48   MinHeight  16   SurfaceFormat  NV12
Codec  HEVC   BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  8192   MaxHeight  8192   MaxMBCount  262144    MinWidth  144  MinHeight  144  SurfaceFormat  NV12
Codec  HEVC   BitDepth  10  ChromaFormat  4:2:0  Supported  1  MaxWidth  8192   MaxHeight  8192   MaxMBCount  262144    MinWidth  144  MinHeight  144  SurfaceFormat  NV12 P016
Codec  HEVC   BitDepth  12  ChromaFormat  4:2:0  Supported  1  MaxWidth  8192   MaxHeight  8192   MaxMBCount  262144    MinWidth  144  MinHeight  144  SurfaceFormat  NV12 P016
Codec  HEVC   BitDepth  8   ChromaFormat  4:4:4  Supported  0  MaxWidth  0      MaxHeight  0      MaxMBCount  0         MinWidth  0    MinHeight  0    SurfaceFormat  N/A
Codec  HEVC   BitDepth  10  ChromaFormat  4:4:4  Supported  0  MaxWidth  0      MaxHeight  0      MaxMBCount  0         MinWidth  0    MinHeight  0    SurfaceFormat  N/A
Codec  HEVC   BitDepth  12  ChromaFormat  4:4:4  Supported  0  MaxWidth  0      MaxHeight  0      MaxMBCount  0         MinWidth  0    MinHeight  0    SurfaceFormat  N/A
Codec  VC1    BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  2032   MaxHeight  2032   MaxMBCount  8192      MinWidth  48   MinHeight  16   SurfaceFormat  NV12
Codec  VP8    BitDepth  8   ChromaFormat  4:2:0  Supported  0  MaxWidth  0      MaxHeight  0      MaxMBCount  0         MinWidth  0    MinHeight  0    SurfaceFormat  N/A
Codec  VP9    BitDepth  8   ChromaFormat  4:2:0  Supported  1  MaxWidth  8192   MaxHeight  8192   MaxMBCount  262144    MinWidth  128  MinHeight  128  SurfaceFormat  NV12
Codec  VP9    BitDepth  10  ChromaFormat  4:2:0  Supported  1  MaxWidth  8192   MaxHeight  8192   MaxMBCount  262144    MinWidth  128  MinHeight  128  SurfaceFormat  NV12 P016
Codec  VP9    BitDepth  12  ChromaFormat  4:2:0  Supported  1  MaxWidth  8192   MaxHeight  8192   MaxMBCount  262144    MinWidth  128  MinHeight  128  SurfaceFormat  NV12 P016

```

Honestly it's kinda impressive how much my nearly three year old laptop is capable of, dedicated hardware decode of 4k HEVC? Sure thing... 

The sample code then
==

The sample code is essentially boils down to a pile of projects under 'AppEncode', 'AppDecode', and 'AppTranscode' directories - in theory demonstrating in isolation how to do those tasks, I say in theory because all of the examples are effectively a main(int argc, char\*\* arg) that parse the command line and then spin up the shared code in the NvDecoder/NvEncoder directories.

That's super annoying, as it means the useful code is sat inside two C++ classes plus a pile of inheritance to change the behaviour at times and if you want to work out what one of these 'isolated' examples are actually doing you have to traipse up and down a pile of abstracted mess to work out how the API is actually being invoked. This is a masterclass in how *not* to write useful example code for an SDK in case anybody is still unclear as to how I feel about this.

The useful code for decoding exists in a single class, *NvDecoder*, with most of the interesting code taking place in *Decode* and *GetFrame*. There is a lot of work happening under the covers which will need looking at in a following blog post, but what is interesting here is that Nvidia provide the means not only to decode the video bitstream, but also to parse the codec around that bitstream. That's kinda cool because this isn't a small feat - it's quite common for video decoder APIs to only deal with the bitstream which makes the cost of entry quite high unless you've already got all of that code lying about. Of course if you're already parsing the nals and such in your pipeline you can skip that aspect of the Nvidia API (this being what is recommended) but because their parser is built to work with their decoder that's clearly what I'm going to start with.

The useful code for encoding exists in a single class, *NvEncoder*, but with various overrides in things like *NvEncoderCuda*, *NvEncoderD3D11*, etc. So that's a pain in the arse, not to mention the heap of boolean decisions around #IFDEFs, if(m_bSomeFlag) making it difficult to work out what one of the isolated examples is doing.  It is safe to say that understanding the encoding process is going to be frustrating.


Next steps
==

Honestly getting this far was a faff in itself, some of it was harder because of my Nixos setup and some of it was easier (Side by side GCC installations and such are never that much fun), and my driver set up whilst easy on paper took me a few attempts to get correct because of my previous efforts in entirely disabling the GPU...

Having the samples building and working though, we can be confident that if I write the right code in the next steps that the rest of it will work...
