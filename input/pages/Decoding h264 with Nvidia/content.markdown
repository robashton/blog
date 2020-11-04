The first task for me will be to attempt to decode one of my 'known good' h264 sources. Now - my sources are nearly all transport streams with audio (often multiple) and such, so I'll need to parse those files, pull the streams out of them, filter the video stream from a single pid and send the h264 frame data into the API for decode. This is quite a lot of work and there is no way of testing whether the results are good short of either dumping them to disk and telling ffmpeg what it's looking at (raw frames), or piping that data into an encoder in code and writing out a fresh transport stream with the round-tripped h264.

I'll take that second option because I already have all the code required to do this in Erlang in our proprietary workflow engine - a simplified example of the code I'll write to test my decoder appears below. (Working inside of a mature codebase has its advantages)

```erlang

#workflow {
  generator = #read_from_ts { name = source, filename  = <<"foo.ts">> },
  processors = [
    #nvidia_decoder { name = decode, from = { source, ?video_frames_with_stream_id(256) } },
    #x264_encoder { name = encode, from = decode },
    #ts_writer { name = write, from = encode, filename  = <<"out.ts">> }
  ]
}


```

Now to get to this point, the 'best' way to achieve this is to try to mirror the underlying API as best as possible in Erlang and write as direct a NIF as possible under this. We can assume that's what I'm then using in the Erlang, that allows me to write a test Erlang file and simply calls one or two of the methods with minimum config and build that up as I go. So we'll say that that's what I'm doing with an *nvidia_test.erl* calling into an *nvidia_api.erl* which my *nvidia_decoder.erl* will eventually leverage.

We can therefore jump straight into the C and write some functions that we can assume are being called from Erlang with the appropriate arguments.

Parsing the h264
==

Each frame has a blob of 'data', which is essentially a sequence of NAL units, some of which are metadata describing the video content and some of which are the data itself. We can parse this ourselves in Erlang and pass just the data into the decoder, or we can just pass the whole lot into Nvidia's parser and use the callbacks provided by that parser to then feed a decoder.

We do actually have code lying around for parsing h264, which is why we even have the concept of a 'frame' at all in the above code, but by far the easiest way to use the Nvidia decoder to use that parser as it then ends up providing the exact structures required *for* that decoder.

Creating a parser is quite easy, we populate a CUVIDPARSERPARAMS with appropriate config and init the darned thing - this is all host code for execution on the CPU and is not remotely specific to nvidia and doesn't require any hardware setup. In my code, the parser parameters are passed in from Erlang, but hard coded below. *decode_session* is a pointer to a struct containing both the parser and a pile of information also passed in by the Erlang.

```c

  CUVIDPARSERPARAMS videoParserParameters = {}
  videoParserParameters.CodecType = cudaVideoCodec_H264;
  videoParserParameters.ulMaxNumDecodeSurfaces = 1;
  videoParserParameters.ulClockRate = 90000;
  videoParserParameters.ulMaxDisplayDelay = 0;
  videoParserParameters.pUserData = decode_session;

  videoParserParameters.pfnSequenceCallback = decode_session_handle_video_sequence;
  videoParserParameters.pfnDecodePicture = decode_session_handle_picture_decode;
  videoParserParameters.pfnDisplayPicture = decode_session_handle_picture_display;
 

 cuvidCreateVideoParser(&decode_session->parser, &videoParserParameters);

```


There are three callbacks on this struct, which are very much designed to work around the decode itself. 

- *pfnSequenceCallback*: Stream information, create the decoder if you want / Stream has changed and decoder needs reconfiguring
- *pfnDecodePicture*: Here is data and information about that data, stick it into the decoder
- *pfnDisplayPicture*: You should pulling a frame out of the decoder here cos it's ready

If the reader is unfamiliar with video streams, the reason we have two callbacks for decode/display is that frames in the incoming h264 stream are not necessarily in display order because you can have references to previous/future frames in encoded h264 data. It's not simply 'one in, one out'.

The form of the data going in therefore is 'packets' with timestamps which we can use for correlation coming back out again, calling the code from Erlang (but removing all the NIF mess) looks like this, simply throwing data at the parser and letting it do its job.

```c

static void decode_frame(decode_session* session, void* data, int size, int64_t timestamp)
{
  CUVIDSOURCEDATAPACKET packet = { 0 };
  packet.payload = data;
  packet.payload_size = size;
  packet.flags = CUVID_PKT_TIMESTAMP;
  packet.timestamp = timestamp;

  if (size == 0) {
    packet.flags |= CUVID_PKT_ENDOFSTREAM;
  }

  cuCtxPushCurrent(session->ctx);
  cr = cuvidParseVideoData(session->parser, &packet);
  cuCtxPopCurrent(session->ctx);

}


```

My first test effectively boiled down to calling this parser with a couple of frames with empty callbacks and printfs just to make sure that things were initialising as expected and the callbacks were being invoked.

```c

  static int CUDAAPI decode_session_handle_video_sequence(void *obj, CUVIDEOFORMAT* pVideoFormat)
  {
    TRACE("handle_video_sequence  \r\n");
  }

```

Each of these callbacks present us with a *void\* obj*, which is the *pUserData* passed into the parser parameters on creation, we're using a struct here containing the parser and parameters, and it makes sense to stash the decoder in this struct too.

Initialising the hardware
==

We can't create a decoder until we've parsed some h264, but the decoder API is built directly on top of some CUDA constructs and those constructs will need creating up front if we are to create that decoder. 

All CUDA operations revolve around having a CUDA context created around the device we want to use for the CUDA operations, ignoring the return results (not something I'm doing in the real code), a basic setup looks thus. (I've also stripped the Erlang comms from these implementations, as messaging code and binary reference counting is outside the scope of this blog entry).

```c

 CUdevice cuDevice = 0;
 CUcontext ctx = 0;


 cuInit(0);
 cuDeviceGet(&cuDevice, 0);
 cuCtxCreate(&ctx, 0, cuDevice);

```

Now, for all operations involving CUDA, this context will need binding to the current thread (except where in cases where various Nvidia APIs helpfully do this for us) and there are multiple ways of managing that context, the documentation tells us to do it one way whilst saying that another way is the default and the samples go on to do it in a whole other manner. I'll actually try and cover this in the next entry because it might save somebody some time in the future (or somebody might e-mail me to tell me I've completely missed the point, that'd be quite nice).

We stash this context on the struct being used in the parser above so I can then use it in calls later, but that's the extent of the setup we can do until we've parsed some of the stream.

Creating the decoder
==

In the callback for *pfnSequenceCallback*, we get told about the video format in the struct *CUVIDEOFORMAT*, and get passed our struct as a *void\* *, so the first thing to do here is grab that struct because it has some config on it, and then create a *CUVIDDECODECREATEINFO* and populate it from both the config and the information about the video given to us by the parser.

```c
  static int CUDAAPI decode_session_handle_video_sequence(void *obj, CUVIDEOFORMAT* pVideoFormat) {
    CUresult cr;
    decode_session *session = (decode_session *) obj;
    CUVIDDECODECREATEINFO decode_create_info = { 0 };

    decode_create_info.ulWidth = pVideoFormat->coded_width;
    decode_create_info.ulHeight = pVideoFormat->coded_height;
    decode_create_info.CodecType = pVideoFormat->codec;
    decode_create_info.ChromaFormat = pVideoFormat->chroma_format;
    decode_create_info.bitDepthMinus8 = pVideoFormat->bit_depth_luma_minus8;
    decode_create_info.ulTargetWidth = pVideoFormat->coded_width;
    decode_create_info.ulTargetHeight = pVideoFormat->coded_height;
    decode_create_info.ulNumDecodeSurfaces = pVideoFormat->min_num_decode_surfaces;
    decode_create_info.ulNumOutputSurfaces = 2;
    decode_create_info.ulIntraDecodeOnly = 0;
    decode_create_info.Reserved1 = 0;
    decode_create_info.ulCreationFlags = cudaVideoCreate_PreferCUVID;

    decode_create_info.ulMaxWidth = session->max_width;
    decode_create_info.ulMaxHeight = session->max_height;
    decode_create_info.OutputFormat = session->output_format;
    decode_create_info.DeinterlaceMode = session->deinterlace_mode;
  }

```

Creating the decoder itself is just a case of binding the CUDA context to the current thread and invoking the relevant API.

```c

  cuCtxPushCurrent(session->ctx);
  cr = cuvidCreateDecoder(&session->decoder, &decode_create_info);
  cuCtxPopCurrent(NULL);

```

The callback expects a return value of *< 0* if there is a failure, or  *min_num_decode_surfaces* if there is a success, so...


```c

  if ( CUDA_SUCCESS == cr ) {
    return pVideoFormat->min_num_decode_surfaces;
  } else  {
    return -1;
  }
  
```

Bit of a faff, but fairly linear at least, with these steps completed we'll start receiving parsed frame data into *pfnDecodePicture* which we can feed directly into the decoder.

```c

  static int CUDAAPI decode_session_handle_picture_decode(void* obj, CUVIDPICPARAMS* pPicParams)
  {
    decode_session *session = (decode_session *) obj;
    CUresult cr;

    cuCtxPushCurrent(session->ctx);
    cr = cuvidDecodePicture(session->decoder, pPicParams);
    cuCtxPopCurrent(NULL);

    if ( CUDA_SUCCESS == cr ) {
      return 1;
    } else {
      return -1;
    }
}

```

*pPicParams* contains the data as well as information about this frame (such as whether it's an iframe or not) and conveniently the parser gives the exact data that the decoder expects in order to do its job. With this, we'll start getting invocations of *pfnDisplayPicture* with raw frame data which we can send back into our application for further processing. This is actually a bit more complicated, as up until now the memory management has been taken care of us by the APIs we are using.

The parser is accepting our host memory buffers, and then creating buffers in host memory for its own data which are being copied into device memory by the decoder, and in order to get the data out we'll need to copy it out of these decoder managed buffers into either host or device memory.

Copying data out of device memory into host memory is expensive because of limited bandwidth and proximity but is necessary at *some* point if we are to output that data to anywhere useful. It is however not always desirable to immediately do this if we are then to do an encode or transform on the GPU, we probably want to create buffers on the device itself and perform a copy from the decoder-managed buffers onto our own managed buffers.

Our code will assume that we want the memory copied into more device memory and we'll incur another copy if we want to subsequently copy it out onto the host; this is for convenience as in the real world we'll almost certainly be doing further operations on the GPU and only moving data to the host for testing (as in the above workflow) or for very specific operations (such as some sort of fingerprinting or overlay scenario).

So, the first thing we need to do here is call cuvidMapVideoFrame, which will block until the frame specified by *picture_index* has been decoded and is ready for copying out. MapVideoFrame *effectively* locks that decoder-owned buffer and readies it for use in further calls (copying it into something we own). Once  we've called Map, we check the decode status to make sure that the data is worth copying out in the first place. I've left us a *...* as a placeholder for the next steps.

```c

static int CUDAAPI decode_session_handle_picture_display(void* obj, CUVIDPARSERDISPINFO* pDispInfo)
{
  decode_session *session = (decode_session *) obj;
  CUresult cr = 0;
  CUdeviceptr srcFrame = 0;
  unsigned int srcPitch = 0;
  CUVIDGETDECODESTATUS status = {0};

  CUVIDPROCPARAMS vpp = {0};
  vpp.progressive_frame = pDispInfo->progressive_frame;
  vpp.second_field = pDispInfo->repeat_first_field + 1;
  vpp.top_field_first = pDispInfo->top_field_first;
  vpp.unpaired_field = pDispInfo->repeat_first_field < 0;

  cuCtxPushCurrent(session->ctx);

  if((cr = cuvidMapVideoFrame(session->decoder, pDispInfo->picture_index, &srcFrame, &srcPitch, &vpp)) != CUDA_SUCCESS) {
    return -1;
  }

  cr = cuvidGetDecodeStatus(session->decoder, pDispInfo->picture_index, &status);

  if (cr == CUDA_SUCCESS && (status.decodeStatus == cuvidDecodeStatus_Error || status.decodeStatus == cuvidDecodeStatus_Error_Concealed))
  {
    return -1;
  }

  // ... 

  cuCtxPopCurrent(NULL);


```

And now we're at the meat of it, we've locked the decoded frame and need somewhere to put it. In the real world we operate a pool of surfaces which can be passed into Erlang and reference counted before being returned to the pool when Erlang has finished using that surface. In this example we'll just create a surface on demand and assume that somebody will destroy it (or re-use it) at some point - obviously creation is expensive so it is best not to be doing this on demand in reality.

We have to calculate the sizes for this surface based on the *CUVIDEOFORMAT* that we received in the pfnSequenceCallback, *bpp* will change depending on the bit depth of the source, but the only output format the decoder actually supports is NV12 so I've hard coded byte height to (Chroma = 1 * height) + (Luma  = 0.5 * height), as those are the planes  that we'll expect in this format. If you don't know about planar formats then go and [read about them](https://wiki.videolan.org/YUV) if you're writing this code haha, you'll need to understand it.

```c

  void* dstFrame;
  int dstPitch;
  int bpp = pVideoFormat->bit_depth_luma_minus8 > 0 ? 2 : 1;
  int frame_width = pVideoFormat->display_area.right - pVideoFormat->display_area.left;
  int byte_width = frame_width * bpp;
  int byte_height = (pVideoFormat->display_area.right - pVideoFormat->display_area.left) * 1.5;

  cuMemAllocPitch((CUdeviceptr *)&dstFrame, &dstPitch, frameWidth  * bpp, byte_height, 16);

```

We allocate pitched memory (also known as byte-aligned where the pitch is the stride..), essentially we want our buffer to be a be a nice round number, usually a power of 2 because  it makes for efficient read/writes. The pitch is output into 'dstPitch' which we'll need to use later on when using this buffer because maths.

Assuming we have a buffer created as above, we can copy from our mapped video frame into our new device memory with

```c

  CUDA_MEMCPY2D m = { 0 };
  CUresult cr;

  m.srcMemoryType = CU_MEMORYTYPE_DEVICE;
  m.srcDevice = (CUdeviceptr)srcFrame;
  m.srcPitch = srcPitch;
  m.dstMemoryType = CU_MEMORYTYPE_DEVICE;
  m.dstDevice = (CUdeviceptr)dstFrame;
  m.dstPitch = dstPitch;
  m.WidthInBytes = byte_width;
  m.Height = byte_height;

  cuMemcpy2D(&m);

```

If we wanted this in host memory, we'd do a straight up malloc of (byte_width * byte_height) with a pitch of byte_width, and copy it out this way


```

  CUDA_MEMCPY2D m = { 0 };
  CUresult cr;

  m.srcMemoryType = CU_MEMORYTYPE_DEVICE;
  m.srcDevice = (CUdeviceptr)srcFrame;
  m.srcPitch = srcPitch;
  m.dstMemoryType = CU_MEMORYTYPE_HOST;
  m.dstHost = (CUdeviceptr)dstFrame;
  m.dstPitch = byte_width;
  m.WidthInBytes = byte_width;
  m.Height = byte_height;

  cuMemcpy2D(&m);

```

That data can then be fired into libx264 along with *pDispInfo->timestamp*, encoded and viewed with pleasure. I guess the next post I should  probably talk about CUDA context management before I get to the job of encoding or transforming this data and  maybe it's worth talking a little about how I'm managing reference counted surfaces between Erlang and C as well as that's a whole world of fun after this.

Mundane, but hopefully useful to me or somebody else in the future.
