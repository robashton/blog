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

The encode API looks nothing like the decode API in that it's sat in its own header file (which is again expected to be included as part of the repo, but the library we're going to be loading is *definitely*  expected to exist on the runtime as part of the driver install.

- *Include/nvEncodeAPI.h* contains all definitions/enums/functions/etc

and somewhere on the host OS installed as part of the driver package (for me, /run/opengl-driver/lib/)

- *libnvidia-encode.so*

Creating the API
==

The functions that we end up using are loaded as a struct of function pointers and we actually only link a single function directly from that library to get hold of that struct. As a consequence of this, every struct being passed into a function in this API tends to be versioned indepedendently, such is life.


```c

  uint32_t version = 0;
  uint32_t currentVersion = (NVENCAPI_MAJOR_VERSION << 4) | NVENCAPI_MINOR_VERSION;
  NV_ENCODE_API_FUNCTION_LIST nvenc = {NV_ENCODE_API_FUNCTION_LIST_VER};

  NvEncodeAPIGetMaxSupportedVersion(&version);
  if (currentVersion > version)
  {
    return -1; // or whatever
  }

  NvEncodeAPICreateInstance(&nvenc);

  // use the functions on nvenc

```

This doesn't do anything other than a *dlopen* of the library exporting the functions and copy the pointers to those functions onto the nvenc struct so they can be called. Invoke it once, cache the results somewhere and then we can get to the business of talking to the GPU.

Creating an encoder
==

Unlike the decode stack, the encoder doesn't seem to be built *directly* on top of CUDA, it having the capability of instantiating on top of OpenGL/DirectX/Cuda as options (and then utilise surfaces from those systems). Because we're using the decoder (and planning on using CUDA to run upscaling algorithms and such), it makes sense to use CUDA for the encoder too.

Because of this, we'll want to use our already-created CUDA context and pass this into the open session call.

```

  NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS encodeSessionExParams = { NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER };
  encodeSessionExParams.device = ctx;
  encodeSessionExParams.deviceType = NV_ENC_DEVICE_TYPE_CUDA;
  encodeSessionExParams.apiVersion = NVENCAPI_VERSION;
  void* encoder = NULL;

  nvenc.nvEncOpenEncodeSessionEx(&encodeSessionExParams, &encoder);

```

Now, this doesn't do very much other than give us an encode session that isn't initialise - and to initialise it we need to tell it what form our desired output needs to take. It is entirely possible to spend a day or two wondering why this call gives you back 'NV_ENC_ERR_INVALID_PARAM' if it's not set up correctly (and reading the sample code doesn't help because the configuration of the encoder happens across multiple files because of the way that the samples are written), the below seems to be the the minimum required..

In the Real World (tm) we have an input surface with a known width/height from the decoder, here we'll hard code it along with the frame rate (I know my source is 25fps). In the Real World a load of these args will be passed in from Erlang, not C.

```c

  NV_ENC_INITIALIZE_PARAMS initializeParams = { NV_ENC_INITIALIZE_PARAMS_VER };
  NV_ENC_CONFIG encodeConfig = { NV_ENC_CONFIG_VER };
  NV_ENC_PRESET_CONFIG presetConfig = { NV_ENC_PRESET_CONFIG_VER, { NV_ENC_CONFIG_VER } };

  initializeParams.encodeConfig = &encodeConfig;

  // The essentials
  initializeParams->encodeGUID = NV_ENC_CODEC_H264_GUID;
  initializeParams->presetGUID = NV_ENC_PRESET_P4_GUID;

  // These need to line up with the input surface dimensions
  initializeParams->encodeWidth = 576;
  initializeParams->encodeHeight = 720;

  // Ditto these
  initializeParams->frameRateNum = 25;
  initializeParams->frameRateDen = 1;

  // I seemed to need these as well
  initializeParams.enablePTD = 1;
  initializeParams.encodeConfig->frameIntervalP = 3;
  initializeParams.encodeConfig->gopLength = 50;
  initializeParams.tuningInfo = NV_ENC_TUNING_INFO_HIGH_QUALITY;


```

Once this initialize params basics is set up, we can ask the encoder to populate the actual details for us based on the preset and codec specified in the 'essentials'

```c

  nvenc.nvEncGetEncodePresetConfigEx(encode_session->encoder, initializeParams.encodeGUID, initializeParams.presetGUID, tuningInfo, &presetConfig);
  memcpy(initializeParams.encodeConfig, &presetConfig.presetCfg, sizeof(NV_ENC_CONFIG));

```

I'm not entirely sure why we don't just invoke nvEncGetEncoderPresetConfigEx on the struct already contained on the initializeParams, but the samples all end up doing this and the above code is precarious enough already (honestly, the number of ways this can go wrong with a single error is infuriating!). So we stick with the samples way of doing stuff!

I also needed to then go and set up the IDR frequency to match our requested gop length (the samples do this too) and not doing this resulted in NV_ENC_ERR_INVALID_PARAM so..


```c

    initializeParams.encodeConfig->encodeCodecConfig.h264Config.idrPeriod = initializeParams.encodeConfig->gopLength;

```

With all of the above done correctly, a call to the init fn will return a success and if not I'm really sorry you're on your own because I've served my time already.






