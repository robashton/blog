Mundane blog posts here we go! I recently got asked about adding NVIDIA capability to our stack and that is a process I went through and completed with only moderate frustration. I don't tend to write very much C in my day to day job (This is by design, nobody wants me writing C and I don't want to be writing C) but here we are writing C because that's the easiest way to integrate native stuff with Erlang and I'm not about to go and learn Rust just to play with an SDK that we may not end up using in production. I will go and learn Rust in 2021 though, Rust is where our native stuff is heading but I digress - thisis the life of a developer working in this sort of environment - putting off learning new things until it becomes strictly necessary or we'd  never get any bloody work done.

We'll ignore the Erlang bit for the most part, because I could probably write several blog posts on my experience of writing NIFs (badly) alone, and we'll just take a quick overview of how the NVIDIA stuff fits together because coming to this task entirely from scratch with no knowledge of how the various bits fit together made set up that little bit more interesting.

My OS
==

I run Nixos, my entire system including drivers, software packages, configuration and such exist on Github in a [repository](https://github.com/robashton/nixos-install) and it makes set up on new hardware or re-paving from scratch on existing hardware a very simply task of cloning a repo and running it against that hardware.

The hardest thing about the setup of this laptop with the Nvidia chip on it was disabling the GPU in the first place entirely so to squeeze as much battery life out of it as possible whilst sitting in bars at conferences and such and writing code until somebody wants to chat to me. (I actually did this whilst sat at a conference in Lithuania, that seems a different world now..). There is a perfectly good integrated Intel GPU (that also does hardware accelerated encodes/decodes too!) that I've been using for the lifetime of his hardware (and we use this stuff in production so it makes sense to use it locally).

The first thing I needed to do was update the kernel to latest and survey what the state of Nvidia drivers/etc is in this world having not looked at it for a couple of years. The answer is 'in a state of flux' (*when isn't it?*), but it looks like we've finally got sensible offloading of the GPU built in and in theory we can run an X session off-screen for doing rendering and such on that GPU without draining the battery just because I was silly enough to open vim inside a terminal emulator. 

The support for the encode/decode functionality is built into these drivers in the form of dynamically loaded libraries and they need to be set up properly before anything will work. On Windows this probably just means running an EXE, on MacOS it probably means dragging an icon onto another icon and on Nixos I need configure the drivers with the information of where to find the hardware and how I want it to operate.

I had been lying to ACPI to get my touchpad working with Nvidia disabled, but now comes the time to tell the truth once more..

```
    boot.kernelParams = [ "acpi_osi=Linux" ];
```

Hey NVIDIA, this is where my hardware is (located using lspci)

```
  hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
  hardware.nvidia.prime.intelBusId = "PCI:0:2:0";
```

And I want the official drivers pls, blacklist nouveau - don't even think about it pal.

```
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.prime.offload.enable = true;
  hardware.nvidia.nvidiaPersistenced = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  boot.kernelModules = [ "nvidia-uvm" "nvidia-drm" ];
  boot.blacklistedKernelModules = [ "nouveau" ];
```

And also the X11 packages for this stuff

```
  environment.systemPackages = with pkgs; [
    linuxPackages.nvidia_x11
  ];
```

And may as well get the opengl stuff set up while we're here, although it's not strictly useful for the encodes it's good for testing the hardware itself.

```
  hardware.opengl = {
    enable = true;
    driSupport = true;
  }
```

I also follow the official guidance in setting up a quick bash script to run things with the Nvidia GPU instead of the default Intel one.

Having a Nixos setup is quite nice, a quick rebuild and I have a new boot option to start up with all of this enabled (and the old option is still there in case I got anything wrong, which I definitely did in my first few passes here).

Anyway, this gives me a few things..

- *glxgears*:  Woo, spinny gears on my laptop powered by Intel
- *nvidia-offload glxgears*: Woo, spinny gears powered by Nvidia
- */run/opengl-driver/lib/libnvidia-encode.so*: Library for doing encode stuff
- */run/opengl-driver/lib/dri/nvidia_drv_video.so*: The actual video driver

Happy this is all setup, I need to look next at the tools required to write code that uses these things...

The project stuff
===

I don't tend to install SDKs or even development tools and such in my global environment, it nearly always ends up being the case that I need a different version of something for one project or another and because I'm on Nixos I just use Nix shells for the individual projects and their development requirements [(Something I have written a bit about)](https://purerl-cookbook.readthedocs.io/en/latest/devenv/nix.html)

It turns out I just need to add these packages to my environment and I'm good to go, I already have nvidia_x11, but I 'add' it again here so I can use it to generate some environment variables later on in the shell.nix so everything lines up.

```

  cudatoolkit
  nvidia-video-sdk
  linuxPackages.nvidia_x11

```

As a bonus, I also go and add these because it'll make the code samples build and run properly (and as code samples typically seem to be the main entrance to SDKs like this that's a helpful thing.

```
    cmake
    pkgconfig
    gcc8
    ffmpeg-full

```

As a further bonus, I go and add *gdb* to this list because I literally don't have any development tools on my host OS and I'm bound to cause a few SIGSEGVs over the next couple of weeks that need debugging.

The cuda toolkit doesn't (at time of writing) work with the modern version of GCC on my OS so I needed to explicitly pull GCC8 and set that in the environment so that when I'm building code with our standard makefiles we'll do the right thing.

```
  shellHook = ''
    export CUDA_PATH=${pkgs.cudatoolkit}
    export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib
    export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
    export EXTRA_CCFLAGS="-I/usr/include"
    export CC=${pkgs.gcc8}/bin/gcc
    export CXX=${pkgs.gcc8}/bin/g++
  '';
```

Anyway, that's my setup - seeing as I've made it to 1000 words already I'll leave the next post of "exploring the samples, SDK surface area, documentation, etc" to the next one. mundane *and* wordy, everybody's favourite.
