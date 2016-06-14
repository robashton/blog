[Vir](http://github.com/robashton/vir) is a bunching together of various scripts we had lying around our projects written in a combination of Bash and Erlang to manage the execution/release/etc process for our applications. It does what we need to do and stops about there - I suspect the best way of using Vir is to either use it as it is or fork it for your own organisation rather than trying to make it help everybody.

The easiest way to get started with vir is to clone it to ~/.vir and add this folder to the path, but it'll work if you just add it to a GH repo and run it locally to so... whatever - do what you want it's just a bash script.

Anyway, running vir should give us a list of possible commands, for now we'll just create an empty web application in a folder, so do something like the below..

    mkdir awesomeapp
    cd awesomeapp
    git init
    vir init -t web awesome
    git commit ...

This creates an application called "*awesome*" (and builds it) based off the *web* and gives us a folder structure that looks similar to below:

As mentioned in a previous blog entry, this is at immediate glance a lot to digest, but we can go through it a little at a time and see just what has been created for us.

**Config**

    apps/awesome/release-files/sys.config
    apps/awesome/src/awesome_config.erl

sys.config is a standalone file containing various key-value pairs of config and awesome_config is a wrapper that provides an API to read that file. Not much to see here.

**Application startup**

    apps/awesome/src/awesome_app.erl
    apps/awesome/src/awesome_sup.erl

An application requires something that implements the OTP Behaviour "Application" (*awesome_app.erl*), and if I want child processes within the structure I'll need a supervisor to manager them, that's (*awesome_sup.erl*).

**A web application**

    apps/awesome/src/awesome_cowboy.erl

This is just a OTP genserver that uses Cowboy (one of our dependencies) to create a simple http listener.

**Release artifacts**

    deployment/build_no
    deployment/major_ver
    deployment/minor_ver

This is a cheap way of bumping version for the application during a release cycle.

**Dependencies**

    deps/cowboy/
    deps/cowlib/
    deps/edown/
    deps/gen_leader/
    deps/goldrush/
    deps/gproc/
    deps/jsx/
    deps/lager/
    deps/ranch/

Lots of folders containing lots more of the above. They were cloned and brought in because the Makefile contains a list of dependencies.

In reality we're only explicitly bringing in *cowboy*, *gproc*, *jsx* and *lager* and the others are further dependencies of these. Because Erlang operates in a single global namespace you can't do explicit imports ala NodeJS and have multiple versions of things in the application.

It doesn't matter too much anyway because dependency applications often spin up a fleet of processes on start-up rather than simply operating as library code, so you wouldn't want more than one version of an application running within a project.

**Various Manifests**

    apps/awesome/relx.config
    apps/awesome/src/awesome.app.src

*relx.config* is a manifest specifying how to do a release with all the appropriate files, and *awesome.app.src* tells the boot system what state our application needs to be in before it can be started.

**A Makefile**

    Makefile

Yup, we use make.

Using it
==

So how do we use this? Well the bash script we just ran probably did all this already but loosely our general dev cycle will be

    make -j apps         #  "make in parallel, the apps only, ignore the deps"
    vir run awesome      #  "vir, run the app please"

If we add new dependencies, then we'll need to run the following command after a build before running

    vir boot             # Generate bootscripts for each application based on the manifests

So what do we have when it starts up? Well, let's look at the logs first

    Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

    13:08:49.978 [info] Application lager started on node nonode@nohost
    13:08:49.979 [info] Application ranch started on node nonode@nohost
    13:08:49.979 [info] Application crypto started on node nonode@nohost
    13:08:49.980 [info] Application cowlib started on node nonode@nohost
    13:08:49.988 [info] Application cowboy started on node nonode@nohost
    13:08:49.998 [info] Application gproc started on node nonode@nohost
    13:08:49.998 [info] Application shared started on node nonode@nohost
    Mode dev not found
    13:08:50.024 [info] Application awesome started on node nonode@nohost
    Eshell V6.1  (abort with ^G)

Neato, we see all the applications specified in *awesome.app.src* started up. (Mode dev isn't found because we haven't got one and that's the default mode)

This is awesome.app.src for reference.

    {application, awesome,
     [
      {description, ""},
      {vsn, "1.0.0"},
      {registered, []},
      {modules, []},
      {included_applications, []},
      {applications,
       [
        kernel,
        jsx,
        stdlib,
        lager,
        cowboy,
        gproc,
        shared
       ]},
      {mod, { awesome_app, []}},
      {env, []}
      ]}.

See what I mean about dependencies not simply being a pile of code, that list of folders we have in deps contain actual applications that are started up before awesome_app itself.

Is it working?

    curl http://localhost:3000/index.html
    <html>
      <head>
      </head>
      <body>
        <h1>Hello world</h1>
      </body>
    </html>

Yup.

Next up
==

I'll look at our Makefile and how we handle dependencies, and explain a bit more our decisions around that.
