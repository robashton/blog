Previous entries in this series:

  - [Look ma, no frameworks](/entries/look-ma,-no-frameworks.html)

How do I start a project if I have no framework? What are the first things I do and how do I organise it? Let's have a look shall we?

# Create the repo

I use git, no brainer.

```bash
git init
touch README.md
git add README.md 
git commit -m "Readme"
git remote add origin githubetc
git push -u origin master
```

#Create a manifest

I use npm to manage my dependencies, makes sense

```bash
npm init
git add package.json 
git commit -m "package.json"
```

#Install grunt

I use grunt to run my various build/test/deploy scripts, I could just use all the tools I use stand-alone but having a single build system with tasks is documentation we then haven't got to write. It's no fun if new joiners have to install and run crap.
Grunt comes in two parts. the package itself ("grunt") and the command line package for running it ("grunt-cli")

```bash
npm install grunt --save-dev
npm install -g grunt-cli
```

Client/script/dev dependencies I usually stick in as dev dependencies ala above, we build and then we deploy the compiled assets so we don't need them in production.

What I often do is put global dependencies like this as a pre-install script for the package.json so people haven't got to remember to install them (note, not in OSS stuff as that might annoy folk):

```bash
vim package.json
````

```json
{
  "name": "awesome-app",
  "version": "etc",
  "scripts": {
    "preinstall": "npm install -g grunt-cli"
  },
  "etc"
}
```

Grunt needs a Gruntfile, this looks like this:

```bash
vim Gruntfile.js
```

```javascript
module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json')
  })
}
```

```bash
git add Gruntfile.js 
git commit -am "Grunt setup"
```

# Create the app

I'll usually have a folder called "client", which contains all the HTMl/CSS/JS in it. The build script will take these assets and build them into a "public" directory which is empty by default. I say HTML/CSS, but I actually mean Jade/Stylus.

I'm not going to write about the server code for any of this, because I don't care if we're using ASP.NET/Ruby/Node - this is a client application and the server is irrelevant.

What matters is that somehow these jade/stylus files can be rendered by the server or pre-built somehow.

Then 

```bash
touch client/index.jade
touch client/theme.styl
touch client/app.js
```

I don't bother with elaborate folder hierarchies to begin with, lumping everything in a single folder to begin with makes perfect sense and when/if it gets too big it'll probably be split up by feature rather than by whether it's CSS or not.

# Build script for my app.js

app.js is the entry point to my client application, it'll be imported in my HTML like so

```xml
<script type="text/javascript" src="/app.js"></script>
```

I don't bother with [any of that AMD stuff](/entries/why-i-stopped-using-amd.html) for the obvious reasons, but obviously I'm not going to have everythingin the same file. I like to use Browserify cos that's kinda neat and encourages us to push code into modules and install them via npm.

Now, we can get our web server to do this stuff on request, but I like to run a build script and get errors immediately when changing my files. I now have grunt, so I'll use the "grunt-browserify" package to make this part of my build.

```bash
npm install grunt-browserify --save-dev
vim Gruntfile.js
```

```javascript
module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    browserify: {
      '/public/app.js': ['/client/app.js']
    }
  })
  grunt.loadNpmTasks('grunt-browserify')
}
```

This now means I can run

```bash
grunt browserify
```

And the application will be built from "client/app" to "public/app"

This actually gets tedious after a while, so I often end up with another grunt module "grunt-contrib-watch", to watch for changes and automatically r-erun this file.

```bash
npm install grunt-contrib-watch --save-dev
vim Gruntfile.js
```

```javascript
module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    browserify: {
      'public/app.js': ['client/app.js']
    }
    watch: {
      files: [ "client/**/*.js"],
      tasks: [ 'browserify' ]
    }
  })
  grunt.loadNpmTasks('grunt-browserify')
  grunt.loadNpmTasks('grunt-contrib-watch')
}
```

This means I can now run

```bash
grunt watch
```

And not think about this again.

# Automate all the things

I actually go much further than the above, and use grunt to

- Run the web server
- Run the tests automatically (using grunt-contrib-watch again)
- Run any other servers needed (redis/phantomjs/etc)

Modules I find useful for this

- grunt-nodemon
- grunt-concurrent
- grunt-exec
- grunt-env

I'll register a custom task called 'develop' which means all any developer needs to do in order to start any work on my project is type

```bash
grunt develop
```

And they can get to work immediately.

# This is quite a lot of set-up

I don't do this very often, this is a collection of tools/libraries that I really like and have come to appreciate very much. Anything is interchangeable for other things and I'm not afraid to change any part of it at some point if something better comes up.

I do have a repo which I clone which has these things already set up for a blank project - but this is personal to me and the way I want to work. In a team this might be different (compromise) and for you it'll be different too.

So I won't be sharing it. Sorry :)
