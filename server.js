var
  path = require('path'),
  http = require('http'),
  paperboy = require('paperboy'),
  common = require('./common'),
  PORT = process.env.PORT || 8000,
  WEBROOT = path.join(path.dirname(__filename), 'site'),
  fork = require('child_process').fork
  
  var cachedPosts = null;
  common.getAllPostsInfo(function(posts) {
    cachedPosts = posts;
  });

http.createServer(function(req, res) {
  paperboy
    .deliver(WEBROOT, req, res)
    .otherwise(function(err) {
      if(cachedPosts !== null) {
        for(var i = 0; i < cachedPosts.length; i++) {
          var post = cachedPosts[i];
          if(req.url.indexOf(post.redirect) >= 0) {
            res.writeHead(301, { 'Location': '/entries/' + common.titleToPage(post.title) });
            res.end();
            return;
          }
        }
      }
      res.writeHead(404, {'Content-Type': 'text/plain'});
      res.end("Error 404: File not found");
    });
}).listen(PORT);

/*
var build = null
setInterval(function() {
  if(build) return
  build = fork(process.cwd() + '/build.js', [], {
    cwd: process.cwd()
  })
  build.on('exit', function() {
    console.log('re-built site')
    build = null
  })
}, 10000)

*/
