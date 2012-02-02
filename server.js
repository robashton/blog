var
  path = require('path'),
  http = require('http'),
  paperboy = require('paperboy'),

  PORT = process.env.PORT || 8000,
  WEBROOT = path.join(path.dirname(__filename), 'site');

http.createServer(function(req, res) {
  paperboy
    .deliver(WEBROOT, req, res)
    .otherwise(function(err) {
      res.writeHead(404, {'Content-Type': 'text/plain'});
      res.end("Error 404: File not found");
    });
}).listen(PORT);
