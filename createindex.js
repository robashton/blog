var http = require('http');
var jsdom = require('jsdom');
var _ = require('underscore');
var EventEmitter = require( "events" ).EventEmitter;
var fs = require('fs');

var CreateIndex = function(posts) {
  EventEmitter.call(this);
  
  this.posts = posts;
  this.data = '';
};

CreateIndex.prototype = {
  saveTo: function(path) {
    fs.writeFile(path, this.data);
  },
  run: function() {
    this.data = '{';
    for(var i = 0; i < this.posts.length; i++) {
      if(i > 0) this.data += ',';
      var post = this.posts[i];
      this.data += '\n\t"' + post.date.toString() + '": ' + '{';
      this.data += '\n\t\t"title" : "' + post.title + '",';
      this.data += '\n\t\t"link" : "' + post.link + '",';
      this.data += '\n\t}'
    }
    this.data += '\n}';
    this.emit('finished');
  }
};

_.extend(CreateIndex.prototype, EventEmitter.prototype);

exports.CreateIndex = CreateIndex;
