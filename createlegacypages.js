var http = require('http');
var jsdom = require('jsdom');
var _ = require('underscore');
var EventEmitter = require( "events" ).EventEmitter;
var fs = require('fs');

var CreateLegacyPages = function(posts) {
  EventEmitter.call(this);
  
  this.posts = posts;
  this.pages = [];
  this.pendingPosts = 0;
};

CreateLegacyPages.prototype = {
  saveTo: function(path) {
    for(var i = 0; i < this.pages.length; i++) {
      var page = this.pages[i];
      fs.createDir(
    }
  },
  run: function() {
    for(var i = 0; i < this.posts.length; i++) {
      this.processPost(this.posts[i]);
    }
  },
  processPost: function(post) {
    this.pendingPosts++;
    var page = {};
    page.metadata = {
      date: post.date,
      title: post.title      
    };
    page.body = post.content;
    this.pendingPosts--;
    this.determineIfFinished();
  },
  determineIfFinished: function() {
    if(this.pendingPosts === 0)
      this.emit('finished');
  }
};

_.extend(CreateLegacyPages.prototype, EventEmitter.prototype);

exports.CreateLegacyPages = CreateLegacyPages;
