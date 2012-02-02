var http = require('http');
var jsdom = require('jsdom');
var _ = require('underscore');
var EventEmitter = require( "events" ).EventEmitter;

var InitialDataPass = function() {
  EventEmitter.call(this);
  this.maximumCategories = 13;
  this.posts = [];
  this.pendingPosts = 0;
  this.pendingCategories = 0;
};

InitialDataPass.prototype = {
  getPosts: function() {
    return _(this.posts)
      .chain()
      .uniq(false, function(item) { return item.title; })
      .sortBy(function(item) { return -item.date; })
      .value();
  },
  retrieveCategory: function(i) {
    this.pendingCategories++;
    var self = this;
    jsdom.env('http://internal.codeofrob.com/category/' + i + '.aspx',
    [ 'http://code.jquery.com/jquery-1.5.min.js' ],
    function(errors, window) {
      var $ = window.$;
      $('div.post').each(function(){
        self.processPost($(this));
      });
      self.pendingCategories--;    
    });
  },
  processPost: function(element) {
    this.pendingPosts++;
    var post = {
      link: element.find('div.title a').attr('href'),
      title: element.find('div.title a').text()
                    .replace('&amp;', '&')
                    .replace('&rsquo;', '\''),
      date: new Date(element.find('div.info a').eq(0).text())
    }
    this.fetchPostContent(post);
  },
  fetchPostContent: function(post) {
    var self = this;
    jsdom.env('http://internal.codeofrob.com/' + post.link,
    [ 'http://code.jquery.com/jquery-1.5.min.js' ],
    function(errors, window) {
      var $ = window.$;
      post.content = $('div.post div.body').html();
      self.addPost(post);   
    });
  },
  addPost: function(post) {
    this.posts.push(post);
    this.emit('post', post);
    this.pendingPosts--;
    this.determineIfFinished();
  },
  determineIfFinished: function() {
    if(this.pendingPosts === 0 && this.pendingCategories === 0)
      this.emit('finished');
  },
  run: function() {
    for(var i = 1 ; i <= this.maximumCategories ; i++) {
      this.retrieveCategory(i);
    }
  }
};
_.extend(InitialDataPass.prototype, EventEmitter.prototype);

exports.InitialDataPass = InitialDataPass;


