var fs = require('fs');
var wrench = require('wrench');
var _ = require('underscore');
var plates = require('plates');
var common = require('./common');

var getAllPostsInfo = function(callback) {
  var folders = fs.readdirSync('./input/pages/');
  var metaCollection = [];
  for(var i in folders) {
    var folder = './input/pages/' + folders[i];
    fs.readFile(folder + '/meta.json', function(err, data) {
      var meta = JSON.parse(data);
      metaCollection.push(meta);
      if(metaCollection.length === folders.length)
        callback(metaCollection);
    });
  }
};

var compilePosts = function(posts) {
  wrench.rmdirSyncRecursive('site/entries/');
  fs.mkdirSync('site/entries', '777');
  var pageTemplate = fs.readFileSync('input/source/pagetemplate.html', 'utf8');
  for(var i = 0 ; i < posts.length; i++) {
    var post = posts[i];
    var outputFilename = 'site/entries/' + common.titleToPage(post.title);
    var inputHtml = fs.readFileSync('input/pages/' + common.titleToFolder(post.title) + '/content.html', 'utf8');
    var pageHtml = plates.bind(pageTemplate, { post: inputHtml, title: post.title, "post-title": post.title });
    fs.writeFileSync(outputFilename, pageHtml, 'utf8');
  }
};

getAllPostsInfo(function(posts) {
  posts = _(posts)
      .sortBy(function(item) { return - (new Date(item.date)); })
  
  var indexTemplate = fs.readFileSync('./input/source/indextemplate.html', 'utf8');
  var listHtml = '';
  for(var i = 0 ; i < posts.length; i++) {
    var post = posts[i];
    listHtml += '<li><a href="entries/' + common.titleToPage(post.title) + '">' + post.title + '</a></li>\n';
  }
  
  var indexHtml = plates.bind(indexTemplate, { posts: listHtml });
  fs.writeFileSync('./site/index.html', indexHtml, 'utf8');
  compilePosts(posts);
});
