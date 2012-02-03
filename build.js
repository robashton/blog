var fs = require('fs');
var wrench = require('wrench');
var _ = require('underscore');
var plates = require('plates');
var common = require('./common');
var RSS = require('rss');

var compilePosts = function(posts) {

  var feed = new RSS({
    title: 'Rob Ashton\'s blog',
    description: 'Software development dumping ground',
    feed_url: 'http://feeds.feedburner.com/robashton',
    site_url: 'http://codeofrob.com',
    image_url: 'http://codeofrob.com/img/cover.jpg',
    author: 'Rob Ashton'
  });

  wrench.rmdirSyncRecursive('site/entries/');
  fs.mkdirSync('site/entries', '777');
  var pageTemplate = fs.readFileSync('input/source/pagetemplate.html', 'utf8');
  for(var i = 0 ; i < posts.length; i++) {
    var post = posts[i];
    var outputFilename = 'site/entries/' + common.titleToPage(post.title);
    var inputHtml = fs.readFileSync('input/pages/' + common.titleToFolder(post.title) + '/content.html', 'utf8');
    var commentHtml = '';
    var inputComments = JSON.parse(fs.readFileSync('input/pages/' + common.titleToFolder(post.title) + '/comments.json', 'utf8'));
    
    feed.item({
        title:  post.title,
        description: inputHtml.replace(/\<!\[CDATA\[/gm, '').replace(/\]\]\>/gm, ''), // replace(/<(?:.|\n)*?>/gm, '').substr(0, 512) + '...',
        url: 'http://codeofrob.com/entries/' + common.titleToPage(post.title),
        date: post.date
    });
    
    for(var j = 0; j < inputComments.length; j++) {
      var comment = inputComments[j];
      commentHtml += '<div class="comment">\n';
      commentHtml += '<div class="comment-author">\n';
      commentHtml += '<img src="' + comment.author.avatar + '"/>\n';
      commentHtml += '<p>' + comment.author.name + '</p>';
      commentHtml += '</div>\n';
      commentHtml += '<div class="comment-body">\n';
      commentHtml += common.textToHtml(comment.text);
      commentHtml += '</div>';
      commentHtml += '</div>\n';
    }
    
    var pageHtml = plates.bind(pageTemplate, { post: inputHtml, title: post.title, "post-title": post.title, "post-comments": commentHtml });
    fs.writeFileSync(outputFilename, pageHtml, 'utf8');
  }
  
  var rssFeed = feed.xml();
  fs.writeFileSync('site/rss.xml', rssFeed, 'utf8');
};

common.getAllPostsInfo(function(posts) {
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
