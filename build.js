var fs = require('fs');
var path = require('path');
var wrench = require('wrench');
var _ = require('underscore');
var plates = require('plates');
var common = require('./common');
var RSS = require('rss');
var markdown = require('markdown').markdown;

console.log('Rebuilding site')

var compilePosts = function(posts) {

  var feed = new RSS({
    title: 'Rob Ashton\'s blog',
    description: 'Software development dumping ground',
    feed_url: 'http://feeds.feedburner.com/robashton',
    site_url: 'http://codeofrob.com',
    image_url: 'http://codeofrob.com/img/cover.jpg',
    author: 'Rob Ashton'
  });

  var readPageHtmlSync = function(folder) {
    var mdfilename = 'input/pages/' + folder + '/content.markdown';
    var htmlfilename = 'input/pages/' + folder + '/content.html';
    var mdstat = path.existsSync(mdfilename);
    if(mdstat) {
      var mdcontent = fs.readFileSync(mdfilename, 'utf8'); 
      var html = markdown.toHTML(mdcontent);
      fs.writeFileSync(htmlfilename, html , 'utf8');
    }
    var inputHtml = fs.readFileSync(htmlfilename, 'utf8');
    return inputHtml;
  };

  //console.log('Removing old entries')
  //wrench.rmdirSyncRecursive('site/entries/');
  //fs.mkdirSync('site/entries', '777');
  var pageTemplate = fs.readFileSync('input/source/pagetemplate.html', 'utf8');
  for(var i = 0 ; i < posts.length; i++) {
    var post = posts[i];
    var outputFilename = 'site/entries/' + common.titleToPage(post.title);
    var inputHtml = readPageHtmlSync(common.titleToFolder(post.title));
    var commentHtml = '';
    var inputComments = JSON.parse(fs.readFileSync('input/pages/' + common.titleToFolder(post.title) + '/comments.json', 'utf8'));
    
    if( i < 10 && new Date(post.date) < new Date()) {
      feed.item({
          title:  post.title,
          url: 'http://codeofrob.com/entries/' + common.titleToPage(post.title),
          description: inputHtml.replace(/\<!\[CDATA\[/gm, '').replace(/\]\]\>/gm, ''), // replace(/<(?:.|\n)*?>/gm, '').substr(0, 512) + '...',
          date: post.date
      });
    }
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
    console.log('Entry written', outputFilename)
  }
  
  var rssFeed = feed.xml();
  fs.writeFileSync('site/rss.xml', rssFeed, 'utf8');
  console.log('RSS written')
};

common.getAllPostsInfo(function(posts) {
  posts = _.chain(posts)
      .sortBy(function(item) { return -(new Date(item.date)); })
      .filter(function(item) { return new Date(item.date) < new Date()})
      .value()

  var indexTemplate = fs.readFileSync('./input/source/indextemplate.html', 'utf8');
  var listHtml = '';
  for(var i = 0 ; i < posts.length; i++) {
    var post = posts[i];
    listHtml += '<li><a href="entries/' + common.titleToPage(post.title) + '">' + post.title + '</a></li>\n';
  }
  
  // In essence doesn't do anything any more, but maybe in the future
  var indexHtml = plates.bind(indexTemplate, { entrylist: listHtml });
  fs.writeFileSync('./site/index.html', indexHtml, 'utf8');
  compilePosts(posts);
  process.exit()
});
