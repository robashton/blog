var fs = require('fs');
var path = require('path');
var wrench = require('wrench');
var _ = require('underscore');
var plates = require('plates');
var common = require('./common');
var RSS = require('rss');
var markdown = require('marked')
var highlighter = require('highlight.js')

markdown.setOptions({
  gfm: true,
  highlight: function(code, lang) {
    if(lang)
      return highlighter.highlight(code, { language: lang }).value
    else
      return highlighter.highlightAuto(code).value
  }
});

console.log('Rebuilding site')

function compileStatics(cb) {
  var statictemplate = fs.readFileSync('./input/source/statictemplate.html', 'utf8');
  fs.readdir('input/static', function(err, files) {
    for(var i = 0; i < files.length; i++) {
      var file = files[i]
      var fullpath = path.join('input/static', files[i])
      var htmlfilename = path.join('site', file)
      var html = fs.readFileSync(fullpath, 'utf8');
      var statichtml = plates.bind(statictemplate, { body: html });
      fs.writeFileSync(htmlfilename, statichtml , 'utf8');
    }
    cb()
  })
}

var compilePosts = function(posts) {

  var feed = new RSS({
    title: 'Rob Ashton\'s blog',
    description: 'Software development dumping ground',
    feed_url: 'http://feeds.feedburner.com/robashton',
    site_url: 'http://codeofrob.com',
    image_url: 'http://codeofrob.com/img/cover.jpg',
    author: 'Rob Ashton'
  });

  var now = new Date()

  var readPageHtmlSync = function(folder) {
    var mdfilename = 'input/pages/' + folder + '/content.markdown';
    var htmlfilename = 'input/pages/' + folder + '/content.html';
    var mdstat = fs.existsSync(mdfilename);
    if(mdstat) {
      var mdcontent = fs.readFileSync(mdfilename, 'utf8');
      var html = markdown(mdcontent);
      fs.writeFileSync(htmlfilename, html , 'utf8');
    }
    var inputHtml = fs.readFileSync(htmlfilename, 'utf8');
    return inputHtml;
  };

  var pageTemplate = fs.readFileSync('input/source/pagetemplate.html', 'utf8');
  var rssItems = 0;
  for(var i = 0 ; i < posts.length; i++) {
    var post = posts[i];
    var outputFilename = 'site/entries/' + common.titleToPage(post.title);
    var inputHtml = readPageHtmlSync(common.titleToFolder(post.title));
    var commentHtml = '';
    var inputComments = JSON.parse(fs.readFileSync('input/pages/' + common.titleToFolder(post.title) + '/comments.json', 'utf8'));

    if( rssItems < 10 && post.date < now) {
      rssItems++
      feed.item({
          title:  post.title,
          url: 'http://codeofrob.com/entries/' + common.titleToPage(post.title),
          description: inputHtml.replace(/\<!\[CDATA\[/gm, '').replace(/\]\]\>/gm, ''),
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

    var datestr = ''
    datestr += post.date.getFullYear() + '-'
    datestr += (post.date.getMonth() + 1) + '-'
    datestr += post.date.getDate()

    var pageHtml = plates.bind(pageTemplate, { post: inputHtml, title: post.title, "post-title": post.title,  date: datestr, "post-comments": commentHtml });
    fs.writeFileSync(outputFilename, pageHtml, 'utf8');
    console.log('Entry written', outputFilename)
  }

  var rssFeed = feed.xml();
  fs.writeFileSync('site/rss.xml', rssFeed, 'utf8');
  console.log('RSS written')
};

common.getAllPostsInfo(function(posts) {
  var newposts = _.chain(posts)
      .sortBy(function(item) { return item.date; })
      .reverse()
      .value()

  var indexTemplate = fs.readFileSync('./input/source/indextemplate.html', 'utf8');
  var unpublishedposts = 0
  var listHtml = '';
  var now = new Date()

  var allEntries = ''
  ,  found = 0
  for(var i = 0 ; i < newposts.length; i++) {
    var post = newposts[i]
    if(post.date < now) {
      if(found++ < 10)
        listHtml += '<li><a href="entries/' + common.titleToPage(post.title) + '">' + post.title + '</a></li>\n';
    } else
      unpublishedposts++

    allEntries += post.date + ': http://codeofrob.com/entries/' + common.titleToPage(post.title) + '\r\n'
  }

  // In essence doesn't do anything any more, but maybe in the future
  var indexHtml = plates.bind(indexTemplate, { entrylist: listHtml, unpublishedposts: unpublishedposts || "zero"});
  fs.writeFileSync('./site/blog.html', indexHtml, 'utf8');
  fs.writeFileSync('./site/allposts.txt', allEntries, 'utf8')
  compilePosts(newposts);
  compileStatics(function() {
    process.exit()
  })
});
