var argv = require('optimist').argv
  , fs = require('fs')
  , wrench = require('wrench')
  , common = require('./common')
  , _ = require('underscore')

  var now = new Date()

var title = argv.title
  , datestr = argv.date
  , force = argv.force
  , date = null

common.getAllPostsInfo(function(allposts) {
  allposts = _.sortBy(allposts, function(post) { return (new Date(post.date)) })

  var newestpost = _.max(allposts, function(item) { return new Date(item.date) })
  var latestdate = new Date(newestpost.date)

  if(datestr) {
    date = new Date([ datestr, ' 09:30:00 GMT'].join(''))
  } else {
    date = common.addDayExcludingWeekends(latestdate)
  }

  var lastdate = date
  if(date <= latestdate && !force) {
    console.log('Shifting posts to the future')
    for(var i = 0; i < allposts.length; i++) {
      var post = allposts[i]
      if(post.date >= lastdate) {
        post.date = common.addDayExcludingWeekends(lastdate)
        lastdate = post.date
        console.log(post.title, ' will now be published on ', post.date)
      }
      fs.writeFileSync('input/pages/' + common.titleToFolder(post.title) + '/meta.json', 
                      JSON.stringify(post), 'utf8')
    }
  }
  createNewPost(date, title)
})

function createNewPost(date, title) {
  wrench.copyDirSyncRecursive('input/source/newpost', 'input/pages/' + title);

  var meta = JSON.parse(fs.readFileSync('input/pages/' + title + '/meta.json', 'utf8'))
  meta.title = title
  meta.date = date
  fs.writeFileSync('input/pages/' + title + '/meta.json', JSON.stringify(meta), 'utf8')
}
