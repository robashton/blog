var argv = require('optimist').argv
  , fs = require('fs')
  , wrench = require('wrench')
  , common = require('./common')
  , _ = require('underscore')

  var now = new Date()

var title = argv.title
  , year = argv.year || now.getUTCFullYear()
  , month = argv.month || now.getUTCMonth()
  , day = argv.day || null
  , date = null

common.getAllPostsInfo(function(allposts) {
  allposts = _.sort(allposts, function(post) { return -(new Date(post.date)) })

  var newestpost = _.max(allposts, function(item) { return new Date(item.date) })
  var latestdate = new Date(newestpost.date)
  var newdate = addDayExcludingWeekends(latestdate)

  if(day) {
    date = new Date(year, month, day, 11, 0, 0)
  } else {
    date = new Date(newdate.getUTCFullYear(),
                       newdate.getUTCMonth(),
                       newdate.getUTCDate(),
                       11, 0, 0);
  }

  var lastdate = date
  if(date < latestdate) {
    console.log('Shifting posts to the future')
    for(var i = 0; i < allposts.length; i++) {
      var post = allposts[i]
      var postdate = new Date(post.date)
      if(postdate < lastdate) {
        postdate = addDayExcludingWeekends(postdate)
        lastdate = postdate
        post.date = postdate
        console.log(post.title, ' will now be published on ', post.date)
      }
    }
  }
})

function addDayExcludingWeekends(date) {
  var newdate = null
  do {
    newdate = new Date(date.getTime() + (24 * 60 * 60 * 1000))
  } while ( newdate.getUTCDay() === 0 || newdate.getUTCDay() === 6)
  return newdate
}


function createNewPost(date, title) {
  wrench.copyDirSyncRecursive('input/source/newpost', 'input/pages/' + title);

  var meta = JSON.parse(fs.readFileSync('input/pages/' + title + '/meta.json', 'utf8'))
  meta.title = title
  meta.date = date
  fs.writeFileSync('input/pages/' + title + '/meta.json', JSON.stringify(meta), 'utf8')
}
