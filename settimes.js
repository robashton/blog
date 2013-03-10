var common = require('./common')
 ,  _ = require('underscore')

common.getAllPostsInfo(function(allposts) {
  allposts = _.sortBy(allposts, function(post) { return (post.date) })
  var now = new Date()
  var nextExpectedDate = new Date(common.addDayExcludingWeekends(now))
  for(var i = 0; i < allposts.length; i++) {
    var post = allposts[i]
    if(post.date <= now) continue; 
    post.date = nextExpectedDate
    console.log(post.date, post.title)
    nextExpectedDate = common.addDayExcludingWeekends(nextExpectedDate)
    fs.writeFileSync('input/pages/' + common.titleToFolder(post.title) + '/meta.json', 
                    JSON.stringify(post), 'utf8') 
  }
})
