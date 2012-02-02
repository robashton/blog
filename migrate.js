var InitialDataPass = require('./initialdatapass').InitialDataPass;
var fs = require('fs');
var wrench = require('wrench');
var common = require('./common');

performPassOne = function(callback) {
  wrench.rmdirSyncRecursive('./input/pages/', true);
  fs.mkdirSync('./input/pages', '777');
  var passone = new InitialDataPass();
  passone.on('finished', function() {
    
    var allPosts = passone.getPosts();
    
    for(var i = 0 ; i < allPosts.length; i++) {
      var post = allPosts[i];
      console.log(post.title);
      var directory = './input/pages/' + common.titleToFolder(post.title);
      fs.mkdirSync(directory, '777');
      fs.writeFileSync(directory + '/meta.json', JSON.stringify({
        title: post.title,
        date: post.date,
        redirect: post.link
      }));
      fs.writeFileSync(directory + '/content.html', post.content);
      fs.writeFileSync(directory + '/comments.json', JSON.stringify(post.comments));
    }
    callback(allPosts);   
  });
  passone.on('post', function(post) {
  
  });
  passone.run();
};

performPassOne(function(posts) {
  console.log('Migrated');
});
