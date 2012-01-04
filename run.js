var InitialDataPass = require('./initialdatapass').InitialDataPass;
var CreateIndex = require('./createindex').CreateIndex;

performPassOne = function(callback) {
  var passone = new InitialDataPass();
  passone.on('finished', function() {
    console.log('Found all posts, ordering');
    var posts = passone.getPosts();
    callback(posts);    
  });
  passone.on('post', function(post) {
    console.log('Post found');
  });
  passone.run();
};

performPassOne(function(posts) {
  var createIndex = new CreateIndex(posts);
  createIndex.on('finished', function() {
    createIndex.saveTo('input/index.json');
    console.log('Index created');
  }); 
  
  
  createIndex.run();
});
