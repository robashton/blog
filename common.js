var fs = require('fs');

module.exports = {
  titleToPage: function(title) {
    return title.toLowerCase()
            .replace(/ /g, '-')
            .replace(/[\'#\(\)]/g, '')
            .replace('/', '-')
            .replace('?', '')
            + '.html';
  },
  titleToFolder: function(title) {
    return title.replace('/', '-');
  },
  getAllPostsInfo:  function(callback) {
    var folders = fs.readdirSync('./input/pages/');
    var metaCollection = [];
    for(var i in folders) {
      var folder = './input/pages/' + folders[i];
      fs.readFile(folder + '/meta.json', function(err, data) {
        var meta = JSON.parse(data);
        meta.date = new Date(meta.date)
        metaCollection.push(meta);
        if(metaCollection.length === folders.length)
          callback(metaCollection);
      });
    }
  },
  textToHtml: function(text) {
    return text.replace('\t', ' ').replace('<', '&lt;').replace('>', '&rt;').replace('\n', '<br/>');
  }
};
