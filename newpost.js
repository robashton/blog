var argv = require('optimist').argv
  , fs = require('fs')
  , wrench = require('wrench')

var title = argv.title
wrench.copyDirSyncRecursive('input/source/newpost', 'input/pages/' + title);

var meta = JSON.parse(fs.readFileSync('input/pages/' + title + '/meta.json', 'utf8'))
meta.title = title

fs.writeFileSync('input/pages/' + title + '/meta.json', JSON.stringify(meta), 'utf8')




