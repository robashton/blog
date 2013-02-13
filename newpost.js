var argv = require('optimist').argv
  , fs = require('fs')
  , wrench = require('wrench')

var title = argv.title
wrench.copyDirSyncRecursive('input/source/newpost', 'input/pages/' + title);


