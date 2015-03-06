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
      return highlighter.highlight(lang, code).value
    else
      return highlighter.highlightAuto(code).value
  }
});


function dohtml(in, out) {
  var html = fs.readFileSync(in, 'utf8');
  var statichtml = plates.bind(statictemplate, { body: html });
  fs.writeFileSync(out, statichtml , 'utf8');
}


function domarkdown(in, out) {

}
