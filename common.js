module.exports = {
  titleToPage: function(title) {
    return title.toLowerCase()
            .replace(/ /g, '-')
            .replace(/[\'#\(\)]/g, '')
            .replace('/', '-')
            + '.html';
  },
  titleToFolder: function(title) {
    return title.replace('/', '-');
  }
};
