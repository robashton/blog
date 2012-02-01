module.exports = {
  titleToPage: function(title) {
    return title.toLowerCase()
            .replace(/ /g, '-')
            .replace(/[\'#\(\)]/g, '') 
            + '.htm';
  }
};
