$(function() {
  $('.collapse')
  .each(function() {
    var $div = $(this)
    $div.hide()
    $div.before(
      $("<div/>")
      .addClass("toggle-collapse")
      .text("Click to show...")
      .click(function() {
         $(this).hide()
         $div.show()
      }))
  })
})
