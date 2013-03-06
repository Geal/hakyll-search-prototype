window.onload = function() {
  var input = $('#search')
  var submit = $('#submit')
  var urls = []

  $.ajax({
    type: 'GET',
    url: '/urls.json',
    dataType: 'json',
    timeout: 2000,
    success: function(data){
      console.log(data)
      urls = data
    },
    error: function(xhr, type){
      console.log('Ajax error!')
    }
  })

  function search() {
    $('#results').text('')
    console.log(input.val())
    var searchWord = input.val().toLowerCase()
    $.ajax({
      type: 'GET',
      url: '/search.json',
      dataType: 'json',
      timeout: 2000,
      success: function(data){
        console.log(data[searchWord])
        var indexes = data[searchWord]
        var results = $('#results')
        $.map(indexes, function(item, index) {
          var url = urls[item]
          results.append("<li><a href=\""+url+"\">"+url+"</a></li>")
        })
      },
      error: function(xhr, type){
        console.log('Ajax error!')
      }
    })
  }
  submit.on("click", search)
  input.on("keypress", function(e){
    if(e && e.keyCode == 13) {
      search()
    }
  })
}
