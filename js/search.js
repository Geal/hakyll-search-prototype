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
    var searchWords = input.val().toLowerCase().split(" ")
    console.log(searchWords)
    $.ajax({
      type: 'GET',
      url: '/search.json',
      dataType: 'json',
      timeout: 2000,
      success: function(data){
        var res = $.map(searchWords, function(word, index) {
          return data[word]
        }).sort()

        for(var i = 1; i < res.length; i++) {
          if(res[i] === res[i-1]) {
            res.splice(i--, 1)
          }
        }
        var indexes = res
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
