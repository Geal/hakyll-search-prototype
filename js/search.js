$.ajax({
  type: 'GET',
  url: '/search.json',
  // type of data we are expecting in return:
  dataType: 'json',
  timeout: 300,
  context: $('#results'),
  success: function(data){
    // Supposing this JSON payload was received:
    //   {"project": {"id": 42, "html": "<div>..." }}
    // append the HTML to context object.
    console.log(data)
    this.append(data)
  },
  error: function(xhr, type){
    console.log('Ajax error!')
  }
})
