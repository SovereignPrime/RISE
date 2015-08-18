NitrogenClass.prototype.$tagsinput = function(path, tagsinputOptions, selectPostbackInfo) {
    var n = this;
    jQuery.extend(tagsinputOptions, {
        source: function(query) {
            console.log(query);
           n.$queue_event(null, null, selectPostbackInfo, "search_term="+n.$urlencode(query), {
              dataType: 'json',

              success: function(data) {
                  console.log(data);
                 return data;
              }
          });
        }
    });
    jQuery(path).tagsinput(tagsinputOptions);
}
