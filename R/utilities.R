source("apiKey.R") #contains one line to set lastfm apiKey like so: apiKey <-"01234567890123456789012345678901"

buildJSONString <-
  function(method,
           element,
           username = "",
           limit = 0,
           mbid = "",
           artist = "",
           track = "",
           period = "") {
    paste(
      "http://ws.audioscrobbler.com/2.0/?method=",
      method,
      ".get",
      element,
      if (username != "") {
        paste0("&user=", username)
      },
      "&api_key=",
      apiKey,
      "&format=json",
      if (period != "") {
        paste0("&period=", period)
      },
      if (limit != 0) {
        paste0("&limit=", limit)
      },
      if (mbid != "") {
        paste0("&mbid=", mbid)
      },
      if (artist != "") {
        paste0("&artist=", artist)
      },
      if (track != "") {
        paste0("&track=", track)
      },
      sep = ""
    )
  }