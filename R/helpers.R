require(RCurl)
#'Code by http://stackoverflow.com/a/32897076/4166885
serverReachable <- function(url) {
  # status <- tryCatch(
  #   getURL(url, ssl.verifypeer = FALSE, useragent = "R"),
  #   error = function(e)
  #     e
  # )
  # if (inherits(status, "error")) {
  #   return(F)
  # } else{
  return(T)
  # }
}

buildJSONString <-
  function(apiKey,
           method,
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