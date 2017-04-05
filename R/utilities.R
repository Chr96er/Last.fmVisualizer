#'@importFrom sunburstR sunburst renderSunburst
#'@importFrom jsonlite fromJSON
#'@import shiny
#'@import shinyUtils
#'@importFrom sunburstR sunburstOutput
#'@importFrom shinyjs runjs
#'@import data.table

#'@export
buildJSONString <-
  function(method,
           element,
           username = "",
           limit = 0,
           mbid = "",
           artist = "",
           track = "",
           period = "") {
    if (file.exists(paste0(getwd(), "/www/apiKeys/lastfm"))) {
      #inst/application/ is wd
      lastfmApiKey <-
        readLines(paste0(getwd(), "/www/apiKeys/lastfm")) #contains one line to set lastfm apiKey like so: lastfmApiKey <-"01234567890123456789012345678901"
    } else {
      #root is wd
      lastfmApiKey <-
        readLines(paste0(getwd(), "inst/application/www/apiKeys/lastfm")) #contains one line to set lastfm apiKey like so: lastfmApiKey <-"01234567890123456789012345678901"
    }
    paste(
      "http://ws.audioscrobbler.com/2.0/?method=",
      method,
      ".get",
      element,
      if (username != "") {
        paste0("&user=", username)
      },
      "&api_key=",
      lastfmApiKey,
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

#'@export
openLink <- function(url, type = "modal") {
  switch(type,
         "modal" = openLinkInModal(url),
         "tab" = openLinkInTab(url))
}

openLinkInModal <- function(url) {
  openLinkScript <- paste0(
    "var page = '",
    url,
    "';
    $('spotifymodal').dialog(\"close\");
    var $dialog = $('<div id =\"spotifymodal\"></div>').html('<iframe style=\"border: 0px; \" src=\"' + page + '\" width=\"100%\" height=\"100%\"></iframe>')
    .dialog({
    autoOpen: false,
    dialogClass: 'dialog_fixed,ui-widget-header',
    modal: true,
    height: 500,
    minWidth: 400,
    minHeight: 400,
    draggable:true
    });
    $dialog.dialog('open');"
    )
  shinyjs::runjs(openLinkScript)
}

openLinkInTab <- function(url) {
  shinyjs::runjs(paste0("var win = window.open('",
                        url,
                        "', '_blank');
                        win.focus();"))
}