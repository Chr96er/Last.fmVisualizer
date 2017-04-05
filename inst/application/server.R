#'Shiny server functions
#'see lastfm api: http://www.last.fm/api
#' Copy all imports to utilities.R since this file is not sourced during Build
#'@importFrom sunburstR sunburst renderSunburst
#'@importFrom jsonlite fromJSON
#'@import shiny
#'@import shinyUtils
#'@importFrom shinyjs runjs
library(shiny)
library(shinyUtils)
library(data.table)
library(Last.fmVisualizer)

VERSION = strsplit(gsub(".VERSION", replacement = "", dir()[grep(dir(), pattern = "VERSION")]), "\\.")[[1]]
spotifyBaseUrl <- "https://api.spotify.com/v1/"
lastfmBaseUrl <- "https://www.last.fm/de/music/"

#Temporary fix: variables for sunburst using hyphen as only split-character
hypenReplacement <- " " #Hyphens are replaced by this string
sunburstSeparator <- "-" #The separator used by sunburstR
concatSeparator <-
  ": " #For concatenating multiple elements in one level

function(input, output, session) {
  output$wd <- renderText({print(getwd())})
  
  output$version <- renderUI({
    list(
      tags$p(
        "Source code available under https://github.com/Chr96er/Last.fmVisualizer"
      ),
      tags$p(
        "Version ",
        paste(VERSION[1:3], collapse = "."),
        "; build ",
        VERSION[4],
        align = "right"
      )
    )
  })
  
  output$manual <- renderUI({
    manual(
      "The lastfmVisualizer application plots a user's top tags, artists, albums, and tracks as sunburst charts, using the last.fm API. Insert a last.fm user name or select a friend to get started."
    )
  })
  
  output$head <- renderUI({
    list(
      htmlStyle(),
      tags$link(href = "context/context.bootstrap.css"),
      tags$script(src = "context/context.js"),
      tags$script(
        "
        $(function(){
        $('body').on('contextmenu', 'path', function(){
        if(typeof contextTrigger == 'undefined'){
        contextTrigger = true;
        }
        contextTrigger = !contextTrigger;
        linkID = $(this).index();
        });
        });
        context.init({
        fadeSpeed: 100,
        filter: function($obj){},
        above: 'auto',
        preventDoubleContext: true,
        compress: false
        });
        context.attach('path', [{text: 'Open in Spotify', action: function(e){
        Shiny.onInputChange('contextmenu', linkID + '_' + 'spotify' + '_' + contextTrigger);
        }},
        {text: 'Open in last.FM', action: function(e){
        Shiny.onInputChange('contextmenu', linkID + '_' + 'lastfm' + '_' + contextTrigger);
        }}]);
        "
      ),
      HTML(
        '<script type="text/javascript" src="//code.jquery.com/ui/1.11.1/jquery-ui.js">'
      )
    )
    })
  
  getUsername <- reactive({
    #Make sure all required variables exist, else exit function
    req(input$username)
    input$username
  })
  
  sunburstDataset <- reactive({
    #Make sure all required variables exist, else exit function
    req(input$element,
        input$period,
        input$limit)
    
    #variables for lastfm api
    element <- switch(
      input$element,
      "Album" = "topalbums",
      "Artist" = "topartists",
      "Track" = "toptracks",
      "Artist-Album" = "topalbums",
      "Artist-Track" = "toptracks",
      "Tag-Artist-Album-Track" = "toptracks"
    )
    
    period <- switch(
      input$period,
      "Week" = "7day",
      "Month" = "1month",
      "3 Months" = "3month",
      "6 Months" = "6month",
      "Year" = "12month",
      "Overall" = "overall"
    )
    
    username <- getUsername()
    limit <- input$limit
    
    JSONString <-
      buildJSONString(
        method = "user",
        element = element,
        username = username,
        limit = limit,
        period = period
      )
    
    #get json from lastfm and parse to object
    withProgress({
      JSONObject <-
        jsonlite::fromJSON(JSONString)
    }, style = "old", message = "Loading...")
    
    #Make sure JSONString is valid by parsing jsonstring, which should contain a message in case of an error
    shiny::validate(need(is.null(JSONObject$error), message = JSONObject$message))
    
    jsonDataset <- switch(
      element,
      "topalbums" = JSONObject$topalbums$album,
      "topartists" = JSONObject$topartists$artist,
      "toptracks" = JSONObject$toptracks$track
    )
    
    #hyphens need to be replaced right now, until sunshine accepts a parameter for splitting levels
    name <- gsub(jsonDataset$name,
                 pattern = "-",
                 replacement = hypenReplacement)
    
    playcount <- as.numeric(jsonDataset$playcount)
    
    #Artistname does not necessarily exist
    artistName = NULL
    if (!is.null(jsonDataset$artist$name)) {
      artistName = gsub(jsonDataset$artist$name,
                        pattern = "-",
                        replacement = hypenReplacement)
    }
    
    #Create dataset based on user input
    sunburstDataset <- switch(
      input$element,
      "Album" = data.frame(name,
                           playcount, stringsAsFactors = F),
      "Artist" = data.frame(name,
                            playcount, stringsAsFactors = F),
      "Track" = data.frame(name,
                           playcount, stringsAsFactors = F),
      "Artist-Album" = data.frame(artistName,
                                  name,
                                  playcount, stringsAsFactors = F),
      "Artist-Track" = data.frame(
        artistName,
        name,
        mbid = jsonDataset$mbid,
        playcount,
        stringsAsFactors = F
      ),
      "Tag-Artist-Album-Track" = data.frame(
        artistName,
        name,
        mbid = jsonDataset$mbid,
        playcount,
        stringsAsFactors = F
      )
    )
    return(sunburstDataset)
  })
  
  sunburstRenderDataset <- reactive({
    sunburstDataset <- sunburstDataset()
    if (grepl("Tag", input$element)) {
      sunburstDataset$album = ""
      sunburstDataset$tag = ""
      
      #If tags are needed, a request to the lastfm api is required for each track
      #This may take a while, so a progress bar is set up
      withProgress({
        for (i in 1:nrow(sunburstDataset)) {
          incProgress(1,
                      message = paste0(
                        "Fetching result ",
                        i,
                        " of ",
                        nrow(sunburstDataset),
                        " results."
                      ))
          if (sunburstDataset[i, ]$mbid != "") {
            #Use mbid to look up track
            JSONString <-
              buildJSONString(
                method = "track",
                element = "info",
                mbid = jsonDataset[i, ]$mbid
              )
          } else{
            #mbid is missing -> use artist and track for lookup
            JSONString <-
              buildJSONString(
                method = "track",
                element = "info",
                artist = jsonDataset[i, ]$artist$name,
                track = jsonDataset[i, ]$name
              )
            #replace space in artist/track name for lastfm request
            JSONString <-
              gsub(JSONString,
                   pattern = " ",
                   replacement = "+")
          }
          
          JSONObject <- jsonlite::fromJSON(JSONString)
          
          #set album to unknown by default since it may be missing in lastfms json answer
          sunburstDataset[i, "album"] = "unknown"
          if (is.null(JSONObject$error) &&
              !is.null(JSONObject$track$album$title[1])) {
            sunburstDataset[i, "album"] = gsub(
              JSONObject$track$album$title[1],
              pattern = "-",
              replacement = hypenReplacement
            )
          }
          
          #set tag to unknown by default since it may be missing in lastfms json answer
          sunburstDataset[i, "tag"] = "unknown"
          if (is.null(JSONObject$error) &&
              !is.null(JSONObject$track$toptags$tag$name[1])) {
            sunburstDataset[i, "tag"] = gsub(
              JSONObject$track$toptags$tag$name[1],
              pattern = "-",
              replacement = hypenReplacement
            )
          }
        }
      }, min = 1, max = nrow(sunburstDataset))
      sunburstDataset <-
        sunburstDataset[, c('tag', 'artistName', 'album', 'name', 'playcount')]
    }
    
    sunburstDataset$mbid = NULL
    sunburstDataset <- as.matrix(sunburstDataset)
    
    concVec = c()
    if (ncol(sunburstDataset) > 2) {
      concVec <-
        apply(sunburstDataset[, 1:(ncol(sunburstDataset) - 1)], 1, function(x) {
          paste(x, collapse = sunburstSeparator)
        })
    } else{
      concVec <- sunburstDataset[, 1]
    }
    
    sunburstDataset <- data.frame(concVec,
                                  sunburstDataset[, ncol(sunburstDataset)])
    
    return(sunburstDataset = sunburstDataset)
  })
  
  #'Function to get friends for currently selected user
  friendsDataset <- reactive({
    withProgress({
      username <- getUsername()
      
      JSONString <-
        buildJSONString(
          method = "user",
          element = "friends",
          username = username,
          limit = 10
        )
      
      JSONObject <- jsonlite::fromJSON(JSONString)
      #Make sure JSONString is valid by parsing jsonstring, which should contain a message in case of an error
      shiny::validate(need(is.null(JSONObject$error), message = JSONObject$message))
      friendsDataset <- JSONObject$friends$user$name
      return(friendsDataset = friendsDataset)
    }, value = NULL, message = "Loading charts")
  })
  
  
  output$sunburst <- sunburstR::renderSunburst({
    #todo: test whether sunburstdataset has been properly parsed
    sunburstDataset <- sunburstRenderDataset()
    shiny::validate(need(!is.null(sunburstDataset), message = "Error in code!"))
    shiny::validate(
      need(nrow(sunburstDataset) != 0, message = "There is no data available for this user with the selected filters")
    )
    sunburstR::sunburst(sunburstDataset,
                        count = T,
                        legend = c(w = 200))
  })
  
  
  output$friends <- renderUI({
    withProgress({
      friendsDataset <- friendsDataset()
      rows <- lapply(friendsDataset, function(x) {
        list(actionLink(x,
                        x,
                        class = "friendlink"), ",")
      })
      rows[[length(rows)]][[2]] <- NULL
      do.call(shiny::tagList,
              list(
                tags$label("View friend's charts:"),
                tags$br(),
                rows,
                tags$p(),
                # javascript code to set username input field to selected friend upon clicking it
                tags$script(
                  HTML(
                    'var all = $(".friendlink");
                    for(var i = 0, max = all.length; i < max; i++)
                    {
                    var el = all[i];
                    el.onclick = function(){
                    Shiny.onInputChange("friend", this.id);
                    }
                    }
                    '
                  )
                  )
                  ))
  }, value = NULL, message = "Loading friend list")
    })
  
  observeEvent(input$contextmenu, {
    contextmenu <- input$contextmenu
    if (is.null(contextmenu)) {
      return(NULL)
    }
    contextmenu <- strsplit(contextmenu, "_")[[1]]
    linkID <- as.numeric(contextmenu[1]) - 1
    
    sunburstDataset <- as.data.table(sunburstDataset())
    #Order by artist count
    sunburstDataset <- sunburstDataset[, .(name), by = artistName]
    unfolded <- c()
    for (i in 1:nrow(unique(sunburstDataset, by = "artistName"))) {
      currentArtist <-
        unique(sunburstDataset, by = "artistName")[i, artistName]
      unfolded <-
        c(unfolded, currentArtist, sunburstDataset[artistName == currentArtist, name])
    }
    
    clicked <- unfolded[linkID]
    if (sunburstDataset[artistName == clicked, .N]) {
      #Artist clicked - get artist's tracks
      # tracks <- sunburstDataset[artistName == clicked, name] #ToDo: Get all child tracks and pass to spotify
      tracks <- NULL
      artist <- clicked
    } else {
      #Track clicked - get artist
      tracks <- clicked
      artist <- sunburstDataset[name == clicked, artistName]
      
    }
    switch(
      contextmenu[2],
      "spotify" = openInSpotify(artist, tracks),
      "lastfm" = openInLastfm(artist, tracks)
    )
  })
  
  openInSpotify <- function(artist, tracks) {
    if (is.null(tracks)) {
      url <-
        paste0(spotifyBaseUrl,
               "search?q=",
               "artist:",
               artist,
               "&type=artist")
      url <- gsub(" ", "+", url)
      JSONObject <- jsonlite::fromJSON(url)
      # uri <- JSONObject$tracks$items$u[1:length(tracks)]
      urls <- JSONObject$artist$items$external_urls[1, ]
    } else {
      urls <- sapply(tracks, function(x) {
        url <-
          paste0(
            spotifyBaseUrl,
            "search?q=",
            "track:",
            x,
            "%20artist:",
            artist,
            "&type=track"
          )
        url <- gsub(" ", "+", url)
        JSONObject <- jsonlite::fromJSON(url)
        # uri <- JSONObject$tracks$items$u[1:length(tracks)]
        return(JSONObject$tracks$items$external_urls[1, ])
      })
    }
    
    extUrl <- urls[1] #ToDo: Multiple tracks, currently only first
    openLink(extUrl, "modal")
  }
  
  openInLastfm <- function(artist, tracks) {
    track <- tracks[1]
    if (is.null(track)) {
      url <- paste0(lastfmBaseUrl,
                    artist)
    } else {
      url <- paste0(lastfmBaseUrl,
                    artist,
                    "/_/",
                    track)
    }
    url <- gsub(" ", "+", url)
    openLink(url, "tab") #Can't display last.fm in modal/iframe
  }
  
  output$username <- renderUI({
    if (is.null(input$friend)) {
      textInput("username",
                "last.fm Username:",
                value = "",
                placeholder = "e.g. Chr_96er")
    }
    else{
      textInput("username", "last.fm Username:", input$friend)
    }
  })
  }
