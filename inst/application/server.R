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
  output$version <- renderUI({
    renderVersion(url = "https://github.com/Chr96er/Last.fmVisualizer")
  })
  
  output$manual <- renderUI({
    manual(
      "The lastfmVisualizer application plots a user's top tags, artists, albums, and tracks as sunburst charts, using the last.fm API. Insert a last.fm user name or select a friend to get started."
    )
  })
  
  renderContextMenu <- reactive({
    input$element %in% c("Artist-Track", "Artist-Album", "Artist")
  })
  
  output$head <- renderUI({
    if (is.null(input$element)) {
      return(NULL)
    }
    list(
      htmlStyle(),
      tags$link(href = "context/context.bootstrap.css"),
      tags$script(src = "context/context.js"),
      tags$script(src = "//code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$script(HTML(
        ifelse(
          renderContextMenu(),
          paste0(
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
            
            context.attach('path', [{text: 'Open in Spotify ', innerHTML: '",
            paste0(
              insertIcon(
                src = "open_in_browser.png",
                id = "open_in_browser",
                class = "contextMenuIcons"
              )
            ),
            "', action: function(e){
            Shiny.onInputChange('contextmenu', linkID + '_' + 'spotify' + '_' + contextTrigger);
            }},
            {text: 'Open in last.FM ', innerHTML: '",
            paste0(
              insertIcon(
                src = "open_in_new.png",
                id = "open_in_new",
                class = "contextMenuIcons"
              )
            ),
            "', action: function(e){
            Shiny.onInputChange('contextmenu', linkID + '_' + 'lastfm' + '_' + contextTrigger);
            }}]);
            "
      ),
      "context.destroy('path')"
    )
    ))
    )
            })
  getUsername <- debounce(reactive({
    #Make sure all required variables exist, else exit function
    req(input$username)
    input$username
  }), 1000)
  
  sunburstDataset <- reactive({
    #Make sure all required variables exist, else exit function
    req(input$element,
        input$period,
        input$limit)
    
    #variables for lastfm api
    element <- lookupElement(input$element)
    
    period <- lookupPeriod(input$period)
    
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
    
    shiny::validate(need(!is.null(nrow(jsonDataset)), message = "No data for user available"))
    
    #hyphens need to be replaced right now, until sunshine accepts a parameter for splitting levels
    name <- gsub(jsonDataset$name,
                 pattern = "-",
                 replacement = hypenReplacement)
    
    playcount <- as.numeric(jsonDataset$playcount)
    
    #Artistname does not necessarily exist
    artistName = NULL
    if (!is.null(jsonDataset$artist$name))
    {
      artistName = gsub(jsonDataset$artist$name,
                        pattern = "-",
                        replacement = hypenReplacement)
    }
    
    #Create dataset based on user input
    sunburstDataset <- switch(
      input$element,
      "Album" = data.table(name,
                           playcount),
      "Artist" = data.table(artistName = name,
                            playcount),
      "Track" = data.table(name,
                           playcount),
      "Artist-Album" = data.table(artistName,
                                  name,
                                  playcount),
      "Artist-Track" = data.table(artistName,
                                  name,
                                  playcount),
      "Tag-Artist-Album-Track" = data.table(artistName,
                                            name,
                                            playcount)
    )
    return(sunburstDataset)
  })
  
  sunburstRenderDataset <- reactive({
    sunburstDataset <- sunburstDataset()
    if (grepl("Tag", input$element))
    {
      sunburstDataset$album = ""
      sunburstDataset$tag = ""
      
      #If tags are needed, a request to the lastfm api is required for each track
      #This may take a while, so a progress bar is set up
      withProgress({
        for (i in 1:nrow(sunburstDataset))
        {
          incProgress(1,
                      message = paste0(
                        "Fetching result ",
                        i,
                        " of ",
                        nrow(sunburstDataset),
                        " results."
                      ))
          JSONString <-
            buildJSONString(
              method = "track",
              element = "info",
              artist = sunburstDataset[i, artistName],
              track = sunburstDataset[i, name]
            )
          #replace space in artist/track name for lastfm request
          JSONString <-
            gsub(JSONString,
                 pattern = " ",
                 replacement = "+")
          
          JSONObject <- jsonlite::fromJSON(JSONString)
          
          #set album to unknown by default since it may be missing in lastfms json answer
          sunburstDataset[i, "album"] = "unknown"
          if (is.null(JSONObject$error) &&
              !is.null(JSONObject$track$album$title[1]))
          {
            sunburstDataset[i, "album"] = gsub(
              JSONObject$track$album$title[1],
              pattern = "-",
              replacement = hypenReplacement
            )
          }
          
          #set tag to unknown by default since it may be missing in lastfms json answer
          sunburstDataset[i, "tag"] = "unknown"
          if (is.null(JSONObject$error) &&
              !is.null(JSONObject$track$toptags$tag$name[1]))
          {
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
    
    sunburstDataset <- as.matrix(sunburstDataset)
    
    concVec = c()
    if (ncol(sunburstDataset) > 2)
    {
      concVec <-
        apply(sunburstDataset[, 1:(ncol(sunburstDataset) - 1)], 1, function(x)
        {
          paste(x, collapse = sunburstSeparator)
        })
    } else
    {
      concVec <- sunburstDataset[, 1]
    }
    
    sunburstDataset <- data.table(concVec,
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
      shiny::validate(need(
        !is.null(friendsDataset),
        "This user has not last.fm friends"
      ))
      rows <- lapply(friendsDataset, function(x)
      {
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
  
  observeEvent(input$contextmenu,
               {
                 contextmenu <- input$contextmenu
                 if (is.null(contextmenu) ||
                     !input$element %in% c("Artist-Track", "Artist-Album", "Artist"))
                 {
                   return(NULL)
                 }
                 type <- tolower(gsub("Artist-", "", input$element))
                 contextmenu <- strsplit(contextmenu, "_")[[1]]
                 linkID <- as.numeric(contextmenu[1]) - 1
                 
                 sunburstDataset <- sunburstDataset()
                 #Order by artist count
                 if (input$element == "Artist") {
                   unfolded <- sunburstDataset[, artistName]
                 } else{
                   sunburstDataset <-
                     sunburstDataset[, .(name), by = artistName]
                   unfolded <- c()
                   for (i in 1:nrow(unique(sunburstDataset, by = "artistName"))) {
                     currentArtist <-
                       unique(sunburstDataset, by = "artistName")[i, artistName]
                     unfolded <-
                       c(unfolded, currentArtist, sunburstDataset[artistName == currentArtist, name])
                   }
                 }
                 
                 clicked <- unfolded[linkID]
                 if (sunburstDataset[artistName == clicked, .N]) {
                   #Artist clicked - ToDo: get artist's tracks/albums
                   # tracks/albums <- sunburstDataset[artistName == clicked, name] #ToDo: Get all child tracks/albums and pass to spotify
                   tracks_albums <- NULL
                   artist <- clicked
                 } else {
                   #Track clicked - get artist
                   tracks_albums <- clicked
                   artist <-
                     sunburstDataset[name == clicked, artistName]
                 }
                 switch(
                   contextmenu[2],
                   "spotify" = openInSpotify(artist, tracks_albums, type = type),
                   "lastfm" = openInLastfm(artist, tracks_albums)
                 )
               })
  
  openInSpotify <- function(artist, tracks_albums, type)
  {
    if (is.null(tracks_albums))
    {
      url <-
        paste0(spotifyBaseUrl,
               "search?q=",
               "artist:",
               artist,
               "&type=artist")
      url <- gsub(" ", "+", url)
      JSONObject <- jsonlite::fromJSON(url)
      if (length(JSONObject$artist$items) > 0) {
        extUrl <- JSONObject$artist$items$external_urls[1, ]
      }
    } else
    {
      url <-
        paste0(
          spotifyBaseUrl,
          "search?q=",
          type,
          ":",
          tracks_albums,
          "%20artist:",
          artist,
          "&type=",
          type
        )
      url <- gsub(" ", "+", url)
      JSONObject <- jsonlite::fromJSON(url)
      if (!is.null(
        c(
          JSONObject$tracks$items$external_urls[1],
          JSONObject$albums$items$external_urls[1]
        )
      )) {
        extUrl = max(
          JSONObject$tracks$items$external_urls[1, ],
          JSONObject$albums$items$external_urls[1, ]
        )
      } else {
        extUrl = NULL
      }
      
    }
    if (!is.null(extUrl)) {
      openLink(extUrl, "modal")
    }
  }
  
  openInLastfm <- function(artist, tracks_albums)
  {
    track_album <- tracks_albums[1]
    if (is.null(track_album))
    {
      url <- paste0(lastfmBaseUrl,
                    artist)
    } else
    {
      url <- paste0(lastfmBaseUrl,
                    artist,
                    "/_/",
                    track_album)
    }
    url <- gsub(" ", "+", url)
    openLink(url, "tab") #Can't display last.fm in modal/iframe
  }
  
  output$username <- renderUI({
    if (is.null(input$friend))
    {
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
