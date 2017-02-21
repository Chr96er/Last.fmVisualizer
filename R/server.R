#'Shiny server functions
#'see lastfm api: http://www.last.fm/api
require(shiny)
require(sunburstR)
require(jsonlite)
require(curl)
require(magrittr)

source("helpers.R")
source("apiKey.R") #contains one line to set lastfm apiKey like so: apiKey <-"01234567890123456789012345678901"

#Temporary fix: variables for sunburst using hyphen as only split-character
hypenReplacement <- " " #Hyphens are replaced by this string
sunburstSeparator <- "-" #The separator used by sunburstR
concatSeparator <-
  ": " #For concatenating multiple elements in one level

function(input, output, session) {
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
        apiKey = apiKey,
        method = "user",
        element = element,
        username = username,
        limit = limit,
        period = period
      )

    #get json from lastfm and parse to object
    withProgress({
      JSONObject <-
        fromJSON(JSONString)
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
                           playcount),
      "Artist" = data.frame(name,
                            playcount),
      "Track" = data.frame(name,
                           playcount),
      "Artist-Album" = data.frame(artistName,
                                  name,
                                  playcount),
      "Artist-Track" = data.frame(artistName,
                                  name,
                                  mbid = jsonDataset$mbid,
                                  playcount),
      "Tag-Artist-Album-Track" = data.frame(artistName,
                                            name,
                                            mbid = jsonDataset$mbid,
                                            playcount)
    )

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
                apiKey = apiKey,
                method = "track",
                element = "info",
                mbid = jsonDataset[i, ]$mbid
              )
          } else{
            #mbid is missing -> use artist and track for lookup
            JSONString <-
              buildJSONString(
                apiKey = apiKey,
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

          JSONObject <- fromJSON(JSONString)

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
          apiKey = apiKey,
          method = "user",
          element = "friends",
          username = username,
          limit = 10
        )

      JSONObject <- fromJSON(JSONString)
      #Make sure JSONString is valid by parsing jsonstring, which should contain a message in case of an error
      shiny::validate(need(is.null(JSONObject$error), message = JSONObject$message))
      friendsDataset <- JSONObject$friends$user$name
      return(friendsDataset = friendsDataset)
    }, value = NULL, message = "Loading charts")
  })


  output$sunburst <- renderSunburst({
    #todo: test whether sunburstdataset has been properly parsed
    sunburstDataset <- sunburstDataset()
    shiny::validate(need(!is.null(sunburstDataset), message = "Error in code!"))
    shiny::validate(
      need(nrow(sunburstDataset) != 0, message = "There is no data available for this user with the selected filters")
    )
    sunburst(sunburstDataset,
             count = T,
             legend = c(w = 200))
  })


  output$friends <- renderUI({
    withProgress({
      friendsDataset <- friendsDataset()
      rows <- lapply(friendsDataset, function(x) {
        list(actionLink(x,
                        x), ",")
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
                    'var all = document.getElementsByTagName("a");
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

  output$username <- renderUI({
    if (is.null(input$friend)) {
      textInput("username",
                "LastFM Username:",
                value = "",
                placeholder = "e.g. Chr_96er")
    }
    else{
      textInput("username", "LastFM Username:", input$friend)
    }
  })
}
