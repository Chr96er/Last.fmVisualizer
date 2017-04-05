#'Shiny ui
#' Copy all imports to utilities.R since this file is not sourced during Build
#'@import shiny
#'@importFrom sunburstR sunburstOutput
#'@importFrom shinyjs runjs
library(shiny)

fluidPage(
  shinyjs::useShinyjs(),
  theme = "bootstrap.css",
  titlePanel("lastfmVisualizer"),
  uiOutput("head"),
  uiOutput("manual"),
  sidebarLayout(
    sidebarPanel(
      textOutput("wd"),
      uiOutput("username"),
      uiOutput("friends"),
      # Only show this panel if long lasting operations expected
      conditionalPanel(
        condition = "input.element=='Tag-Artist-Album-Track'",
        helpText(
          "Warning: The Tag-Artist-Album-Track method may be slow and take a while depending on the selected limit!"
        )
      ),
      radioButtons(
        "element",
        "Tag/Artist/Album/Track:",
        choices = c(
          "Artist",
          "Artist-Album",
          "Artist-Track",
          "Tag-Artist-Album-Track",
          "Album",
          "Track"
        ),
        selected = "Artist-Track"
      ),
      radioButtons(
        "period",
        "Period:",
        choices = c("Week", "Month", "3 Months", "6 Months", "Year", "Overall"),
        selected = "3 Months"
      ),
      numericInput(
        "limit",
        "Result limit:",
        min = 1,
        max = 1000,
        value = 5
      )
    ),
    mainPanel(sunburstR::sunburstOutput("sunburst"))
  ),
  absolutePanel(uiOutput("version"), bottom = 0, right = 10)
)
