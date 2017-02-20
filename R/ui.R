require(shiny)
require(shinythemes)
require(sunburstR)

fluidPage(theme = "bootstrap.css",
          titlePanel("LastFM Visualizer"),
          sidebarLayout(
            sidebarPanel(
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
                "Artist/Album/Track:",
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
            mainPanel(sunburstOutput("sunburst"))
          ))
