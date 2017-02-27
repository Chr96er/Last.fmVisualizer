library(shiny)

incVersion <-
  function(level = c("build", "minor", "major", "release"),
           build = T,
           version = F) {
    level = level[1]
    if (!version) {
      versionFile <- dir()[grep(dir(), pattern = "VERSION")]
      version <-
        strsplit(gsub(".VERSION", replacement = "", versionFile), "\\.")[[1]]
      if (build) {
        build = sample(1:99999, 1)
      }
      version = switch(
        level,
        build   = paste(version[1], version[2], version[3]    , build, sep = "."),
        minor   = paste(version[1], version[2], version[3] + 1, build, sep = "."),
        major   = paste(version[1], version[2] + 1, version[3], build, sep = "."),
        release = paste(version[1] + 1, version[2], version[3], build, sep = ".")
      )
    }
    file.rename(paste0(getwd(), "/", versionFile),
                paste0(getwd(), "/", version, ".VERSION"))
  }

manual <- function(text) {
  h4(text, style = "font-style: italic;
     font-weight: 100; line-height: 1;
     color: #888888;")
}

htmlStyle <- function(){
  tags$head(tags$style("html * {font-family: palanquin;
                                 }"
  )
  )
}