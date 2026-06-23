packages <- c("shiny","shinyFiles","shinyTime","shinyWidgets","behavr",
              "data.table","ggplot2","damr","zeitgebr","sleepr","ggetho",
              "DT","fs","dplyr","Hmisc","shinyjs","readr","colourpicker",
              "lattice","survival","Formula","stringr","DescTools",
              "graphics","doBy","zoo","tm","base","scales","shinythemes",
              "openxlsx","ActCR","tools",
              "nonlinearTseries","shinytitle","tidyverse")

for(pkg in packages) {
  cat("Loading:", pkg, "... ")
  result <- tryCatch({
    library(pkg, character.only = TRUE)
    cat("OK\n")
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
  })
}


R.version.string
sessionInfo()



options(rgl.useNULL = TRUE)


setwd("/Users/cchuang/Desktop/Rtivity_main")
options(shiny.error = function() { traceback(); stop() })
shiny::runApp()


# After setting up files but BEFORE clicking Start Analysis
# Run this in R console:
print(Conditions$df)
print(Directory())

# Check what the Conditions dataframe looks like
print(Conditions$df)

# Check the column names
names(Conditions$df)

# Check the directory
print(Directory())

# Restart the app
setwd("/Users/cchuang/Desktop/Rtivity_main")
options(rgl.useNULL = TRUE)
shiny::runApp()



