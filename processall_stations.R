
# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("tidyverse","ggtext","htmlwidgets","janitor",
              "plotly")

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Functions --------------------------------------------------------------

source("importCSdata.r")

#rmarkdown::find_pandoc()
# Process r scripts

list.files(pattern = "process_",full.names = TRUE) %>% 
  #Process only a subset
  stringr::str_subset(., "MAT06|lake|inlet|waterplots", negate = TRUE) %>% 
  map(function(x) {
    print(x)
    source(x)
})

