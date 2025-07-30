
# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","htmlwidgets","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse","rstudioapi")

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Functions --------------------------------------------------------------

source("importCSdata.r")


# Process r scripts

list.files(pattern = "process_",full.names = TRUE) %>% 
  stringr::str_subset(., "MAT06|lake|inlet", negate = TRUE) %>% 
  map(function(x) {
    print(x)
    source(x)
})
