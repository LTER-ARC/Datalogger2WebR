
# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","htmlwidgets","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse","rstudioapi")

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Check if script is running on the website. If so setwd else use the project wd

web_logger_dir <- "/www/arcdeims7/sites/default/files/data/datalogger"
if (dir.exists(web_logger_dir)) {
  setwd(web_logger_dir)
}

# Functions --------------------------------------------------------------

source("importCSdata.r")


# Process r scripts

list.files(pattern = "process_MAT06",full.names = TRUE) %>% 
  map(function(x) {
    print(x)
    source(x)
})
