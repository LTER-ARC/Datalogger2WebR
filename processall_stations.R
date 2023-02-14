
# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","htmlwidgets","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse","rstudioapi")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
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

# source("process_imn_rad.R")
# source("processMAT06blk1.R")
# source("processMAT06blk3.R")
# source("processMAT81.R")
# source("process_lake_toolik.R")

list.files(pattern = "process_",full.names = TRUE) %>% 
  map(function(x) {
    print(x)
    source(x)
})
