## Read data from Campbell Sci logger files.  
##  Repeater Logger
## For table loggers, i.e. TOA5 files
## column names are in the 2nd row of the file
## For MIXED ARRAY DATA FILES**(Older array data without variable names.)
## importCSdata expects the .fsl file in the same directory
## with the same name as the data file but with .fsl extension
## The .fsl file has the column labels. A list of dataframes will
## be returned. The lists names will be the ID numbers.
## Jim Laundre 2021-06-28
## Revised

# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","htmlwidgets","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Functions --------------------------------------------------------------
setwd("/www/arcdeims7/sites/default/files/data/datalogger")
source("importCSdata.r")

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------

tabl_1 <-  "./current/CR510_Repeater.dat"
html_tabl_1 <- "./Itigaknit-Repeater.html"
logger_file <- c(tabl_1)
#-------------------------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime(html_tabl_1)
if(is.na(html_file_date)) {html_file_date <-0}  # Check if there is a file

if(html_file_date < dat_file_date) {
  
  #****************************************************************************************************
  # Itigaknit Repeater logger data are in CSI vector data files
  # importCSdata will read in multiple files and create a list of data frames for each file
  # For plotting data frames of the met and soil data are extracted and limited to x months
  #****************************************************************************************************
    
  logger_data<- logger_file %>% map(function(x) importCSdata(x))
  logger_data<- flatten(logger_data)
  
  #lets see what was read in.
  #map(logger_data,~names(.x))
  
  met_data <-  logger_data[[1]] %>%
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp)-months(12))
  
  #****************************************************************************************************
  #Plots
  #****************************************************************************************************
  
  attach(met_data)
  
  p1 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=reftemp_avg, color = "control")) +
    geom_hline(aes(yintercept = 0))+
    scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
    scale_color_manual(values = c(
      'logger Reference Temperature' = 'blue')) +
    labs(title = "Itigaknit RepeaterMet Data",
         x = "Date",
         y = "Degrees Celsius",
         color = '') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 1),
          legend.position = "top")

  p2 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=volts_avg, color = "Battery avg")) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "volts",
         color = '')+
    scale_color_manual(values = c("Battery avg" = "black"))+
    theme(plot.title =  element_text(hjust = 1),
          legend.position = "top")+
    coord_cartesian(ylim = c(10,14)) +
    theme_bw() 
 
  #  the dynamicTicks needs to be true for the buttons to show
  # autorange needs to be FALSE for range to work
  
  # set the min and max for the initial x axis display in ggplotly
  min_date <-max(timestamp)- lubridate::days(5)
  max_date <-max(timestamp)
  
  # Define xaxis options for using a range slider
  xax<- list( 
    autorange=F,
    range= list(min_date, max_date),
    rangeselector = list(
      buttons = list(
        list(count = 1, label = "1 week", strp ="week", stepmode = "backward"),
        list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
        list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
        list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
        list(step = "all")
      )),
    rangeslider = list(type = "date", thickness=0.05))
  # Create a list of arguments for the annotation layout to add titles to the subplots
  anno_agr <-list(x = .5,
                  text = "",
                  y = 1,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "center",
                  yanchor = "top",
                  yshift = 20,
                  showarrow = FALSE,
                  font = list(size = 15))
  
  # Create 3 plots for the panel
  anno_agr$text <- "Logger Reference Temperature"
  # Create 2 plots for the panel
  p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
          layout(xaxis= xax,
                 yaxis =list(zerolinewidth = .1, 
                             fixedrange = FALSE),
                 annotations = anno_agr) %>% 
    partial_bundle()
  
  anno_agr$text <- "Battery"
  p2_p <- ggplotly(p2,dynamicTicks = T) %>%
          layout(xaxis= xax,
                 yaxis = (list(fixedrange = FALSE)),
                 annotations = anno_agr) %>%
    partial_bundle()
  

  p <- subplot(p1_p,p2_p, nrows=2, shareX = TRUE,titleY = T,
               heights = c(.6,.4), which_layout = 2) %>% 
    layout(title = 'Itigaknit Repeater Station',margin = 0.01)
    
  htmlwidgets::saveWidget(p, html_tabl_1, title = "Itigaknit Repeater Station")
  
 
}
