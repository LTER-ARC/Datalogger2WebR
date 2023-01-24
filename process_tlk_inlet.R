## Read data from Campbell Sci logger files.  
##  
## For table loggers, i.e. TOA5 files
## column nmaes are in the 2nd row of the file
## For MIXED ARRAY DATA FILES**(Older array data without variable names.)
## importCSdata expects the .fsl file in the same directory
## with the same name as the data file but with .fsl extension
## Ths .fsl file has the column labels. A list of dataframes will
## be returned. The lists names will be the ID numbers.
## Jim Laundre 2020-03-04
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

logger_file <-  "./current/TLK_Inlet.dat"
html_tabl_1 <- "./tlk_inlet.html"
#-------------------------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime(html_tabl_1)
if(is.na(html_file_date)) {html_file_date <-0}

if(html_file_date < dat_file_date) {
    
  #****************************************************************************************************
  # tlk_inlet logger data are in CSI array data files
  # importCSdata will read in multiple files and create a list of data frames for each file.  
  #
  #****************************************************************************************************
  
  logger_data<- logger_file %>% map(function(x) importCSdata(x))
  logger_data<- flatten(logger_data)
  
  #lets see what was read in.
 # map(logger_data,~names(.x))
  
  logger_data <- logger_data[[1]] %>%
   # clean_names() %>%
    arrange(timestamp)
  
  #Plots
  
  
  attach(logger_data)
  
  # Create 4 plots for the panel
  
 p1 <- ggplot(logger_data) +
    geom_line(aes(x=timestamp, y=stage_ht_avg, color = "Stage Height")) +
    scale_x_datetime()+
    labs(title ="Toolik Inlet stage height",
         x = "Date",
         y = "mm ",
         color = '')+
    theme_bw() 
  
  p2 <- ggplot(logger_data) +
    geom_line(aes(x=timestamp, y=water_temp_c, color = "Water Temperature")) +
    scale_x_datetime()+
    labs(title ="Toolik Inlet Water Temperature",
         x = "Date",
         y = "celsius ",
         color = '')+
    scale_color_manual(values = c("Water Temperature" = "blue"))+
    theme_bw() 
  
  p3 <- ggplot(logger_data) +
    geom_line(aes(x=timestamp, y=x25c_u_scm1, color = "Conductivity")) +
    scale_x_datetime()+
    labs(title ="Toolik Inlet Conductivity",
         x = "Date",
         y = "uS/cm",
         color = '')+
    scale_color_manual(values = c("Conductivity" = "green"))+
    theme_bw() 
  
  p4 <- ggplot(logger_data) +
    geom_line(aes(x=timestamp, y=battery_volts, color = "Battery")) +
    scale_x_datetime()+
    labs(title ="Toolik Inlet",
         x = "Date",
         y = "volts",
         color = '')+
    scale_color_manual(values = c("Battery" = "black"))+
    theme_bw() 
  #----------------------------------------------------------------------
  # Convert to plotly plots.
  # the dynamicTicks needs to be true for the buttons to show
  # auto range needs to be FALSE for range to work
  
  # set the min and max for the initial x axis display in ggplotly
  min_date <-max(logger_data$timestamp)- lubridate::days(5)
  max_date <-max(logger_data$timestamp)
  
  # Define xaxis options for using a range slider
  xax<- list( 
    autorange=F,
    range= list(min_date, max_date),
    rangeselector = list(
      buttons = list(
        list(count = 1, label = "1 week", strp ="week", stepmode = "backward"),
        list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
        list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
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
 #---------------------------------------------------------- 
  
  anno_agr$text <- "Stage Height"  
  p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
    layout(xaxis= xax,
           yaxis =list(fixedrange = FALSE),
           annotations = anno_agr) %>% 
    partial_bundle()
  
  anno_agr$text <- "Water Temperature"  
  p2_p <- ggplotly(p2,dynamicTicks = T) %>% 
    layout(xaxis= xax,
           yaxis =list(fixedrange = FALSE),
           annotations = anno_agr) %>%
    partial_bundle()
  
  anno_agr$text <- "Conductivity"  
  p3_p <- ggplotly(p3,dynamicTicks = T) %>% 
    layout(xaxis= xax,
           yaxis =list(rangemode="nonnegative",
                       fixedrange = FALSE),
           annotations = anno_agr) %>%
    partial_bundle()
  anno_agr$text <- "Battery"  
  p4_p <- ggplotly(p4,dynamicTicks = T) %>% 
    layout(xaxis= xax,
           yaxis =list(fixedrange = FALSE),
           annotations = anno_agr) %>%
    partial_bundle()
  
  p <- subplot(p1_p,p2_p,p3_p,p4_p, nrows=4, shareX = TRUE,titleY = T,
               heights = c(.3,.3,.3,.1))%>% 
    layout(title = 'Toolik Inlet Station',margin = 0.01)
  
  htmlwidgets::saveWidget(p, html_tabl_1,title = "Toolik Inlet")
}
