## Read data from Campbell Sci logger files.  
##  water plot logger
## For table loggers, i.e. TOA5 files
## column names are in the 2nd row of the file
## For MIXED ARRAY DATA FILES**(Older array data without variable names.)
## importCSdata expects the .fsl file in the same directory
## with the same name as the data file but with .fsl extension
## The .fsl file has the column labels. A list of dataframes will
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

# Check if script is running on the website. If so setwd else use the project wd
web_logger_dir <- "/www/arcdeims7/sites/default/files/data/datalogger"
if (dir.exists(web_logger_dir)) {
  setwd(web_logger_dir)
}
# Functions --------------------------------------------------------------
source("importCSdata.r")

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------

tabl_1 <-  "./current/WaterPlots_CR10XPB_4_Table1.dat"
tabl_2 <-  "./current/WaterPlots_CR10XPB_4_Table2.dat"
logger_file <- c(tabl_1,tabl_2)

#-------------------------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./waterplot.html")
if(is.na(html_file_date)) {html_file_date <-0} #Check if there a file
if(html_file_date < dat_file_date) {
  
   
  #****************************************************************************************************
  # MAT06 logger data are in CSI TOA5 data files
  # importCSdata will read in multiple files and create a list of data frames for each file
  # For ploting data frames of the met and soil data are extracted and limited to 4 months
  #****************************************************************************************************
    
  logger_data<- logger_file %>% map(function(x) importCSdata(x))
  
  #lets see what was read in.
  #map(logger_data,~names(.x))
  
  moisture_data <-  logger_data[[1]] %>%
    clean_names() %>%
    arrange(timestamp)%>%
    mutate(across(contains("Wat_con"),~ .* 100)) %>% 
    select(-record) %>% 
    filter(timestamp > max(timestamp)-months(6))
  
  temperature_data <- logger_data[[2]]  %>%
    clean_names() %>%
    arrange(timestamp)%>%
    select(-record) %>%
    filter(timestamp > max(timestamp)-months(6))
  
  #****************************************************************************************************
  #Plots
  #****************************************************************************************************
  
  # set the min and max for the initial x axis display in ggplotly
  min_date <-max(moisture_data$timestamp)- lubridate::days(5)
  max_date <-max(moisture_data$timestamp)
  
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
  
  
  #Soil
  sp1 <- temperature_data %>% select(timestamp,contains(c("soil","cm"))) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(-15,15)) +
    labs(x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1) +
    theme(plot.title = element_markdown(hjust = 0.5,color = "black", linewidth = 10),
          legend.position = "top",
          axis.title.y = element_markdown(color = "black", linewidth = 8))
  
  sp2 <- moisture_data %>% select(timestamp,contains("wat_cont")) %>%
    #na_if(799900) %>% na_if(-7999) %>% 
    gather("key", "value", -timestamp) %>%
    ggplot(data=., aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "%",
         color = 'Legend')+
    coord_cartesian(ylim = c(0,120)) +
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1)+
    theme(plot.title = element_markdown(hjust = 0.5,color = "black", linewidth = 10),
          legend.position = "top",
          axis.title.y = element_markdown(color = "black", linewidth = 8))
  sp3 <- ggplot(temperature_data) +
    geom_line(aes(x=timestamp, y=battery_avg, color = "Battery average")) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "volts",
         color = '') +
    coord_cartesian(ylim = c(8,15)) +
    scale_color_manual(values = c("Battery average" = "black"))+
    theme_bw() 
  
  anno_agr$text <- "Soil Temperatures"
  sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
    layout(xaxis= xax,
           yaxis =list(zerolinewidth = .1,fixedrange = FALSE),
           annotations = anno_agr) %>% 
    partial_bundle()
  
  anno_agr$text <- "Soil Moisture"
  sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
    layout(xaxis= xax,
           yaxis =list(zerolinewidth = .1,fixedrange = FALSE),
           annotations = anno_agr) %>% 
    partial_bundle()
  
  anno_agr$text <- "Battery"
  sp3_p <- ggplotly(sp3,dynamicTicks = T) %>% 
    layout(xaxis= list( 
      autorange=F,range= list(min_date, max_date)),
      yaxis = list(autorange = FALSE,fixedrange = FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  p <- subplot(sp1_p,sp2_p,sp3_p, nrows=3, shareX = TRUE,titleY = T,
               heights = c(.4,.4,.2)) %>% 
    layout(title = 'Former Tussock watering plots',margin = 0.01)
  htmlwidgets::saveWidget(p, "waterplot.html", title = "WaterPlot soil")
}


