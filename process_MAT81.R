## Read data from Campbell Sci logger files.  
##  MAT1989 logger
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

# Check if script is running on the website. If so setwd else use the project wd
web_logger_dir <- "/www/arcdeims7/sites/default/files/data/datalogger"
if (dir.exists(web_logger_dir)) {
  setwd(web_logger_dir)
}
# Functions --------------------------------------------------------------
source("importCSdata.r")

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
logger_file <-  "./current/CR1000_HistoricSoil_mean.dat"
#-------------------------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./mat1981met.html")
if(is.na(html_file_date)) {html_file_date <-0}
if(html_file_date < dat_file_date) {
  
  
  #****************************************************************************************************
  # MAT06 logger data are in CSI TOA5 data files
  # importCSdata will read in multiple files and create a list of data frames for each file
  # For ploting data frames of the met and soil data are extracted and limited to 4 months
  #****************************************************************************************************
    
  logger_data<- logger_file %>% map(function(x) importCSdata(x))
  
  #lets see what was read in.
  #map(logger_data,~names(.x))
  
  met_soil_data <-  logger_data[[1]] %>%
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp)-months(12))
  
  #****************************************************************************************************
  #Plots
  #****************************************************************************************************
  
  
  p1 <- ggplot(met_soil_data) +
    geom_line(aes(x=timestamp, y=air_3m_avg, color = "control")) +
    geom_hline(aes(yintercept = 0))+
    scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
    scale_color_manual(values = c(
      'control' = 'blue')) +
    labs(title = "MAT1981 Met Data",
         subtitle = "Control",
         x = "Date",
         y = "Degrees Celsius",
         color = '') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 1),
          legend.position = "top")
  p2 <- ggplot(met_soil_data) +
    theme_bw() +
    geom_line(aes(x=timestamp,y=rh,color="control RH"),linewidth=.1,linetype="twodash")+
    scale_x_datetime()+
    scale_color_manual(values = c(
      "control RH" = "blue")) +
    labs(title = "MAT1981 RH",
         x = "Date",
         y = "Relative Humidity (%)",
         color = '')+
    theme(plot.title = element_text(hjust = 1),
          axis.title.y = element_markdown(color = "black", size = 8),
          legend.position = "top")
  p3 <- ggplot(met_soil_data) +
    geom_line(aes(x=timestamp, y=batt_v_min, color = "Battery avg")) +
    scale_x_datetime()+
    labs(title ="MAT1981",
         x = "Date",
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
  min_date <-max(met_soil_data$timestamp)- lubridate::days(5)
  max_date <-max(met_soil_data$timestamp)
  
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
  anno_agr$text <- "Air Temperature"
  # Create 3 plots for the panel
  p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
          layout(xaxis= xax,
                 yaxis =list(zerolinewidth = .1, 
                             fixedrange = FALSE),
                 annotations = anno_agr) %>% 
    partial_bundle()
  
  anno_agr$text <- "Relative Humidty"
  p2_p <- ggplotly(p2,dynamicTicks = T) %>%
          layout(xaxis= xax,
                 yaxis = (list(fixedrange = FALSE)),
                 annotations = anno_agr) %>%
    partial_bundle()
  
  anno_agr$text <- "Battery"
  p3_p <- ggplotly(p3, dynamicTicks = T) %>%
    layout(
      xaxis = list(autorange = F,
                   range = list(min_date, max_date)),
      yaxis = list(fixedrange = FALSE),
      annotations = anno_agr) %>%
    partial_bundle()
  
  
  p <- subplot(p2_p,p1_p,p3_p, nrows=3, shareX = TRUE,titleY = T,
               heights = c(.20,.6,.2), which_layout = 2) %>% 
    layout(title = 'MAT1981 CT Air/Rh',margin = 0.01)
    
  htmlwidgets::saveWidget(p, "mat1981met.html", title = "MAT1981")
  
  #Soil
  sp1 <- met_soil_data %>% select(timestamp,intersect(contains("avg"),contains("cm"))) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(-15,15)) +
    labs(x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1)
  
  sp2 <- met_soil_data %>% select(timestamp,contains("vw")) %>%
    gather("key", "value", -timestamp) %>%
    ggplot(data=., aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "v/w",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1) +
    theme(plot.title = element_text(hjust = 1),
          axis.title.y = element_markdown(color = "black", size = 8),
          legend.position = "top")
  
  anno_agr$text <- "Control and NP Soil"
  sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,range = c(-15, 15),
                   fixedrange=FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  anno_agr$text <- "Soil Water"
  sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
    layout(xaxis=  xax,
      yaxis = list(autorange=F,range = c(0, 1),
                   fixedrange =FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  htmlwidgets::saveWidget(sp1_p,"mat1981soiltemp.html",title = "MAT81 Soil Temperatures")
  htmlwidgets::saveWidget(sp2_p,"mat1981soilmoist.html",title = "MAT81 Soil Moisture")
  # 
  # p <- subplot(sp1_p,sp2_p, nrows=2, shareX = TRUE,titleY = T,
  #              heights = c(.5,.5), which_layout = 2) %>% 
  #   layout(title = 'MAT1981 Soil Temperature',margin = 0.01)
  # htmlwidgets::saveWidget(p, "mat1981soil.html", title = "MAT1981 Soil")
  
}
