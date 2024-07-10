## Read data from Campbell Sci logger files.  
##  MAT1989 logger
## For table loggers, i.e. TOA5 files
## column nmaes are in the 2nd row of the file
## For MIXED ARRAY DATA FILES**(Older array data without variable names.)
## importCSdata expects the .fsl file in the same directory
## with the same name as the data file but with .fsl extension
## Ths .fsl file has the column labels. A list of dataframes will
## be returned. The lists names will be the ID numbers.
## Jim Laundre 2020-03-04
## Revised
## 2023-07-06 Took out the gh and shade met data plots. They were discontinued.  Jim L

# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","htmlwidgets","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse")

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

tabl_1 <-  "./current/MAT89_Half_Hourly.dat"
tabl_2 <-  "./current/MAT89_Soil_Temp.dat"
logger_file <- c(tabl_1,tabl_2)

#-------------------------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./mat89logger.html")
if(is.na(html_file_date)) {html_file_date <-0}
if(html_file_date < dat_file_date) {
    
  
  #****************************************************************************************************
  # MAT89 logger data are in CSI TOA5 data files
  # importCSdata will read in multiple files and create a list of data frames for each file
  # For ploting data frames of the met and soil data are extracted and limited to 4 months
  #****************************************************************************************************
    
  logger_data<- logger_file %>% map(function(x) importCSdata(x))
  
  #lets see what was read in.
  #map(logger_data,~names(.x))
  
  met_data <-  logger_data[[1]] %>%
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(6)) %>% 
    select(-starts_with("gh"),-starts_with("sh")) %>% 
  mutate(across(where(is.numeric), ~na_if(.,-7999))) 
 
  soil_data <- logger_data[[2]]  %>% 
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(6)) %>% 
   mutate(across(where(is.numeric), ~na_if(.,-7999)))

    # set the min and max for the initial x axis display in ggplotly
  max_date <- max(c(met_data$timestamp,soil_data$timestamp))
  min_date <-max_date - lubridate::days(5) 
  
  #****************************************************************************************************
  #Plots
  #****************************************************************************************************
  
  p1 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=ct_temp_avg, color = "control")) +
    geom_hline(aes(yintercept = 0))+
    scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
    scale_color_manual(values = c(
      'control' = 'blue')) +
    labs(title = "MAT89 Met Data",
         subtitle = "Control",
         x = "Date",
         y = "Degrees Celsius",
         color = '') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.7),
          legend.position = "top")
  p2 <- ggplot(met_data) +
    theme_bw() +
    geom_line(aes(x=timestamp,y=ct_rh,color="control RH"),linewidth=.1,linetype="twodash")+
    scale_x_datetime()+
    scale_color_manual(values = c(
      "control RH" = "blue")) +
    labs(title = "MAT89 Air Temperature/RH",
         x = "Date",
         y = "Relative Humidity (%)",
         color = '')+
    theme(plot.title = element_text(hjust = 0.2),
          axis.title.y = element_markdown(color = "black", size = 8))
  p3  <- ggplot(met_data) +
    theme_bw() +
    geom_line(aes(x=timestamp,y=ctpar_den_avg,color="control PAR"),linewidth=.1)+
    scale_x_datetime()+
    labs(title ="MAT89 Photosynthetically active radiation (PAR)",
         x = "Date",
         y = "PAR (&mu;mol s<sup>1</sup>m<sup>2</sup>) ",
         color = '')+
    theme(plot.title = element_markdown(hjust = 0.5,color = "black", size = 10),
          legend.position = "top",
          axis.title.y = element_markdown(color = "black", size = 8))
  
  #  the dynamicTicks needs to be true for the buttons to show
  # autorange needs to be FALSE for range to work
  
  # Define xaxis opitons for using a range slider
  xax<- list( 
    autorange=F,
    range= list(min_date, max_date),
    rangeselector = list(
      buttons = list(
        list(count = 7, label = "1 week", step ="day", stepmode = "backward"),
        list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
        list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
        list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
        list(step = "all")
      )),
    rangeslider = list(type = "date", thickness=0.05))
  
  # Creat 3 plots for the panel
  p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
          layout(xaxis= xax,
                 yaxis =list(zerolinewidth = .1)) %>% 
    partial_bundle()
  p2_p <- ggplotly(p2,dynamicTicks = T) %>%
          layout(xaxis= xax) %>%
    partial_bundle()
  p3_p <- ggplotly(p3,dynamicTicks = T) %>%
    layout(xaxis= xax) %>%
    partial_bundle()
  
  #wind
  
  wp1 <- met_data %>% select(timestamp,ws_ms_s_wvt) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    #coord_cartesian(ylim = c(-15,15)) +
    labs(x = "Date",
         y = "Wind Speed (m/s)",
         color = 'Legend')+
    theme_bw() +
    geom_line(color="red",linewidth=.1)+
    theme(axis.title.y = element_markdown(color = "black", size = 8))
  
  wp2 <- met_data %>% select(timestamp,wind_dir_d1_wvt) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(0,360)) +
    labs(x = "Date",
         y = "Wind Direction \nDegrees")+
    theme_bw() +
    geom_point(color="deepskyblue",shape =1)+
    theme(axis.title.y = element_markdown(color = "black", size =8))
  
  w1_p <- ggplotly(wp1,dynamicTicks = T)
  w2_p <- ggplotly(wp2,dynamicTicks = T)
  
  p <- subplot(w2_p,w1_p,p3_p,p2_p,p1_p, nrows=5, shareX = TRUE,titleY = T,
               heights = c(.15,.15,.15,.15,.4)) %>% 
    layout(title = 'MAT89 Met Data',margin = 0.01)
  #p <- subplot(p3_p,p2_p,p1_p, nrows=3, shareX = TRUE,titleY = T,heights = c(.2,.2,.6))
 # p <- toWebGL(p)
  htmlwidgets::saveWidget(p, "mat89logger.html", title = "MAT89 Met Data")
  
  #Soil
  sp1 <- soil_data %>% select(timestamp,starts_with("ct")) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(-15,15)) +
    labs(title ="MAT89 CT Soil Temperature",
         x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1)
  
  sp2 <- soil_data %>% select(timestamp,starts_with("np")) %>%
    gather("key", "value", -timestamp) %>%
    ggplot(data=., aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    labs(title ="MAT89 NP Soil Temperature",
         x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    coord_cartesian(ylim = c(-15,15)) +
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1)
  
  sp3 <-  ggplot(soil_data) +
    geom_line(aes(x=timestamp, y=batt_v_min, color = "Battery min")) +
    scale_x_datetime()+
    labs(title ="MAT89",
         x = "Date",
         y = "volts",
         color = '')+
    scale_color_manual(values = c("Battery min" = "black"))+
    theme(plot.title =  element_text(hjust = 1),
          legend.position = "top")+
    coord_cartesian(ylim = c(10,15)) +
    theme_bw() 
  sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,range = c(-15, 15))
    ) %>% 
    partial_bundle()
  sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,range = c(-15, 15))
    ) %>% 
    partial_bundle()
  sp3_p <- ggplotly(sp3,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,range = c(10, 15))
    ) %>% 
    partial_bundle()
  p <- subplot(sp3_p,sp1_p,sp2_p, nrows=3, shareX = TRUE,titleY = T,
               heights = c(.20,.40,.40))%>% 
    layout(title = 'MAT89 Soil Temperatures',margin = 0.01)
  #p <- toWebGL(p)
  htmlwidgets::saveWidget(p, "mat89soil.html", title = "MAT89 Soil Temperatures")
  
}
