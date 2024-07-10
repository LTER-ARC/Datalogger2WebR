## Read data from Campbell Sci logger files.  
##  MAT2006 loggers
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


# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Check if script is running on the website. If so setwd else use the project wd
web_logger_dir <- "/www/arcdeims7/sites/default/files/data/datalogger"
if (dir.exists(web_logger_dir)) {
  setwd(web_logger_dir)
}
# Functions --------------------------------------------------------------

source("importCSdata.r")


# Data Files---------------------------------------------------------------

tabl_1 <-  "./current/MAT06_Blk1_Met.dat"
tabl_2 <-  "./current/MAT06_BLK1_Soil.dat"
tabl_3 <-  "./current/MAT06_BLK1_GH_Soil.dat"
logger_file <-  c(tabl_1,tabl_2, tabl_3)

# Check files dates---------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./mat2006Blk1met.html")
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
  
  met_data <-  logger_data[[1]] %>%
    rename(ct_air_3m_avg = air_3m_avg, ct_rh = rh) %>%
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(2))
  
  
  soil_data <- logger_data[[2]]  %>% 
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(2))
  
  
  soil_GH_data <- logger_data[[3]]  %>% 
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(2))
  
  # set the min and max for the initial x axis display in ggplotly
  max_date <- max(c(met_data$timestamp,soil_data$timestamp,soil_GH_data$timestamp))
  min_date <-max_date - lubridate::days(5) 
  
  #*****************************************************************************
  #Plots
  #*****************************************************************************
  
  #attach(met_data)
  
  p1 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=ct_air_3m_avg, color = "control")) +
    geom_line(aes(x=timestamp, y=gh_air_avg,color = "greenhouse")) +
    geom_hline(aes(yintercept = 0))+
    scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
    scale_color_manual(values = c(
      'control' = 'blue',
      'greenhouse' = 'red')) +
    labs(title = "MAT2006-blk1 Met Data",
         subtitle = "Control, Greenhouse",
         x = "Date",
         y = "Degrees Celsius",
         color = '') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.7),
          legend.position = "top")
  p2 <- ggplot(met_data) +
    theme_bw() +
    geom_line(aes(x=timestamp,y=ct_rh,color="control RH"),linewidth=.1,linetype="twodash")+
    geom_line(aes(x=timestamp,y=gh_rh,color="greenhouse RH"),linewidth=.1,linetype="dotted")+
    scale_x_datetime()+
    scale_color_manual(values = c(
      "control RH" = "blue",
      "greenhouse RH" = "red")) +
    labs(title = "MAT2006-Blk1 Air Temperature/RH",
         x = "Date",
         y = "Relative Humidity (%)",
         color = '')+
    theme(plot.title = element_text(hjust = 0.7),
          axis.title.y = element_markdown(color = "black", size = 8))
  p3 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=batt_v_min, color = "Battery min")) +
    scale_x_datetime()+
    labs(title ="TMAT2006-Blk1 Battery",
         x = "Date",
         y = "volts",
         color = '')+
    coord_cartesian(ylim = c(8,15)) +
    scale_color_manual(values = c("Battery min" = "black"))+
    theme_bw() 
  
  # ---- Setting up interactive plots with ggplotly-------
  # Note for rangeslider to work:
  # the dynamicTicks needs to be true for the buttons to show
  # autorange needs to be FALSE for range to work
  
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
  
  # Air Temperature panel ---------------------------------------------------
  # First convert ggplot plots to ggplotly
  
  #Title plot 1
  anno_agr$text <- "Air Temperature"
  p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
          layout(xaxis= xax,
                 yaxis =list(zerolinewidth = .1),
                 annotations = anno_agr) %>% 
    partial_bundle()

  #Title plot 2
  anno_agr$text <- "Relative Humidty"
  p2_p <- ggplotly(p2,dynamicTicks = T) %>%
          layout(xaxis= xax,
                 annotations = anno_agr) %>%
    partial_bundle()
  
  #Title plot 3
  anno_agr$text <- "Battery"
  p3_p <- ggplotly(p3,dynamicTicks = T) %>% 
    layout(xaxis= list( 
      autorange=F,range= list(min_date, max_date)),
      yaxis = list(autorange = FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  #Put the plots together on a panel 
  panel1 <- subplot(p2_p,p1_p,p3_p, nrows=3, shareX = TRUE,titleY = T,heights = c(.3,.4,.3),which_layout = 2)
  htmlwidgets::saveWidget(panel1, "mat2006Blk1met.html",title = "MAT06 Met")
  
  #Soil ggplot Plots in CT, GH and F10 plots -----
  
  sp1 <- soil_data %>% select(timestamp,intersect(contains("ct"), contains("avg"))) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(-15,15)) +
    labs(title ="MAT2006-Blk1 CT Soil Temperature",
         x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1)
 
  sp2 <- soil_data %>% select(timestamp,starts_with("f10")) %>%
    gather("key", "value", -timestamp) %>%
    ggplot(data=., aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    coord_cartesian(ylim = c(-15,15)) +
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1) +
    theme(plot.title = element_text(hjust = 1),
          axis.title.y = element_markdown(color = "black", size = 8),
          legend.position = "top") 
  
  sp3 <- soil_GH_data %>% select(timestamp,intersect(contains("gh"), contains("avg"))) %>%
    gather("key", "value", -timestamp) %>%
    mutate(treatment =as.factor(str_extract(key,"[ghct]+"))) %>% 
    ggplot(data=., aes(x=timestamp, y = value, color=key)) +
    scale_x_datetime()+
    labs(title ="MAT2006-Blk1 Greenhouse Plot Soil Temperatures",
         x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    coord_cartesian(ylim = c(NA,15)) +
    theme_bw() +
    geom_line(aes(color = key, linewidth = treatment)) +
    guides(linewidth = "none") +
    scale_linewidth_manual(values = c("gh" = 1, "ghct" = 0.5))
  
  sp4 <- soil_GH_data %>% select(timestamp,contains("vw")) %>%
    mutate(across(where(is.numeric), ~na_if(.,-7999))) %>%
    gather("key", "value", -timestamp) %>%
    mutate(treatment =as.factor(str_extract(key,"[ghct]+"))) %>% 
    ggplot(data=., aes(x=timestamp, y = value, color = key)) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "Fractional %",
         color = 'Legend')+
    coord_cartesian(ylim = c(0,NA)) +
    theme_bw() +
    geom_line(aes(color = key, linewidth = treatment)) +
    guides(linewidth = "none") +
    scale_linewidth_manual(values = c("gh" = 1, "ghct" = 0.5))
  
  # Soil Temperature interactive panel ----
  # First convert ggplot plots to ggplotly
  #Title plot 1 
  anno_agr$text <- "Control Plot Soil Temperatures"  
  sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,range = c(-15, 15)),
      annotations = anno_agr
    ) %>% 
    partial_bundle()
  #Title plot 2
  anno_agr$text <- "F10 Plot Soil Temperatures"
  sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,
      fixedrange= FALSE),
      annotations = anno_agr
    ) %>% 
    partial_bundle()
  panel2 <- subplot(sp1_p,sp2_p, nrows=2, shareX = TRUE,titleY = T)
  htmlwidgets::saveWidget(panel2, "mat2006Blk1soil.html", title = "MAT06 soil")
 
  # Greenhouse plot Soil Temperature and moisture interactive panel ---------------------------
  # First convert ggplot plots to ggplotly
  
  #Title plot 1
  anno_agr$text <- "Greenhouse Plot Soil Temperatures"
  sp3_p <- ggplotly(sp3,dynamicTicks = T) %>% 
    layout(xaxis = xax,
           yaxis = list(autorange=FALSE,
                        fixedrange= FALSE),
           annotations = anno_agr) %>% 
    partial_bundle()
  
  #Title plot 2
  anno_agr$text <- "Greenhouse Plot Soil moisture (VW)"
  sp4_p <- ggplotly(sp4,dynamicTicks = T) %>% 
    layout(xaxis = xax,
           yaxis = list(autorange=F,
                        fixedrange= FALSE),
           annotations = anno_agr) %>% 
    partial_bundle()
  
  #Put the plots together on a panel
  panel3 <- subplot(sp3_p,sp4_p, nrows=2, shareX = TRUE,titleY = T) %>% 
    layout(title = 'MAT206-Blk1 Greenhouse Plot Soil Temperature and Moisture',margin = 0.01)
  htmlwidgets::saveWidget(panel3, "mat2006Blk1GHsoil.html", title = "MAT06 Blk1 Greenhouse Soil")
}

