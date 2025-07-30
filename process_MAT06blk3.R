## Read data from Campbell Sci logger files.  
##  MAT2006 loggers
## For table loggers, i.e. TOA5 files
## column names are in the 2nd row of the file
## For MIXED ARRAY DATA FILES**(Older array data without variable names.)
## importCSdata expects the .fsl file in the same directory
## with the same name as the data file but with .fsl extension
## The .fsl file has the column labels. A list of dataframes will
## be returned. The lists names will be the ID numbers.
## Jim Laundre 2021-06-28
## Revised
## 2023-07-13 Moved the greenhouse soil temperature data to the soil panel
##            Added more documentation and removed the install package code since
##            not best practice.  let the lower temperature y axis on the greenhouse
##            soil plot auto range.  Jim L

# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","htmlwidgets","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse","googledrive")

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
# Google drive file
tabl_1 <-  drive_download(as_id("https://drive.google.com/file/d/1-47Oqe3-OOBJEblmlzxQizAzdfsADdh4/view?usp=sharing"), overwrite = TRUE) %>% .$name
tabl_2 <-   drive_download(as_id("https://drive.google.com/file/d/1-As9fM-k1frENRAgLSbFx2x2FqRZA07M/view?usp=sharing"), overwrite = TRUE) %>% .$name
#-------------------------------------------------------------------------
# local files
tabl_1 <-  "./current/MAT06_BLK3_Met.dat"
tabl_2 <-  "./current/MAT06_BLK3_Soil.dat"
#-------------------------------------------------------------------------
logger_file <- c(tabl_1,tabl_2)


# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./mat2006Blk3met.html")
if(is.na(html_file_date)) {html_file_date <-0}

if(html_file_date < dat_file_date) {
  
  #****************************************************************************************************
  # MAT06 logger data are in CSI TOA5 data files
  # importCSdata will read in multiple files and create a list of data frames for each file
  # For plotting, data frames of the met and soil data are extracted and limited to 5 months
  #****************************************************************************************************
    
  logger_data<- logger_file %>% map(function(x) importCSdata(x))
  
  #lets see what was read in.
  #map(logger_data,~names(.x))
  
  met_data <-  logger_data[[1]] %>%
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(6))
  
  soil_data <- logger_data[[2]]  %>% 
    clean_names() %>%
    arrange(timestamp)%>%
    filter(timestamp > max(timestamp) %m-% months(6))
  max_date <- max(c(met_data$timestamp,soil_data$timestamp))
  #****************************************************************************************************
  #Plots
  #****************************************************************************************************
  
 # attach(met_data)
  
  p1 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=airtcontrol_avg, color = "control")) +
    geom_line(aes(x=timestamp, y=air_gh_avg,color = "greenhouse")) +
    geom_hline(aes(yintercept = 0))+
    scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
    scale_color_manual(values = c(
      'control' = 'blue',
      'greenhouse' = 'red')) +
    labs(title = "MAT2006-blk3 Met Data",
         subtitle = "Control, Greenhouse",
         x = "Date",
         y = "Degrees Celsius",
         color = '') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 1),
          legend.position = "top")
  p2 <- ggplot(met_data) +
    theme_bw() +
    geom_line(aes(x=timestamp,y=rhct,color="control RH"),linewidth=.1,linetype="twodash")+
    geom_line(aes(x=timestamp,y=rh_gh,color="greenhouse RH"),linewidth=.1,linetype="dotted")+
    scale_x_datetime()+
    scale_color_manual(values = c(
      "control RH" = "blue",
      "greenhouse RH" = "red")) +
    labs(title = "MAT2006-Blk3 Air RH",
         x = "Date",
         y = "Relative Humidity (%)",
         color = '')+
    theme(plot.title = element_text(hjust = 1),
          axis.title.y = element_markdown(color = "black", size = 8),
          legend.position = "top")
  p3 <- ggplot(met_data) +
    geom_line(aes(x=timestamp, y=battv_avg, color = "Battery avg")) +
    scale_x_datetime()+
    labs(title ="MAT2006-Blk3",
         x = "Date",
         y = "volts",
         color = '')+
    scale_color_manual(values = c("Battery avg" = "black"))+
    theme(plot.title =  element_text(hjust = 1),
          legend.position = "top")+
    coord_cartesian(ylim = c(10,14)) +
    theme_bw() 
 
  # ---- Setting up interactive plots with ggplotly-------
  # Note for rangeslider to work:
  # the dynamicTicks needs to be true for the buttons to show
  # autorange needs to be FALSE for range to work
  
  # set the min and max for the initial x axis display in ggplotly
  min_date <-max_date - lubridate::days(5)
  
  
  # Define xaxis options for using a range slider
  xax<- list( 
    autorange=F,
    range= list(min_date, max_date),
    rangeselector = list(
      buttons = list(
        list(count = 1, label = "1 week", strp ="week", stepmode = "backward"),
        list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
        list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
        #list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
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
  
  # Air Temperature panel ----
  # First convert ggplot plots to ggplotly
  #Title plot 1
  anno_agr$text <- "Air Temperature"
  # Create 3 plots for the panel
  p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
          layout(xaxis= xax,
                 yaxis =list(zerolinewidth = .1, 
                             fixedrange = FALSE),
                 annotations = anno_agr) %>% 
    partial_bundle()
  #Title plot 2
  anno_agr$text <- "Relative Humidty"
  p2_p <- ggplotly(p2,dynamicTicks = T) %>%
          layout(xaxis= xax,
                 yaxis = (list(fixedrange = FALSE)),
                 annotations = anno_agr) %>%
    partial_bundle()

  #Title plot 3
  anno_agr$text <- "Battery"
  p4_p <- ggplotly(p3, dynamicTicks = T) %>%
    layout(
      xaxis = list(autorange = F,
                   range = list(min_date, max_date)),
      yaxis = list(fixedrange = FALSE),
      annotations = anno_agr) %>%
    partial_bundle()
  
  #Put the plots together on a panel
  panel1 <- subplot(p2_p,p1_p,p4_p, nrows=3, shareX = TRUE,titleY = T,
               heights = c(.2,.6,.2), which_layout = 2) %>% 
    layout(title = 'MAT2006-Blk3 CT_GH Air/Rh Temperatures',margin = 0.01)
    
  htmlwidgets::saveWidget(panel1, "mat2006Blk3met.html", title = "MAT06 Blk 3")
  
  
  #Soil ggplot Plots -----
  
  sp1 <- soil_data %>% select(timestamp,starts_with("ct")) %>%
    gather("key", "value", -timestamp)%>%
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(-15,15)) +
    labs(x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key),linewidth=.1)
  
  sp2 <- soil_data %>% select(timestamp,starts_with("np")) %>%
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
  
  sp3 <- soil_data %>% select(timestamp,starts_with("t107")) %>%
    gather("key", "value", -timestamp) %>%
    mutate(treatment =as.factor(str_extract(key,"[ghct]{2}"))) %>% 
    ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    coord_cartesian(ylim = c(NA,15)) +
    labs(x = "Date",
         y = "Celsius ",
         color = 'Legend')+
    theme_bw() +
    geom_line(aes(color = key,linewidth = treatment)) +
    guides(linewidth = "none") +
    scale_linewidth_manual(values = c("gh" = 1, "ct" = 0.2))
  
  sp4 <- soil_data %>% select(timestamp,contains("vw")) %>%
    gather("key", "value", -timestamp) %>%
    mutate(treatment =as.factor(str_extract(key,"[ghct]{2}"))) %>%
    mutate(across(where(is.numeric), ~na_if(.,-7999))) %>%
    ggplot(data=., aes(x=timestamp, y = value,color=key)) +
    scale_x_datetime()+
    labs(x = "Date",
         y = "Fractional %",
         color = 'Legend')+
    coord_cartesian(ylim = c(0,NA)) +
    theme_bw() +
    geom_line(aes(color = key,linewidth = treatment)) +
    guides(linewidth = "none") +
    scale_linewidth_manual(values = c("gh" = 1, "ct" = 0.2))
  
  # Soil Temperature interactive panel ---------------------------
  # First convert ggplot plots to ggplotly
  #Title plot 1 
  anno_agr$text <- "Control Plot Soil Temperatures"
  sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
    layout(xaxis= xax,
      yaxis = list(autorange=F,range = c(-15, 15),
                   fixedrange=FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  #Title plot 2
  anno_agr$text <- "F10 Plot Soil Temperatures"
  sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
    layout(xaxis = xax,
      yaxis = list(autorange=F,range = c(-15, 15),
                   fixedrange =FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  #Title plot 3
  anno_agr$text <- "Greenhouse Plot Soil Temperatures"
  sp3_p <- ggplotly(sp3,dynamicTicks = T) %>% 
    layout(xaxis = xax,
      yaxis = list(autorange=FALSE,
                   fixedrange= FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  #Put the plots together on a panel
  panel2 <- subplot(sp3_p,sp1_p,sp2_p, nrows=3, shareX = TRUE,titleY = T) %>% 
    layout(title = 'MAT206-Blk3 Soil Temperature',margin = 0.01)
  htmlwidgets::saveWidget(panel2, "mat2006Blk3soil.html", title = "MAT06 Soil")

  
  # Greenhouse plot Soil Temperature and moisture interactive panel ---------------------------
  # First convert ggplot plots to ggplotly
  
  # Plot 1 is sp3_p
  
  #Title plot 2
  anno_agr$text <- "Greenhouse Plot Soil moisture"
  sp4_p <- ggplotly(sp4,dynamicTicks = T) %>% 
    layout(xaxis = xax,
      yaxis = list(autorange=F,
                   fixedrange= FALSE),
      annotations = anno_agr) %>% 
    partial_bundle()
  
  #Put the plots together on a panel
  panel3 <- subplot(sp3_p,sp4_p, nrows=2, shareX = TRUE,titleY = T) %>% 
    layout(title = 'MAT206-Blk3 Greenhouse Plot Soil Temperature and Moisture',margin = 0.01)
  htmlwidgets::saveWidget(panel3, "mat2006Blk3GHsoil.html", title = "MAT06 Greenhouse Soil")
}
