## Read data from Campbell Sci logger files.  
## SHR89 logger
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

tabl_1 <-  "./current/shrub.dat"

logger_file <- c(tabl_1)

#-------------------------------------------------------------------------

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./shr89met.html")
if(is.na(html_file_date)) {html_file_date <-0} #Check if there is a file
if(html_file_date < dat_file_date) {
      
    #****************************************************************************************************
    # WetSedge logger data are in CSI TOA5 data files
    # importCSdata will read in multiple files and create a list of data frames for each file
    # For ploting data frames of the met and soil data are extracted and limited to 4 months
    #****************************************************************************************************
      
    logger_data<- logger_file %>% map(function(x) importCSdata(x))
    
    #lets see what was read in.
    #map(logger_data,~names(.x))
    
    met_data <-  logger_data[[1]] %>%
      clean_names() %>%
      arrange(timestamp)%>%
      filter(timestamp > max(timestamp)-months(6))
    
    #****************************************************************************************************
    #Plots
    #****************************************************************************************************
    
    #attach(met_data)
    
    p1 <- ggplot(met_data) +
      geom_line(aes(x=timestamp, y=air_tc_3m_avg, color = "3M Air")) +
      geom_line(aes(x=timestamp, y=t109_3m_avg,color = "3M Air TC")) +
      geom_line(aes(x=timestamp, y=gh_air_tc_avg, color ="Greenhouse Air")) +
      geom_line(aes(x=timestamp, y =sh_air_tc_avg, color = "Shade Air")) +
      geom_hline(aes(yintercept = 0))+
      scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
      scale_color_manual(values = c(
        '3M Air' = 'blue',
        '3M Air TC' = 'red',
        'Greenhouse Air' = 'green',
        'Shade Air' = 'yellow')) +
      labs(x = "Date",
           y = "Degrees Celsius",
           color = '') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.7),
            legend.position = "top")
    p2 <- ggplot(met_data) +
      theme_bw() +
      geom_line(aes(x=timestamp,y=rh_3meter, color = "3M RH")) +
      geom_line(aes(x=timestamp,y=gh_rh, color="Greenhouse RH")) +
      geom_line(aes(x=timestamp,y=sh_rh, color="Shade RH"))+
      scale_x_datetime()+
      scale_color_manual(values = c(
        "3M RH" = "blue",
        "Greenhouse RH" ="green",
        "Shade RH" = "yellow")) +
      labs(x = "Date",
           y = "RH(%)",
           color = '')+
      theme(plot.title = element_text(hjust = 0.7),
            axis.title.y = element_markdown(color = "black", linewidth = 8))
    
    p3 <- ggplot(met_data) +
      geom_line(aes(x=timestamp, y=batt_v_avg, color = "Battery average")) +
      scale_x_datetime()+
      labs(x = "Date",
           y = "volts",
           color = '')+
      coord_cartesian(ylim = c(8,15)) +
      scale_color_manual(values = c("Battery average" = "black"))+
      theme_bw() 
    
    
    #  the dynamicTicks needs to be true for the buttons to show
    # autorange needs to be FALSE for range to work
    
    # set the min and max for the initial x axis display in ggplotly
    min_date <-max(met_data$timestamp)- lubridate::days(5)
    max_date <-max(met_data$timestamp)
    
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
    
    anno_agr$text <- "3M GH and SH Air Temperatures"
    p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
            layout(xaxis= xax,
                   yaxis =list(zerolinewidth = .1,fixedrange = FALSE),
                   annotations = anno_agr) %>% 
      partial_bundle() 
    anno_agr$text <- "3M GH and SH RH"
    p2_p <- ggplotly(p2,dynamicTicks = T) %>%
            layout(xaxis = xax,
                   yaxis = list(fixedrange = FALSE),
                   annotations = anno_agr) %>%
      partial_bundle() 
    anno_agr$text <- "Battery"
    p3_p <- ggplotly(p3,dynamicTicks = T) %>% 
      layout(xaxis= list( 
        autorange=F,range= list(min_date, max_date)),
        yaxis = list(autorange = FALSE,fixedrange = FALSE),
        annotations = anno_agr) %>% 
      partial_bundle()
    
    p <- subplot(p2_p,p1_p,p3_p, nrows=3, shareX = TRUE,titleY = T,heights = c(.2,.7,.1),which_layout = 2)%>% 
      layout(title = 'Shrub1981 CT GH SH Air/Rh',margin = 0.01)
    
    
    htmlwidgets::saveWidget(p, "shr89met.html",title = "Shrub89 Met")
    
    #Soil
    soil_data <- met_data %>% select(timestamp,contains(c("cm","sur"))) %>% 
      na_if(799900) %>% na_if(-7999)
    
    sp1 <- soil_data %>% select(timestamp,contains(c("ct"))) %>%
      gather("key", "value", -timestamp)%>%
      ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
      scale_x_datetime()+
      coord_cartesian(ylim = c(-15,15)) +
      labs(x = "Date",
           y = "Celsius",
           color = 'Legend')+
      theme_bw() +
      geom_line(aes(color = key),linewidth=.1) +
      theme(plot.title = element_markdown(hjust = 0.5,color = "black", linewidth = 10),
            legend.position = "top",
            axis.title.y = element_markdown(color = "black", linewidth = 8))
    
    sp2 <- soil_data %>% select(timestamp,contains(c("gh"))) %>%
      gather("key", "value", -timestamp) %>%
      ggplot(data=., aes(x=timestamp, y = value,color=key)) +
      scale_x_datetime()+
      labs(x = "Date",
           y = "Celsius",
           color = 'Legend')+
      coord_cartesian(ylim = c(-15,15)) +
      theme_bw() +
      geom_line(aes(color = key),linewidth=.1)+
      theme(plot.title = element_markdown(hjust = 0.5,color = "black", linewidth = 10),
            legend.position = "top",
            axis.title.y = element_markdown(color = "black", linewidth = 8))
    
    
    sp3 <- soil_data %>% select(timestamp,contains(c("np"))) %>%
      gather("key", "value", -timestamp) %>%
      ggplot(data=., aes(x=timestamp, y = value,color=key)) +
      scale_x_datetime()+
      labs(x = "Date",
           y = "Celsius",
           color = 'Legend')+
      coord_cartesian(ylim = c(-15,15)) +
      theme_bw() +
      geom_line(aes(color = key),linewidth=.1)+
      theme(plot.title = element_markdown(hjust = 0.5,color = "black", linewidth = 10),
            legend.position = "top",
            axis.title.y = element_markdown(color = "black", linewidth = 8))
    
    anno_agr$text <- "Control Soil Temperatures"
    sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
      layout(xaxis= xax,
             yaxis =list(zerolinewidth = .1,fixedrange = FALSE),
             annotations = anno_agr) %>% 
      partial_bundle() 
    
    anno_agr$text <- "Greenhouse Soil Temperatures"
    sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
      layout(xaxis= xax,
             yaxis =list(zerolinewidth = .1,fixedrange = FALSE),
             annotations = anno_agr) %>% 
      partial_bundle() 
    
    anno_agr$text <- "NP Soil Temperatures"
    sp3_p <- ggplotly(sp2,dynamicTicks = T) %>% 
      layout(xaxis= xax,
             yaxis =list(zerolinewidth = .1,fixedrange = FALSE),
             annotations = anno_agr) %>% 
      partial_bundle()
    
    p <- subplot(sp1_p,sp2_p,sp3_p, nrows=3, shareX = TRUE,titleY = T,which_layout = 1) %>% 
      layout(title = 'Shrub89 Soil Temperatures',margin = 0.01)
    
    htmlwidgets::saveWidget(p, "shr89soil.html", title = "Shrub89 soil")

}

