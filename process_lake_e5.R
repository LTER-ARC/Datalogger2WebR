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

# Check if script is running on the website. If so setwd else use the project wd
web_logger_dir <- "/www/arcdeims7/sites/default/files/data/datalogger"
if (dir.exists(web_logger_dir)) {
  setwd(web_logger_dir)
}
# Functions --------------------------------------------------------------
source("importCSdata.r")

# Function to flatten a list with list of dataframes. From https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists/41882883#41882883
flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

#-------------------------------------------------------------------------

logger_file <-  "./current/Lake_E5.dat"

#****************************************************************************************************
# tlk_inlet logger data are in CSI array data files
# importCSdata will read in multiple files and create a list of data frames for each file.  
#
#****************************************************************************************************

# Check if there are new data to process. If not then skip running the code
dat_file_date <- file.mtime(logger_file[1])
html_file_date <-file.mtime("./waterplot.html")
if(is.na(html_file_date)) {html_file_date <-0} #Check if there a file
if(html_file_date < dat_file_date) {
  
logger_data<- logger_file %>% map(function(x) importCSdata(x))
logger_data<- flattenlist(logger_data)

#lets see what was read in.
map(logger_data,~names(.x))

logger_data <- logger_data[[1]] %>%
 # clean_names() %>%
  arrange(timestamp)

#Plots


attach(logger_data)


# set the min and max for the initial x axis display in ggplotly
min_date <-max(timestamp)- lubridate::days(5)
max_date <-max(timestamp)


p1 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=air_temp_avg, color = "Air Temperature")) +
  scale_x_datetime()+
  labs(title ="Air Temperature",
       x = "Date",
       y = "celsius ",
       color = '')+
  theme_bw() 


p2 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=rh_avg, color = "Relative Humidity")) +
  scale_x_datetime()+
  labs(title ="Lake E5 Relative Humidity",
       x = "Date",
       y = "Percent ",
       color = '')+
  scale_color_manual(values = c("Relative Humidity" = "blue"))+
  theme_bw() 

wp1 <- logger_data %>% select(timestamp,wind_speed_s_wvt) %>%
  gather("key", "value", -timestamp)%>%
  ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
  scale_x_datetime()+
  #coord_cartesian(ylim = c(-15,15)) +
  labs(x = "Date",
       y = "Wind Speed (m/s)",
       color = 'Legend')+
  theme_bw() +
  geom_line(color="red",linewidth =.1)+
  theme(axis.title.y = element_markdown(color = "black", size = 8))

wp2 <- logger_data %>% select(timestamp,wind_dir_d1_wvt) %>%
  gather("key", "value", -timestamp)%>%
  ggplot(data=.,aes(x=timestamp, y = value)) +
  scale_x_datetime()+
  coord_cartesian(ylim = c(0,360)) +
  labs(x = "Date",
       y = "Wind Direction \nDegrees")+
  theme_bw() +
  geom_point(color="deepskyblue",shape =1)+
  theme(axis.title.y = element_markdown(color = "black", size =8))

p3 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=battery_v_min, color = "Battery")) +
  scale_x_datetime()+
  labs(title ="Toolik Inlet Battery",
       x = "Date",
       y = "volts",
       color = '')+
  scale_color_manual(values = c("Battery" = "black"))+
  theme_bw() 

p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,range= list(min_date, max_date))
  ) %>% 
  partial_bundle()
p2_p <- ggplotly(p2,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,range= list(min_date, max_date))
  ) %>% 
  partial_bundle()
p3_p <- ggplotly(p3,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,range= list(min_date, max_date))
  ) %>% 
  partial_bundle()

w1_p <- ggplotly(wp1,dynamicTicks = T)
w2_p <- ggplotly(wp2,dynamicTicks = T)

p <- subplot(p1_p,p2_p,w1_p,w2_p, p3_p, nrows=5, shareX = TRUE,titleY = T,heights = c(.2,.2,.2,.2,.1))

htmlwidgets::saveWidget(p, "lake_e5.html")
}
