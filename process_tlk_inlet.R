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

logger_file <-  "./current/TLK_Inlet.dat"

#****************************************************************************************************
# tlk_inlet logger data are in CSI array data files
# importCSdata will read in multiple files and create a list of data frames for each file.  
#
#****************************************************************************************************

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
p4_p <- ggplotly(p4,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,range= list(min_date, max_date))
  ) %>% 
  partial_bundle()

p <- subplot(p1_p,p2_p,p3_p,p4_p, nrows=4, shareX = TRUE,titleY = T,heights = c(.3,.3,.3,.1))

htmlwidgets::saveWidget(p, "tlk_inlet.html")
