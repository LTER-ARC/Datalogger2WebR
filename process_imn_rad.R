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

logger_file <-  "./current/CR3000_Imn_Radiation_mean.dat"

#****************************************************************************************************
# 
# importCSdata will read in multiple files and create a list of data frames for each file.  
#
#****************************************************************************************************

logger_data<- logger_file %>% map(function(x) importCSdata(x))


#lets see what was read in.
map(logger_data,~names(.x))

logger_data <- logger_data[[1]] %>%
 # clean_names() %>%
  arrange(timestamp) %>%
filter(timestamp > max(timestamp)-months(2))

#Plots


attach(logger_data)


# set the min and max for the initial x axis display in ggplotly
min_date <-max(timestamp)- lubridate::days(5)
max_date <-max(timestamp)


p1 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=t_hmp_avg, color = "Air Temperature")) +
  geom_line(aes(x=timestamp, y=rh_hmp_avg/10, color = "RH scaled")) +
  geom_line(aes(x=timestamp, y=tsoil_avg_1,color= "Soil1 Temperature"))+
  geom_line(aes(x=timestamp, y=tsoil_avg_2,color= "Soi21 Temperature"))+
  scale_x_datetime()+
  coord_cartesian(ylim = c(-20,20)) +
  labs(title ="Imnavait Temperatures",
       x = "Date",
       y = "celsius ",
       color = '')+
  theme_bw() 


p2 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=parup_flxdens_avg, color = "PAR In")) +
  geom_line(aes(x=timestamp, y=pardn1_flxdens_avg, color = "PAR reflec 1")) +
  geom_line(aes(x=timestamp, y=pardn2_flxdens_avg, color = "PAR reflec 2")) +
  scale_x_datetime()+
  labs(title ="Imnaviat Radiation Station",
       x = "Date",
       y = "umole/m^2s^1",
       color = '')+
  scale_color_manual(values = c("PAR In"= "blue", "PAR reflec 1" = "yellow","PAR reflec 2" = "red"))+
  theme_bw() 

p3 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=cmp3up_avg, color = "Solar in short wave")) +
  geom_line(aes(x=timestamp, y=cmp3dn1_avg, color = "Solar out1 short wave")) +
  geom_line(aes(x=timestamp, y=cmp3dn2_avg, color = "Solar out2 short wave")) +
  scale_x_datetime()+
  labs(title ="Imnaviat Radation Station",
       x = "Date",
       y = "W/m^2",
       color = '')+
  #scale_color_manual(values = c("Solar in short wave"= "blue", "Solar out1 short wave" = "yellow","Solar out2 short wave" = "red"))+
  theme_bw() 

p4 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=batt_volt_avg, color = "Battery")) +
  scale_x_datetime()+
  labs(title ="Imnaviat Radation Station",
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

htmlwidgets::saveWidget(p, "imnav_rad.html")
