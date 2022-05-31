## Read data from Campbell Sci logger files.  
##  
## For table loggers, i.e. TOA5 files
## column names are in the 2nd row of the file
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

logger_file <-  "./current/TlkLakeStn.dat"

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
  geom_line(aes(x=timestamp, y=air_tc_avg, color = "Air Temperature")) +
  scale_x_datetime()+
  labs(title ="Air Temperature",
       x = "Date",
       y = "celsius ",
       color = '')+
  theme_bw() 

p2 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=rh_avg, color = "Relative Humidity")) +
  scale_x_datetime()+
  labs(title ="Relative Humidity",
       x = "Date",
       y = "Percent",
       color = '')+
  scale_color_manual(values = c("Relative Humidity" = "blue"))+
  theme_bw() 
# Wind sensors
wp1 <- logger_data %>% select(timestamp,ws_ms_s_wvt) %>%
  gather("key", "value", -timestamp)%>%
  ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
  scale_x_datetime()+
  #coord_cartesian(ylim = c(-15,15)) +
  labs(x = "Date",
       y = "Wind Speed (m/s)",
       color = 'Legend')+
  theme_bw() +
  geom_line(color="red",size=.1)+
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
  geom_line(aes(x=timestamp, y=batt_volt_avg, color = "Battery")) +
  scale_x_datetime()+
  labs(title ="Toolik Lake Float Station",
       x = "Date",
       y = "volts",
       color = '')+
  coord_cartesian(ylim = c(8,15)) +
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
    autorange=F,range= list(min_date, max_date)),
    yaxis = list(autorange = FALSE)
  ) %>% 
  partial_bundle()

w1_p <- ggplotly(wp1,dynamicTicks = T)
w2_p <- ggplotly(wp2,dynamicTicks = T)

p <- subplot(p1_p,p2_p,w1_p,w2_p, p3_p, nrows=5, shareX = TRUE,titleY = T,heights = c(.2,.2,.2,.2,.1))

#htmlwidgets::saveWidget(p, "tlk_lake.html")

# T-chain plot
# Palette that is good for color blind
cbPalette15 <- c("#999999","#68023F","#008169","#EF0096","#00DCB5","#FFCFE2","#003C86","#9400E6","#009FFA","#FF71FD","#7CFFFA","#6A0213","#008607","#F60239","#00E307","#FFDC3D")
#Order and label the columns by shallow to deep depth.
tc_order <- c("temp0_avg","temp25_avg","temp50_avg","temp75_avg","temp1_avg","temp2_avg","temp3_avg","temp4_avg","temp5_avg","temp6_avg","temp7_avg","temp8_avg","temp10_avg","temp12_avg","temp15_avg","temp18_avg")
tc_labels <- c("0m","0.25m","0.5m","0.75m","1m","2m","3m","4m","5m","6m","7m","8m","10m","12m","15m","18m")

tp1 <- logger_data %>% select(timestamp,starts_with("temp")) %>%
  gather("key", "value", -timestamp)%>%
  mutate (key = factor(key, levels = tc_order, labels = tc_labels) )%>%
  ggplot(.,aes(x=timestamp, y = value,color=key)) +
  scale_colour_manual(values=cbPalette15) +
  scale_x_datetime()+
  coord_cartesian(ylim = c(-15,15)) +
  labs(title ="T-chain Temperature",
       x = "Date",
       y = "Celsius ",
       color = 'Legend')+
  theme_bw() +
  geom_line(aes(color = key))
# interactive plotly graph
tp1_p <- ggplotly(tp1,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,
    range= list(min_date, max_date)),
    yaxis = list(autorange=F,range = c(4, 15))
  ) %>% 
  partial_bundle()
wgp <- toWebGL(tp1_p)
p <- toWebGL(p)

saveWidget(p, "tlk_lake.html",title = "Toolik Lake Met",  selfcontained = F, libdir = "lib")
saveWidget(wgp, "tlk_lake_tc.html", title = "T-Chian", selfcontained = F, libdir = "lib")
