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

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------

tabl_1 <-  "./current/CR23X_TWetLand.dat"

logger_file <- tabl_1
#****************************************************************************************************
# TWetland logger data are in CSI array data files
# importCSdata will read in multiple files and create a list of data frames for each file
# For ploting data frames of the met and soil data are extracted and limited to 4 months
#****************************************************************************************************
  
logger_data<- logger_file %>% map(function(x) importCSdata(x))
logger_data<- flatten(logger_data)
#lets see what was read in.
#map(logger_data,~names(.x))

met_data <-  logger_data[[1]] %>%
  rename(air_3m_avg = tair_avg, rh = rel_hum_avg) %>%
  clean_names() %>%
  arrange(timestamp)%>%
  filter(timestamp > max(timestamp)-months(1)) %>%
  select(timestamp,air_3m_avg,rh,tsoil_avg,wind_speed_s_wvt,wind_dir_d1_wvt,par_in_avg,battery,spn1_tot_avg,tsoil_2_avg,water_levl_avg)


#****************************************************************************************************
#Plots
#****************************************************************************************************

attach(met_data)

p1 <- ggplot(met_data) +
  geom_line(aes(x=timestamp, y=air_3m_avg, color = "Air Temperature"))  +
  geom_hline(aes(yintercept = 0))+
  scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
  scale_color_manual(values = c(
    'Air Temperature' = 'blue')) +
  labs(title = "TWetland Met Data",
       subtitle = "Air Temperature",
       x = "Date",
       y = "Degrees Celsius",
       color = '') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.2),
        legend.position = "top",
        axis.title.y = element_markdown(color = "black", size = 8))
p2 <- ggplot(met_data) +
  theme_bw() +
  geom_line(aes(x=timestamp,y=wind_speed_s_wvt,color="Wind Speed"),linewidth=.1,linetype="twodash")+
  scale_x_datetime()+
 # coord_cartesian(ylim = c(1500,2500)) +
  scale_color_manual(values = c(
    "Wind Speed" = "blue")) +
  labs(title = "1 meter Windspeed",
       x = "Date",
       y = "m/s",
       color = '')+
  theme(plot.title = element_text(hjust = 0.2),
        legend.position = "top",
        axis.title.y = element_markdown(color = "black", linewidth = 8))

p3 <- ggplot(met_data) +
  geom_line(aes(x=timestamp, y=battery, color = "volt")) +
  scale_x_datetime()+
  #coord_cartesian(ylim = c(300,500)) +
  labs(title ="Battery",
       x = "Date",
       y = "volt",
       color = '')+
  scale_color_manual(values = c("volt" = "red"))+
  theme(plot.title = element_text(hjust = 0.2),
        legend.position = "top",
        axis.title.y = element_markdown(color = "black", size = 8))+
  theme_bw() 

p4 <- ggplot(met_data) +
  geom_line(aes(x=timestamp, y=rh, color = "rh")) +
  scale_x_datetime()+
  #coord_cartesian(ylim = c(300,500)) +
  labs(title ="Battery",
       x = "Date",
       y = "percent",
       color = '')+
  scale_color_manual(values = c("rh" = "black"))+
  theme(plot.title = element_text(hjust = 0.2),
        legend.position = "top",
        axis.title.y = element_markdown(color = "black", size = 8))+
  theme_bw() 
#  the dynamicTicks needs to be true for the buttons to show
# autorange needs to be FALSE for range to work

# set the min and max for the initial x axis display in ggplotly
min_date <-max(met_data$timestamp)- lubridate::days(10)
max_date <-max(met_data$timestamp)

# Define xaxis options for using a range slider
xax<- list( 
  autorange=F,
  range= list(min_date, max_date),
  rangeselector = list(
    buttons = list(
      list(count = 7, label = "1 week", step ="day", stepmode = "backward"),
      list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
      list(step = "all")
    )),
  rangeslider = list(type = "date", thickness=0.05))

# Create 3 plots for the panel
p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
        layout(xaxis= xax,
               yaxis =list(zerolinewidth = .1)) %>% 
  partial_bundle()
p2_p <- ggplotly(p2,dynamicTicks = T) %>%
        layout(xaxis= xax,
               yaxis = list(autorange=T)) %>%
  partial_bundle()
p3_p <- ggplotly(p3,dynamicTicks = T) %>%
  layout(xaxis= xax,
         yaxis = list(autorange=T)) %>%
  partial_bundle()

p4_p <- ggplotly(p4,dynamicTicks = T) %>%
  layout(xaxis= xax,
         yaxis = list(autorange=T)) %>%
  partial_bundle()


p <- subplot(p1_p,p4_p,p2_p,p3_p, nrows=4, shareX = TRUE,titleY = T,heights = c(.2,.2,.3,.3),which_layout = 1) %>% 
  layout(title = 'Wet Sedge Wetland Station',margin = 0.01)
htmlwidgets::saveWidget(p, "Twetland.html", title = "TWetland ")
