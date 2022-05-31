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

tabl_1 <-  "./current/LMAT_Blk1_Met.dat"
#tabl_2 <-  "./current/LMAT_BLK3_Soil.dat"
#logger_file <- c(tabl_1,tabl_2)
logger_file <- tabl_1
#****************************************************************************************************
# MAT06 logger data are in CSI TOA5 data files
# importCSdata will read in multiple files and create a list of data frames for each file
# For ploting data frames of the met and soil data are extracted and limited to 4 months
#****************************************************************************************************
  
logger_data<- logger_file %>% map(function(x) importCSdata(x))

#lets see what was read in.
map(logger_data,~names(.x))

met_data <-  logger_data[[1]] %>%
  rename(ct_air_3m_avg = air_3m_avg, ct_rh = rh) %>%
  clean_names() %>%
  arrange(timestamp)%>%
  filter(timestamp > max(timestamp)-months(2))

# soil_data <- logger_data[[2]]  %>% 
#   clean_names() %>%
#   arrange(timestamp)%>%
#   filter(timestamp > max(timestamp)-months(2))

#****************************************************************************************************
#Plots
#****************************************************************************************************

attach(met_data)

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
  geom_line(aes(x=timestamp,y=ct_rh,color="control RH"),size=.1,linetype="twodash")+
  geom_line(aes(x=timestamp,y=gh_rh,color="greenhouse RH"),size=.1,linetype="dotted")+
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
#  the dynamicTicks needs to be true for the buttons to show
# autorange needs to be FALSE for range to work

# set the min and max for the initial x axis display in ggplotly
min_date <-max(timestamp)- lubridate::days(5)
max_date <-max(timestamp)

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

# Create 3 plots for the panel
p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
        layout(xaxis= xax,
               yaxis =list(zerolinewidth = .1)) %>% 
  partial_bundle()
p2_p <- ggplotly(p2,dynamicTicks = T) %>%
        layout(xaxis= xax) %>%
  partial_bundle()

p3_p <- ggplotly(p3,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,range= list(min_date, max_date)),
    yaxis = list(autorange = FALSE)
  ) %>% 
  partial_bundle()

p <- subplot(p2_p,p1_p,p3_p, nrows=3, shareX = TRUE,titleY = T,heights = c(.3,.4,.3),which_layout = 2)
#p <- subplot(p3_p,p2_p,p1_p, nrows=3, shareX = TRUE,titleY = T,heights = c(.2,.2,.6))
htmlwidgets::saveWidget(p, "mat2006Blk1met.html",title = "MAT06 Met")

#Soil
sp1 <- met_data %>% select(timestamp,intersect(contains("ct"), contains("avg"))) %>%
  gather("key", "value", -timestamp)%>%
  ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
  scale_x_datetime()+
  coord_cartesian(ylim = c(-15,15)) +
  labs(title ="MAT2006-Blk1 CT Soil Temperature",
       x = "Date",
       y = "Celsius ",
       color = 'Legend')+
  theme_bw() +
  geom_line(aes(color = key),size=.1)

sp2 <- met_data %>% select(timestamp,intersect(contains("gh"), contains("avg"))) %>%
  gather("key", "value", -timestamp) %>%
  ggplot(data=., aes(x=timestamp, y = value,color=key)) +
  scale_x_datetime()+
  labs(title ="MAT2006-Blk1 CT and GH Soil Temperature",
       x = "Date",
       y = "Celsius ",
       color = 'Legend')+
  coord_cartesian(ylim = c(-15,15)) +
  theme_bw() +
  geom_line(aes(color = key),size=.1)

sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,
    range= list(min_date, max_date)),
    yaxis = list(autorange=F,range = c(-15, 15))
  ) %>% 
  partial_bundle()
sp2_p <- ggplotly(sp2,dynamicTicks = T) %>% 
  layout(xaxis=  list( 
    autorange=F,
    range= list(min_date, max_date)),
    yaxis = list(autorange=F,range = c(-15, 15))
  ) %>% 
  partial_bundle()
p <- subplot(sp1_p,sp2_p, nrows=2, shareX = TRUE,titleY = T)
htmlwidgets::saveWidget(p, "mat2006Blk1soil.html", title = "MAT06 soil")



