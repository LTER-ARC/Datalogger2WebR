

# REQUIRED PACKAGES -----------------
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(stringr)
library(lubridate)
library(openxlsx)
library(rChoiceDialogs)
library(janitor)
# Functions ------------------------------------------
getwd()
source("importCSdata.v2.r")

#-----------------------------------------------------------------------
# Choose file
logger_file <- rchoose.files(caption = "Select dat files")
#-----------------------------------------------------------------------

#logger_date<- logger_file %>% map(function(x) importCSdata(x))

#For array files
#    **MIXED ARRAY DATA FILES**(Older array data without variable names.)
# importCSdata expects the .fsl file in the same directory
# with the same name as the data file but with .fsl extension
# Ths .fsl file has the column labels. A list of dataframes will
#be returned. The lists names will be the ID numbers.
logger_data <- importCSdata(logger_file)

#lets see what was read in.
map(logger_data,~names(.x))


soil_hourly <- logger_data$MNT_ID_210  %>% 
  clean_names() %>%
  mutate (timestamp = as.POSIXct(strptime(paste(as.Date(day_rtm,paste(year_rtm-1,12,31,sep = "-")),
                      sprintf("%04d",hour_minute_rtm)),"%Y-%m-%d %H%M"),tz="Etc/GMT+9")) %>%
  select(timestamp,everything()) %>%
  arrange(timestamp)
write.csv (soil_hourly, file = paste0(dirname(logger_file),"/soil_hourly.csv"))
plot_data <-half_hourly
#****************************************************************************************************
#MAT89 met data 
# CSI TOA5 data file create a data frame. Can choose multiple files
#****************************************************************************************************
logger_data <- importCSdata(logger_file) %>% #map_dfr(logger_files, importCSdata, .id = "source")  %>%
  clean_names() %>%
  arrange(timestamp)
#lets see what was read in.
names(logger_data)

#Plots


attach(logger_data)
# lims <- as.POSIXct(strptime(c("2019-12-14 00:00","2020-01-30 00:00"), format = "%Y-%m-%d %H:%M"))
 min_date <-max(timestamp)- lubridate::days(30)
 max_date <-max(timestamp)
# min_date_ms <- interval("1970-01-01 00:00:00", min_date) / dmilliseconds(1)
# max_date_ms <- interval("1970-01-01 00:00:00", max_date) / dmilliseconds(1)
# lims <- as.POSIXct(strptime(C(max(timestamp,lowerdate)), format = "%Y-%m-%d %H:%M"))
p1 <- ggplot(logger_data) +
  geom_line(aes(x=timestamp, y=ct_temp_avg, color = "control")) +
  geom_line(aes(x=timestamp, y=gh_temp_avg,color = "greenhouse")) +
  geom_line(aes(x=timestamp, y=sh_temp_avg,color = "shade")) +
  scale_x_datetime() +
  scale_color_manual(values = c(
    'control' = 'darkblue',
    'greenhouse' = 'green',
    'shade'= 'darkolivegreen')) +
  labs(title = "MAT89 Plot Air Temperatures",
       subtitle = "Control, Greenhouse and Shade",
       x = "Date Time",
       y = "Degrees Celsius",
       color = '') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")
p1
p2 <- ggplot(logger_data) +
  theme_bw() +
  geom_line(aes(x=timestamp,y=ct_rh,color="control RH"),size=.1,linetype="twodash")+
  geom_line(aes(x=timestamp,y=gh_rh,color="greenhouse RH"),size=.1,linetype="dotted")+
  geom_line(aes(x=timestamp,y=sh_rh,color="shade RH"),size=.1,linetype="dashed")+
  scale_x_datetime()+
  scale_color_manual(values = c(
    "control RH" = "darkblue",
    "greenhouse RH" = "green",
    "shade RH"= "darkolivegreen")) +
  labs(title = "MAT89 Air Temperature/RH",
       x = "Date Time",
       y = "Relative Humidity (%)",
       color = '')+
  theme(plot.title = element_text(hjust = 0.5))
p2
p3  <- ggplot(logger_data) +
  theme_bw() +
  geom_line(aes(x=timestamp,y=ctpar_den_avg,color="control PAR"),size=.1)+
  geom_line(aes(x=timestamp,y=ghpar_den_avg,color="greenhouse PAR"),size=.1)+
  geom_line(aes(x=timestamp,y=shpar_den_avg,color="shade PAR"),size=.1)+
  scale_x_datetime()+
  labs(title ="MAT89 Photosynthetically active radiation (PAR)",
       x = "Date",
       y = (expression(paste("PAR ", mu,"mol m-2 s-1"))),
       color = '')+
  theme(plot.title = element_text(hjust = 0.5))

p3
p4  <- ggplot(plot_data) +
  geom_line((aes(x=timestamp, y=CT_Temp_AVG)), color="blue") +
  geom_line((aes(x=timestamp, y=CT_GH_temp_diff))) +
  scale_x_datetime()

#  the dynamicTicks needs to be true for the buttons to show
# autorange needs to be FALSE for range to work
p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
        layout(xaxis= list( 
          autorange=F,
          range= list(min_date, max_date),
          rangeselector = list(
            buttons = list(
              list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
              list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(step = "all")
              )),
          rangeslider = list(type = "date", thickness=0.05)))
p1_p
p2_p <- ggplotly(p2,dynamicTicks = T) %>%
        layout(xaxis= list( 
              autorange=F,
              range= list(min_date, max_date),
        rangeselector = list(
          buttons = list(
            list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
            list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
            list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
            list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
            list(step = "all")
          )),
        rangeslider = list(type = "date", thickness=0.05)))
p2_p  %>%
  partial_bundle()
p1_p  %>% partial_bundle()

p2_pa <- ggplotly(p2,dynamicTicks = T)

p <- subplot(p2_p,p1_p, nrows=2, shareX = TRUE,titleY = T,heights = c(.4,.6))
p
htmlwidgets::saveWidget(p, "index.html")
