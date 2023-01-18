
importCSdata <- function(filename,retopt="data"){
  #************************************************************************************************
  # Import Campbell Scientific data logger files. CSI TOA5 and mixed array files can be imported.
  # For TO5A files the timezone of the TIMESTAMP column will be set to the time zone specified by 
  # time_zone variable.  For mixed arrays the column information is parsed from the .fsl file which
  # should be in the same directory as the .dat file. Mixed array data will return a list of dataframes
  # with each dataframe named "ID_xxx"  where xxx is the array id number.
  # Uisng clean_names to cleanup the variable names.  For mixed arrays file a variable "timestamp" is 
  # calculated from the year_rtm, day_rtm and hour_minute_rtm.
  # TODO add error checks
  #************************************************************************************************
  library(tidyverse)
  library(janitor)
  library(rstudioapi)
  
  time_zone <- "Etc/GMT+9" #Alaska standard time
     # site name from the file name without the file extension and path
  site <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filename))
  #print (site)
    if(retopt=="info"){
    		  # bring in entire header of CSI TOA5 data file for metadata
    		stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
    		return(stn.info)
    	} else {
    	  	# Get the data but first read second line of file which if a TOA5 file will contains the column names
    	    # If it does not then assume it is a mixed array file
    		header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
      		if (header[1]=="TIMESTAMP") { # CSI TOA5 file
        		stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
        		names(stn.data) <- header
        		  # R-format timestamp as a R POSIXct date/timestamps with time zone specified above
        		stn.data <-stn.data %>% clean_names()%>%
        		  mutate(timestamp= parse_date_time(timestamp, "ymd HMS",tz=time_zone ))
        		  #MIXED ARRAY DATA FILES with no header row
      		} else {  
      		    # Read the .fsl file to get the headers. The .fsl file should be in the same directory
      		    # with the same name as the data file but with .fsl extension.
      		  h_filename <- sub("\\.dat$","\\.fsl",filename)
      		  if (!file.exists(h_filename)) {h_filename <- rstudioapi::selectFile(caption = "Select .fsl file")}
      		  header <- read.table(file=h_filename,skip=4, header = FALSE, stringsAsFactors=FALSE, 
      		                       fill = TRUE,col.names = c("var_no","var_names","var_L","extra"))
          		  # Column names will have "L" in the "var_L" column. Variable "var_no" is a sequence of numbers 
          		  # for each array. Separate the groups of labels by using the diff function on "var_no". When the lag 
          		  # difference is not equal to one it's the start of another group. Function cumsum will sum "TRUE", e.g 
          		  # when diff is not=1. The first label of a group is the array ID and is used to replaced the values in 
          		  # "table_id". The dataframe is then split into a list of dataframes using values in "tabl_id"
          	  header <-header %>% 
      		    subset(var_L =="L") %>%   # Keep only labels
      		    mutate(tabl_id = cumsum(c(1, diff(as.numeric(var_no))) != 1)) %>% # find sequence; cumsum the true, e.g when diff is not=1
      		    mutate(tabl_id = paste(site,"ID",var_names[match(tabl_id,tabl_id)], sep= "_")) %>% #replace with the Site name  and ID number
      		    select(var_names,tabl_id) %>% 
      		    split.data.frame(.$tabl_id) %>%
      		    map( ~ (.x %>% select(var_names))) # keep only the variable name column.
      		     # Now read in the data file with the mixed arrays of data
      		  ncol <- max(count.fields(filename, sep = ",")) # get max number of columns in file since first row is not the max
      		  stn.data <- read.csv(filename,header=FALSE, fill = TRUE,col.names = paste0("V", seq_len(ncol)))
      		  stn.data <- split.data.frame(stn.data,stn.data$V1)   # Split into dataframes using V1 column values, e.g ID number
      		  names(stn.data) <- paste(site,"ID",names(stn.data),sep="_")# Rename dataframes to match the format of header file
      		     # Rename columns and eliminate excess columns in each "ID_xx" dataframe using the var_names column
      		     # from header matching "ID_xx" dataframe
      		  for (nm in names(stn.data)) {
      		    stn.data[nm][[1]]<- stn.data[nm][[1]][1:nrow(header[[nm]])]
      		    names(stn.data[nm][[1]])<-make_clean_names(header[[nm]][["var_names"]] )
      		    hourexist <-("hour_minute_rtm" %in% names(stn.data[nm][[1]])) #Check if a daily array with no hour_minute
      		    stn.data[nm][[1]]<- stn.data[nm][[1]] %>%
      		      mutate(timestamp = 
      		               if(hourexist) {
      		                 parse_date_time(paste(as.Date(paste(day_rtm,year_rtm), format="%j %Y"),
      		                                           sprintf("%04d",as.integer(hour_minute_rtm))),"%Y-%m-%d %H%M",tz=time_zone)
      		               } else {
      		                 parse_date_time(paste(as.Date(paste(day_rtm,year_rtm), format="%j %Y"),
      		                                           "0000"),"%Y-%m-%d %H%M",tz=time_zone)
      		               }          
      		      ) %>%
      		      select(timestamp,everything()) %>%
      		      arrange(timestamp)
      		  } # end for
    		  } #end else for array file
    	} # end else for FileType

  	  return(stn.data)
}

