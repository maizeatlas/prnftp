

################################# Load Function ###############################

##### Function to get the daylength at any DOY/LAT 

Daylength<- function(lat, day) {
  TrenchR::daylength(lat, day) }

##### Function color 
# This function serves to highlight which day correspond to Q1, Q2 and Q3 per env and per year 

color <- function(dataset1, LAT1, DOY, YEAR, Quartile,  i){ #EID1
  dataset <- subset(dataset1, LAT== LAT1 & year == YEAR) #& year == YEAR, -
  color_vector <- NULL
  print(i)
  # i <- 177
  list <- as.numeric(unlist(dataset[,DOY]))
  list_quartile <-  as.numeric(unlist(dataset[,Quartile]))
  n <- unique(list_quartile[which(list==i)])
  color<-  ifelse(i %in% list, ifelse(list_quartile[[n]] == 1 , "#00AFBB", ifelse(list_quartile[[n]] ==2, "pink", ifelse(list_quartile[[n]] ==3, "yellow", "black"))), "black") #ifelse(dataset[,Quartile][which(dataset[,Quartile]==dataset[,DOY])]==2, "pink", ifelse(dataset[,Quartile][which(dataset[,Quartile]==dataset[,DOY])]==3, "yellow", "black"), "black"), "black"), "black")
  color_vector <- rbind(color_vector, color)
}

# dataset1 <- IntervalDaylength_doy
# LAT1 <- 49.63
# YEAR <- "2014"
# day <- daylength_year_all_lat$Day[which(daylength_year_all_lat$LAT== 13.75776)]
# DOY <- "doy"
# Quartile <- "quartile"
# daylength_year_all_18 <- subset(daylength_year_all_lat, LAT== LAT1)
# A = 1
# YEAR = unique(IntervalDaylength_doy$year[which(IntervalDaylength_doy$LAT== 13.75776)])


##### function convert minutes in HR:MM:SS 

convert_minutes <- function(minutes) {
  hours <- floor(minutes / 60)
  minutes <- minutes %% 60
  seconds <- (minutes - floor(minutes)) * 60
  return(paste0(hours, ":", floor(minutes), "'", round(seconds), "''"))
}

##### Function to cumsum TT between two dates

GDD_cum_date <- function(dataset, LAT1, ENV1, threshold_max, threshold_min, DATE, TT){ 
  dataset1 <- subset(dataset, LAT == LAT1 & env == ENV1) ### subset by location
  vector <- dataset1[,TT][dataset1[,DATE] >= threshold_min & dataset1[,DATE] <= threshold_max] ### cumsum between emergence and anthesis
  i <- cumsum(vector)
  value_cumTT <- tail(i,1)
  return(value_cumTT)
}

# dataset <- meteo_data_hourly_out_git
LAT1 <- "35.66883"
ENV1 <- "2006_NC"
# threshold_min <- "2003-05-20"
# threshold_max <- "2003-08-01"
# GDD <- "Tavg"
# DATE <- "YYYYMMDD"


##### Function to compute thermal time from emergence to TI

Date_TI <- function( dataset, LAT1,  ENV1,  start, threshold, DATE, TT){ #ENV1,
  dataset1 <- subset(dataset, LAT == LAT1 & env == ENV1) #& env == ENV1
  DATE1 <- dataset1[,DATE]
  vector <- DATE1[DATE1 >= start]
  # dataset1[,TT][is.na(dataset1[,TT])] <- 0
  i <- which.min(abs(threshold - cumsum((dataset1[,TT][which(DATE1>= start)]))))
  Date <- vector[[i]]  
  return(Date)
}

# dataset = meteo_data_hourly_out_git
LAT1  = "35.66883"
start = "2005-01-05" 	
ENV1 = "2005_NC"
threshold = 234
DATE = "YYYYMMDD"
GDD = "WangEngel"


