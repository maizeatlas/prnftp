##### Calculate the thermal time using TT20 formula (Parent & Tardieu 2012)

### Constantes 

Ha <- 76800   
Hd <- 285000
Sd <- 933
R <- 8.314

### Temperature in Kelvin unit
meteo_data_hourly$TK <- meteo_data_hourly$T2M + 273

### Fonction theorique a 20 degresC 
f20 <- (293*exp(-Ha/(R*293)))/(1+exp((Sd/R)-(Hd/(R*293))))

### Function of temperature at X°C 
meteo_data_hourly$Ft <- (meteo_data_hourly$TK*exp(-Ha/(R*meteo_data_hourly$TK))/(1+exp((Sd/R)-(Hd/(R*meteo_data_hourly$TK)))))


### Get the corresponding time spent at 20°C in hour 
#(number of hour(s) at 20°C corresponding to 1 hour at X°C)
meteo_data_hourly <- meteo_data_hourly %>% 
  dplyr::group_by(LAT, YEAR, MO, DY) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(tt20 = Ft/f20) %>%
  as.data.frame 

### Cumsum of the hours a 20°C (tt20) for each day and dividing per 24 to get the number of day at 20°C for each day
meteo_data_hourly  <- meteo_data_hourly %>% 
  dplyr::group_by(LAT, YEAR, MO, DY) %>%
  dplyr::mutate(sumtt20 = cumsum(tt20)) %>% #Get the cumulative hours at 20°C per day, (ultimately this will serve to get the number of day at 20°C)
  dplyr::ungroup() %>%
  as.data.frame()

### Get the total number of day at 20°C per calendar day    
summary_sumtt20 <- meteo_data_hourly %>% 
dplyr::group_by(LAT, YEAR, MO, DY)%>%
dplyr::arrange(YEAR, MO, DY, HR)%>%
dplyr::slice(n())  ## extract the total cumulated temperature at 20°C per day

