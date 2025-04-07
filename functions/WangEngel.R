##### Calculate the thermal time using formula (Wang & Engel 2017)


# Helper function ----

#slightly speeds up computation of functions of 1 argument when there are many more values in x than distinct values
apply_unique<- \(x, f) {
  ux <- unique(x)
  fux <- f(ux)
  fux[match(x, ux)]
}

# Thermal time computed as in Sirius Quality ----

# * Hourly air temperature from daily min and max ----

HourlyAirTemperature24 <- function(i, Tmin, deltaT, tsn, fac, ani, test, cas1, cas2, nightCoef) {
  testnuit <- i<=test
  deltaT * ifelse(testnuit, 
                  sin(pi*(i+cas1)/fac), 
                  tsn*exp(-nightCoef*(i-cas2)/ani)) + Tmin
}

HourlyAirTemperature <- function(Tmin, Tmax, julianDate, latitudeDeg, ..., maxLag=1.5, nightCoef=4, minLag=1) {
  latRad <- apply_unique(latitudeDeg, \(x) x*pi/180)
  
  jrange <- range(julianDate)
  adelt1 <- 0.4014*sin(2*pi*(seq(jrange[1],jrange[2])-77)/365)
  adelt <- adelt1[julianDate - jrange[1] + 1]
  
  tem1 <- sqrt(1-(tan(latRad)*adelt)^2)
  tem2 <- -tan(latRad)*tan(adelt)
  ahou <- atan2(tem1, tem2)
  ady <- ahou/pi*24
  ani <- 24-ady
  
  fac <- ady + maxLag*2
  ddy <- ady - minLag
  tsn <- sin(pi*ddy/fac)
  ady2 <- ady/2
  test <- floor(ady2)+ceiling(ady2)-minLag
  cas1 <- ady2%%1-1
  cas2 <- floor(ady2)+ady2
  
  deltaT <- Tmax-Tmin
  
  hTemp <-vapply(1:24, 
         HourlyAirTemperature24, 
         numeric(length(Tmin)), 
         Tmin, deltaT, tsn, fac, ani, test, cas1, cas2, nightCoef, 
         USE.NAMES = FALSE)
}


# * Growing degree days from hourly temperatures using Wang-Engel formula ----

WangEngel <- \(hTemp, n, ..., Tbase=10, TMin=-17, TOpt=31.5, TMax=43, TShape=1.5, TrefMaize=20) {
  out <- matrix(ncol=24, nrow=n)
  out[hTemp<TMin | hTemp>TMax] <- 0
  fillins <- is.na(out)
  alpha <- log(2)/log((TMax-TMin)/(TOpt-TMin))
  
  Dopt2 <- 2*(TOpt-TMin)^alpha
  Dtem <- (hTemp[fillins]-TMin)^alpha
  Dref <- (TrefMaize-TMin)^alpha
  DDo <- Dref*(Dopt2-Dref)
  Dbase <- TrefMaize-Tbase
  
  out[fillins] <- (Dbase*(Dtem*(Dopt2-Dtem)/DDo)^TShape)
  out
}

# * * Daily thermal time ----

dTTsum <- function(dTmin, dTmax, julianDate, latitudeDeg, ...) {
  hTemp <- HourlyAirTemperature(dTmin, dTmax, julianDate, latitudeDeg, ...)
  n <- length(dTmin)
  .rowMeans(WangEngel(hTemp, n, ...), n, 24)
}



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


##### Calculate the thermal time using GDD formula (Bonhomme et al. 1994)

GrowingDegDays<- function(maxT, minT, optT, baseT){
  if(maxT<=optT & minT>=baseT){
    return(((maxT+minT)/2)-baseT)
  } 
  else if(minT<baseT & (maxT<=optT & maxT>=baseT)){
    return(((maxT+baseT)/2)-baseT)
  } 
  else if(maxT>optT & minT>= baseT & optT>= minT){
    return(((optT - (maxT - optT))+minT)/2 - baseT)
  } 
  else if(maxT>optT & minT <baseT){
    return(((optT - (maxT - optT))+baseT)/2 - baseT)
  }
  else if(maxT<baseT & minT<baseT){
    return(0)
  }
  else if(maxT>optT & minT>optT){
    return(0)
  }
}






