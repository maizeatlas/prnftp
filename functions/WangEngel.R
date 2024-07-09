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

Tmin <- 1.28
Tmax <- 15.6
julianDate <- 1
latitudeDeg <- 30.5

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




