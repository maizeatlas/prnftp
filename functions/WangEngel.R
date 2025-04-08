##### Calculate the thermal time using formula (Wang & Engel 2017)

## Factultative -- Sinusoidale transformation of daily data 
# If temperature data are recorded daily, they need to be transformed into hourly values using the sinusoidale function called here 

whose_dir <- "~/user_path"
functions_path <- paste0(whose_dir, "/functions_path/")

source(paste0(functions_path,"Sinusoidale.R"))


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






