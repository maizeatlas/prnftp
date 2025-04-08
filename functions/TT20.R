##### Calculate the thermal time using TT20 formula (Parent & Tardieu 2012)

### Constantes 

Ha <- 76800   
Hd <- 285000
Sd <- 933
R <- 8.314

#hTemp <- sample(seq(from = 1, to = 20, 1), size = 24, replace = T)

TT20 <- function(hTemp, n){
  TK <- hTemp + 273
  Ft <- TK*exp(-Ha/(R*TK))/(1+exp((Sd/R)-(Hd/(R*TK))))
  f20 <- (293*exp(-Ha/(R*293)))/(1+exp((Sd/R)-(Hd/(R*293))))
  tt20 = Ft/f20
  sumtt20 = cumsum(tt20)
  tt <- tail(sumtt20, 1)/n
  return(tt)
}


