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
