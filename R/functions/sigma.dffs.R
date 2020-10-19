# calculates the sigma parameter for LANDIS dynamic fuel and fire system


sigma.dffs <- function(mean.fire, sd.fire){
  
  sigma <- (-log((exp(4*log(log(mean.fire))) - log(sd.fire))/(exp(4*log(log(mean.fire))))))^(1/2)
  
  return(sigma)
  
  
}