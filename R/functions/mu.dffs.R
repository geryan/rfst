# calculates mu parameter for LANDIS dynamic fire system

mu.dffs <- function(mean.fire, sigma){
  
  mu <- 2*log(log(mean.fire)) - sigma^2
  
  return(mu)
  
}