mean.fire.dffs <- function(mu, sigma){
  
  mean.fire <- exp(exp((mu + sigma^2)/2))
  
  return(mean.fire)
  
}