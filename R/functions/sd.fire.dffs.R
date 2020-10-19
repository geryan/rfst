sd.fire.dffs <- function(mu, sigma){
  
  sd.fire <- exp((exp(2*mu + sigma^2)*(exp(sigma^2) - 1))^(1/2))
  
  return(sd.fire)
  
}