logistic_sf <- function(x){
  
  v <- getValues(x)
  
  z <-  1/(1 + exp(-10*(v - 0.1)))
  
  result <- x
  
  result[] <- z
  
  return(result)
  
}