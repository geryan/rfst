logistic_sf <- function(x, y = 10, z = 0.4){
  
  v <- getValues(x)
  
  w <-  1/(1 + exp(-y*(v - z)))
  
  result <- x
  
  result[] <- w
  
  return(result)
  
}