tsl.agg <- function(x, na.rm = FALSE){
  
  if(na.rm){
    
    result <- mean(x, na.rm = TRUE)
    
    return(result)
    
  } else {
    
    x[which(is.na(x))] <- 250
    
    result <- mean(x)
    
    return(result)
    
  }
  
}