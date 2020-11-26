habitat.upfun <- function(x, na.replace = TRUE){
  
  y <- sapply(
    X = x,
    FUN = function(x){
      if(is.na(x)){
        if(na.replace){
          return(1)
        } else{
          return(NA_real_)
        }
      }
      
      if(x <= 100){
        y <- 0
      } else if(x <= 150){
        y <- x/50 - 2 
      } else {
        y <- 1
      }
      
      return(y)
    }
  )
  
  
  return(y)
  
  
}