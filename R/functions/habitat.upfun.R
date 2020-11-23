habitat.upfun <- function(x){
  
  if(x <= 100){
    y <- 0
  } else if(x <= 150){
    y <- x/50 - 2 
  } else {
    y <- 1
  }
  
  return(y)
  
}