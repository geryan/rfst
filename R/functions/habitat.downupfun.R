habitat.downupfun <- function(x){
  
  if(x <= 5){
    y <- 1-x/5
  } else if(x <= 100){
    y <- 0
  } else if(x <= 150){
    y <- x/50 - 2 
  } else {
    y <- 1
  }
  
  return(y)
  
}