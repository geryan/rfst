lp <- function(x, ...){
  
  library(rasterVis)
  
  if(missing(...)){
    z <- levelplot(x, margin = FALSE)
  } else {
    z <- levelplot(stack(x, ...), margin = FALSE)
  }
  
  return(z)
  
}