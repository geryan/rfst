lp <- function(x, twocol = FALSE, ...){
  
  library(rasterVis)
  
  if(twocol){
    if(missing(...)){
      z <- levelplot(x, col.regions = magma(n = 16)[c(1,11)], at = c(0, 0.5, 1), margin = FALSE)
    } else {
      z <- levelplot(stack(x, ...), col.regions = magma(n = 16)[c(1,11)], at = c(0, 0.5, 1), margin = FALSE)
    }
  }else{
    if(missing(...)){
      z <- levelplot(x, margin = FALSE)
    } else {
      z <- levelplot(stack(x, ...), margin = FALSE)
    }  
  }
  
  
  return(z)
  
}