lp <- function(x, twocol = FALSE, zlim = NULL, mm = NULL, ...){
  
  library(rasterVis)
  
  if(!is.null(mm)){
    zlim <- c(min(minValue(x)),
             max(maxValue(x)))
    
    x <- x[[mm]]
  }
  
  
  if(twocol){
    if(missing(...)){
      z <- levelplot(
        x,
        col.regions = magma(n = 16)[c(1,11)],
        at = c(0, 0.5, 1),
        margin = FALSE
      )
    } else {
      z <- levelplot(
        stack(x, ...),
        col.regions = magma(n = 16)[c(1,11)],
        at = c(0, 0.5, 1),
        margin = FALSE
      )
    }
  } else if(!is.null(zlim)){
    if(missing(...)){
      z <- levelplot(
        x,
        col.regions = magma(16), 
        at = seq(zlim[1], zlim[2], length=17),
        colorkey = list(
          at = seq(zlim[1], zlim[2], length=16),
          col = magma(16)
        ),
        margin = FALSE
      )
    } else {
      z <- levelplot(
        stack(x, ...),
        col.regions = magma(16), 
        at = seq(zlim[1], zlim[2], length=17),
        colorkey = list(
          at = seq(zlim[1], zlim[2], length=16),
          col = magma(16)
        ),
        margin = FALSE
      )
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