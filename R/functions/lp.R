lp <- function(x, twocol = FALSE, zlim = NULL, mm = NULL, names.attr = NULL, ...){
  
  library(rasterVis)
  library(viridis)
  
  if(!is.null(mm)){
    zlim <- c(min(minValue(x)),
             max(maxValue(x)))
    
    x <- x[[mm]]
  }
  
  if(is.null(names.attr)){
    names.attr <- names(x)
  }
  
  if(twocol){
    if(missing(...)){
      z <- levelplot(
        x,
        col.regions = magma(n = 16)[c(1,11)],
        at = c(0, 0.5, 1),
        margin = FALSE,
        names.attr
      )
    } else {
      z <- levelplot(
        stack(x, ...),
        col.regions = magma(n = 16)[c(1,11)],
        at = c(0, 0.5, 1),
        margin = FALSE,
        names.attr
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
        margin = FALSE,
        names.attr
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
        margin = FALSE,
        names.attr
      )
    }
  }else{
    if(missing(...)){
      z <- levelplot(x, margin = FALSE, names.attr)
    } else {
      z <- levelplot(stack(x, ...), margin = FALSE, names.attr)
    }  
  }
  
  
  return(z)
  
}