get.threshold.max.sss <- function(
  x,
  y,
  sp,
  return_maxsss = FALSE,
  plot_roc = FALSE,
  plot_sss = FALSE
){
  
  hs <- x[[1]] %>%
    raster::extract(
    y = y$pa_dat[[which(y$sp == sp)]]
  )
  
  thresholds <- seq(from = 0, to = 1, by = 0.001)
  
  patable <- tibble(
    hs = hs,
    pa = y$pa_dat[[which(y$sp == sp)]]$PA
  )
  
  sens <- sapply(
    X = thresholds,
    FUN = function(z, patable){
      pres <- patable %>%
        filter(pa == 1)
      
      sens <- length(which(pres$hs >= z))/length(pres$hs)
      
      return(sens)
    },
    patable = patable
  )
  
  
  spec <- sapply(
    X = thresholds,
    FUN = function(z, patable){
      absc <- patable %>%
        filter(pa == 0)
      
      spec <- length(which(absc$hs < z))/length(absc$hs)
      
      return(spec)
    },
    patable = patable
  )
  
  
  sss <- sens + spec
  
  maxsss <- max(sss)
  
  th_maxsss <- thresholds[which.max(sss)]
  
  if(plot_roc){
    plot(x = 1- spec, y = sens)
  }
  
  if(plot_sss){
    plot(x = thresholds, y = sss)
  }
  
  
  if(return_maxsss){
    result <- maxsss
  } else {
    result <- th_maxsss
  }
  
  return(result)
  
}