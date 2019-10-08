mply.landscape <- function(x){
  
  library(tibble)
  library(magrittr)
  library(dplyr)
  
  z <- mapply(
    FUN = landscape,
    population = x$init_pop,
    suitability = x$hs,
    carrying_capacity = x$cc,
    "mortality" = x$mort
  )
  
  z <- apply(
    X = z,
    MARGIN = 2,
    FUN = as.landscape
  )
  
  zz <- tibble(lsc = z)
  
  result <- bind_cols(x, zz)
  
  return(result)
  
}