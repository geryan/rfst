mply.landscape <- function(x, ccfun){
  
  library(tibble)
  library(magrittr)
  library(dplyr)
  
  z <- mapply(
    FUN = landscape,
    population = x$init_pop,
    suitability = x$predmaps,
    "mortality" = x$mort,
    MoreArgs = list(
      carrying_capacity = ccfun
    )
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