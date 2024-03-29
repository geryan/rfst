mply.initpop <- function(x, cc, proj_mask, out_path = "output/pva_vars"){
  
  library(tibble)
  library(magrittr)
  library(dplyr)
  
  library(future)
  library(future.apply)
  
  z <- future_mapply(
    FUN = initpop,
    hs = x$hs,
    popsize = x$popsize,
    varset = x$varset,
    species = x$species,
    scn_id = x$scn_id,
    tm = x$tm,
    MoreArgs = list(
      cc = cc,
      proj_mask = proj_mask,
      out_path = out_path
    ),
    future.packages = c("raster")
  )
  
  zz <- tibble(init_pop = z)
  
  result <- bind_cols(x, zz)
  
  return(result)
  
}