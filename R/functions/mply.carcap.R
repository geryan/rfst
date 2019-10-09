mply.carcap <- function(x, carcap, proj_mask, out_path = "output/pva_vars"){
  
  ## TOTALLY INCOMPLETE
  
  library(tibble)
  library(dplyr)
  
  z <- mapply(
    FUN = mapcc,
    inputlist,
    cc = x$cc,
    popsize = x$popsize,
    varset = x$varset,
    species = x$species,
    scn_id = x$scn_id,
    tm = x$tm,
    MoreArgs = list(
      proj_mask = proj_mask,
      out_path = out_path
    )
  )
  
  zz <- tibble(init_pop = z)
  
  result <- bind_cols(x, zz)
  
  return(result)
  
}