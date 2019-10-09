mply.simulation <- function(x, ntimesteps, nreplicates, ncores, proj_mask, out_path){
  
  library(tibble)
  library(magrittr)
  library(dplyr)
  
  z <- mapply(
    FUN = parsim,
    landscape = x$lsc,
    population_dynamics = x$pd,
    scn_id = x$scn_id,
    varset = x$varset,
    species = x$species,
    MoreArgs = list(
      timesteps = ntimesteps,
      replicates = nreplicates,
      workers = ncores,
      proj_mask = proj_mask,
      out_path = out_path
    )
  )
  
  z <- apply(
    X = z,
    MARGIN = 2,
    FUN = as.simulation_results
  )
  
  zz <- tibble(pva = z)
  
  result <- bind_cols(x, zz)
  
  return(result)
  
}