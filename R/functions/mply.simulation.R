mply.simulation <- function(
  x,
  ntimesteps,
  nreplicates,
  ncores,
  proj_mask,
  out_path,
  save.sims = FALSE,
  ss.path = "output/pva_objects",
  save.pops = FALSE,
  sp.path = "output/pva_pops"){
  
  library(tibble)
  library(magrittr)
  library(dplyr)
  library(future)
  library(future.apply)
  
  #z <- mapply(
  z <- future_mapply(
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
      out_path = out_path,
      save.sims = save.sims,
      ss.path = ss.path,
      save.pops = save.pops,
      sp.path = sp.path
    ),
    future.packages = c(
      "steps",
      "raster",
      "sp"
    )
  )
  
  z <- apply(
    X = z,
    MARGIN = 2,
    FUN = as.simulation_results
  )
  
  zz <- tibble(pva_res = z)
  
  result <- bind_cols(x, zz)
  
  return(result)
  
}