parsim <- function(
  landscape,
  population_dynamics,
  timesteps,
  replicates,
  workers,
  proj_mask,
  out_path,
  scn_id,
  varset,
  species,
  cc = TRUE,
  save = FALSE){
  
  library(future)
  library(future.apply)
  
  li <- vector("list", replicates)
  
  plan(multisession, workers = workers)
  
  result <- future_lapply(X = li,
                          FUN = simulation,
                          landscape = landscape,
                          population_dynamics = population_dynamics,
                          timesteps = timesteps,
                          replicates = 1,
                          verbose = FALSE)
  
  result <- bind.simulation.repetitions(result)
  
  registerDoMC(cores = workers)
  
  foreach (i = 1:replicates) %do% {
    foreach(j = 1:timesteps) %dopar% {
      Newborn <- rst.op(input1 = result[[i]][[j]][[1]][[1]],
                        op = "writeonly",
                        proj_mask = proj_mask,
                        filename = sprintf(
                          "%s/simpop_%s_%s_%s_%s_%s_n.grd",
                          out_path,
                          scn_id,
                          varset,
                          species,
                          i,
                          j),
                        layernames = "Newborn")
      
      Juvenile <- rst.op(input1 = result[[i]][[j]][[1]][[2]],
                         op = "writeonly",
                         proj_mask = proj_mask,
                         filename = sprintf(
                           "%s/simpop_%s_%s_%s_%s_%s_j.grd",
                           out_path,
                           scn_id,
                           varset,
                           species,
                           i,
                           j),
                         layernames = "Juvenile")
      
      Adult <- rst.op(input1 = result[[i]][[j]][[1]][[3]],
                      op = "writeonly",
                      proj_mask = proj_mask,
                      filename = sprintf(
                        "%s/simpop_%s_%s_%s_%s_%s_a.grd",
                        out_path,
                        scn_id,
                        varset,
                        species,
                        i,
                        j),
                      layernames = "Adult")
      
      if(cc){
        carrying_capacity <- rst.op(input1 = result[[i]][[j]][[3]],
                                    op = "writeonly",
                                    proj_mask = proj_mask,
                                    filename = sprintf(
                                      "%s/cc_%s_%s_%s_%s_%s.grd",
                                      out_path,
                                      scn_id,
                                      varset,
                                      species,
                                      i,
                                      j),
                                    layernames = sprintf("Carrying_Capacity_%s", j))
      }
      
    }
  }
  
  for (i in 1:replicates){
    for(j in 1:timesteps){
      
      Newborn <- raster(
        sprintf(
          "%s/simpop_%s_%s_%s_%s_%s_n.grd",
          out_path,
          scn_id,
          varset,
          species,
          i,
          j)
      )
      
      Juvenile <- raster(
        sprintf(
          "%s/simpop_%s_%s_%s_%s_%s_j.grd",
          out_path,
          scn_id,
          varset,
          species,
          i,
          j)
        )
      
      Adult <- raster(
        sprintf(
          "%s/simpop_%s_%s_%s_%s_%s_a.grd",
          out_path,
          scn_id,
          varset,
          species,
          i,
          j)
      )
      
      result[[i]][[j]][[1]] <- stack(Newborn, Juvenile, Adult)
      
      if(cc){
        carrying_capacity <- raster(
          sprintf(
            "%s/cc_%s_%s_%s_%s_%s.grd",
            out_path,
            scn_id,
            varset,
            species,
            i,
            j)
        )
        
        result[[i]][[j]][[3]] <- carrying_capacity
        
      }
    }
  }
  
  gc(verbose = FALSE)
  
  if(save){
    saveRDS(
      object = result,
      file = sprintf(
        "%s/pva_%s_%s_%s.Rds",
        "output/pva_objects",
        scn_id,
        varset,
        species
      )
    )
  }
  
  return(result)
}