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
  species){
  
  library(future)
  library(future.apply)
  
  li <- vector("list", replicates)
  
  plan(multiprocess, workers = workers)
  
  result <- future_lapply(X = li,
                          FUN = simulation,
                          landscape = landscape,
                          population_dynamics = population_dynamics,
                          timesteps = timesteps,
                          replicates = 1)
  
  result <- bind.simulation.repetitions(result)
  
  foreach (i = 1:replicates) %do% {
    foreach(j = 1:timesteps) %dopar% {
      Newborn <- rst.op(input1 = result[[i]][[j]][[1]],
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
      
      Juvenile <- rst.op(input1 = result[[i]][[j]][[1]],
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
      
      Adult <- rst.op(input1 = result[[i]][[j]][[1]],
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
      result[[i]][[j]] <- stack(Newborn, Juvenile, Adult)
      
      names(result[[i]][[j]]) <- c("Newborn", "Juvenile", "Adult") 
    }
  }
  
  return(result)
}