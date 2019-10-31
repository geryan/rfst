gps <- function(x, workers) {
  
  total_stages <- raster::nlayers(x[[1]][[1]]$population)
  
  timesteps <- length(x[[1]])
  
  sims <- length(x)
  
  pop_array <- array(dim=c(timesteps, total_stages, sims))
  
  for(i in seq_len(sims)) {
  
      pop_array[, , i] <- gpr(x[[i]], workers = workers)
  
  }
  
  return(pop_array)
  
}