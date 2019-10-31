get_pop_simulation <- function(x, ...) {
  total_stages <- raster::nlayers(x[[1]][[1]]$population)
  timesteps <- length(x[[1]])
  sims <- length(x)
  
  pop_array <- array(dim=c(timesteps, total_stages, sims))
  
  for(i in seq_len(sims)) {
    pop_array[, , i] <- get_pop_replicate(x[[i]])
  }
  return(pop_array)
}

# from STEPS internal code https://github.com/steps-dev/steps/blob/d0cf24d0394732f26ea371b36d4e2f469e16a972/R/simulation_results-class.R#L881