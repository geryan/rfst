gpr <- function(x, workers) {
  
  library(future)
  library(future.apply)
  
  plan(multisession, workers = workers)
  
  total_stages <- raster::nlayers(x[[1]]$population)
  
  idx <- which(!is.na(raster::getValues(x[[1]]$population[[1]])))
  
  pops <- future_lapply(x, function(x) raster::extract(x$population, idx))
  
  pop_sums <- future_lapply(pops, function(x) colSums(x))
  
  pop_matrix <- matrix(unlist(pop_sums), ncol = total_stages, byrow = TRUE)
  
  return(pop_matrix)
  
}