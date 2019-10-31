get_pop_replicate <- function(x, ...) {
  total_stages <- raster::nlayers(x[[1]]$population)
  idx <- which(!is.na(raster::getValues(x[[1]]$population[[1]])))
  pops <- lapply(x, function(x) raster::extract(x$population, idx))
  pop_sums <- lapply(pops, function(x) colSums(x))
  pop_matrix <- matrix(unlist(pop_sums), ncol = total_stages, byrow = TRUE)
  return(pop_matrix)
}

# from STEPS https://github.com/steps-dev/steps/blob/d0cf24d0394732f26ea371b36d4e2f469e16a972/R/simulation_results-class.R#L872