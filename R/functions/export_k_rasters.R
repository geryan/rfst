require(foreach)

export_k_rasters <- function(simulation_result, initial_k, period = 10, summary_function = mean) {
  # get the simulation timespan
  timespan <- length(simulation_result[[1]])
  # divide the timespan into groups based on period
  epochs <- split(1:timespan, ceiling(seq_along(1:timespan) / period))
  # get non-NA cell indices
  idx <- which(!is.na(simulation_result[[1]][[1]][[3]][]))
  # loop over list slots and summarise data (could be parallelised to increase speed)
  k_summaries <- foreach(e = 1:length(epochs)) %do% {
    # get k in list of vectors
    k <- lapply(epochs[[e]], FUN = function(x) simulation_result[[1]][[x]][[3]][idx])
    # covert list to matrix
    k <- do.call(cbind, k)
    # get means of all rows
    mat_calc <- apply(k, 1, summary_function)
    # create shell raster
    k_summary <- simulation_result[[1]][[1]][[3]]
    # assign new values
    k_summary[idx] <- round(mat_calc)
    # return summary
    k_summary
  }
  return(c(initial_k, k_summaries))
}