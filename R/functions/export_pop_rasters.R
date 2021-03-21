require(foreach)

export_pop_rasters <- function(simulation_result, initial_pops, period = 10, summary_function = mean) {
  # get the simulation timespan
  timespan <- length(simulation_result[[1]])
  # divide the timespan into groups based on period
  epochs <- split(1:timespan, ceiling(seq_along(1:timespan) / period))
  # get non-NA cell indices
  idx <- which(!is.na(simulation_result[[1]][[1]][[1]][[1]][]))
  # loop over list slots and summarise data (could be parallelised to increase speed)
  pop_summaries <- foreach(e = 1:length(epochs)) %do% {
    # get pops in list of matrices
    pops <- lapply(epochs[[e]], FUN = function(x) simulation_result[[1]][[x]][[1]][idx])
    # covert list to array
    pops <- array(unlist(pops), dim = c(nrow(pops[[1]]), ncol(pops[[1]]), length(pops)))
    # get means of all cells (CHANGE THIS FOR OTHER SUMMARIES)
    array_calc <- apply(pops, c(1, 2), summary_function)
    # create shell raster
    pop_summary <- simulation_result[[1]][[1]][[1]]
    # assign new values
    pop_summary[idx] <- round(array_calc)
    # return summary
    pop_summary
  }
  return(c(initial_pops, pop_summaries))
}