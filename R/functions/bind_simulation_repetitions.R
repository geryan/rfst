bind_simulation_repetitions <- function(arglist){ # means of overcoming memory limitations to do large number of repetitions of simulation - takes list of simulation results objects from same landscape and number of timesteps and returns single combined results object
  
  if(!is.list(arglist)){stop("Please pass simulation results in a list")}
  
  for(i in 1:length(arglist)){
    if(!is.simulation_results(arglist[[i]])) stop("Arguments are not STEPS simulation results")
  }
  
  for( i in 2:length(arglist)){
    
    if(length(arglist[[1]][[1]]) != length(arglist[[i]][[1]])){
      stop("Results contain differing number of timesteps")
    }
    
    if(!all.equal(arglist[[1]][[1]][[1]]$suitability, arglist[[i]][[1]][[1]]$suitability)){
      stop("Results contain differing habitat suitability layers")
    }
  }
  
  
  sim_results <- do.call(c, arglist)
  
  class(sim_results) <- c("simulation_results", "list")
  
  return(sim_results)
  
}