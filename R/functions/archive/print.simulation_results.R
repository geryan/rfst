print.simulation_results <- function (x, print.as.list = FALSE, ...) {
  if(print.as.list){
    print(x)
  } else{
    cat("STEPS simulation results for", length(x), "replicates over", length(x[[1]]), "years")
  }
}