as_class <- function (object, name, type = c("function", "list")) {
  type <- match.arg(type)
  stopifnot(inherits(object, type))
  class(object) <- c(name, class(object))
  invisible(object)
}

as.landscape <- function (landscape) {
  as_class(landscape, "landscape", "list")
}

as.population_growth <- function (simple_growth) {
  as_class(simple_growth, "population_growth", "function")
}

as.simulation_results <- function (simulation_results) {
  as_class(simulation_results, "simulation_results", "list")
}