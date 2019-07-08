vplot <- function(x, ...){
  library("viridis")
  plot(x, col = viridis(100), ...)
}

zplot <- function(x, ...){
  library("viridis")
  plot(x, col = viridis(100), zlim = c(0, 1), ...)
}