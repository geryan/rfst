altm <- function(x){
  
  y <- dim(x)
  
  z <- x
  
  z[1,] <- x[1,]*x[y[1],]
  
  return(z)
  
}
# function converts naiive post-breeding leslie matrix
# into post-breeding leslie matrix
# by multilying survival values in last col by
# fecundity values in first col
# essentially assumes that only adults breed