nex <- function(x, p = FALSE){
  
  
  minpop <- apply(
    X = x,
    FUN = function(x){
      min(rowSums(x))
    },
    MARGIN = 3
  )
  
  nex <- length(which(minpop == 0))
  
  if(p){
    nex <- nex/length(minpop)
  }
  
  return(nex)
}