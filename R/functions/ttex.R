# time to extinction from array

ttex <- function(x){
  
  rs <- apply(
    X = x,
    FUN = rowSums,
    MARGIN = 3
  )
  
  tex <- apply(
    X = rs,
    FUN = function(z){
      ifelse(any(z == 0), min(which(z == 0)), NA)
    },
    MARGIN = 2
  )
  
  return(tex)
  
}