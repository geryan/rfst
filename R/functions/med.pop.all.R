med.pop.all <- function(x){
  
  med.pop.all <- apply(
    X = x,
    MARGIN = 3,
    FUN = function(x){median(rowSums(x))}
  )
  
  return(list(med.pop.all))
  
}