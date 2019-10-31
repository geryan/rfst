emp.all <- function(x){
  
  emp.all <- apply(
    X = x,
    MARGIN = 3,
    FUN = function(x){min(rowSums(x))}
  )
  
  return(list(emp.all))
  
}