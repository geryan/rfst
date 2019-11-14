mmp <- function(x, q = NULL){
  
  mmp <- mean(
    apply(
      X = x,
      FUN = function(x){
        min(rowSums(x))
      },
      MARGIN = 3
    )
  )
  
  
  
  return(mmp)
}