emp <- function(x, q = NULL){
  
  if(is.null(q)){
    emp <- min(
      apply(
        X = x,
        FUN = function(x){
          min(rowSums(x))
        },
        MARGIN = 3
      )
    )
  } else {
    emp <- stats::quantile(
      x = apply(
        X = x,
        FUN = function(x){
          min(rowSums(x))
        },
        MARGIN = 3
      ),
      probs = q
    )
  }
  
  
  
  return(emp)
}