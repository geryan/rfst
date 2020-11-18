med.pop <- function(x, q = NULL){
  
  if(is.null(q)){
    med.pop <- median(
      apply(
        X = x,
        FUN = function(x){
          median(rowSums(x))
        },
        MARGIN = 3
      )
    )
  } else {
    med.pop <- stats::quantile(
      x = apply(
        X = x,
        FUN = function(x){
          median(rowSums(x))
        },
        MARGIN = 3
      ),
      probs = q
    )
  }
  
  
  
  return(med.pop)
}