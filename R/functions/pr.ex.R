pr.ex <- function(x){
  
  nex <- length(
    which(
      apply(
        X = x,
        FUN = function(x){
          min(rowSums(x))
        },
        MARGIN = 3
      ) == 0
    )
  )
  
  n <- dim(x)[3]
  
  pr.ex <- nex/n
  
  return(pr.ex)
  
}