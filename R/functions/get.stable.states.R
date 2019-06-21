get.stable.states <- function(
  tm
){
  
  result <- abs(
    eigen(tm)$vectors[,1] / base::sum(eigen(tm)$vectors[,1])
    )
  
  return(result)
}