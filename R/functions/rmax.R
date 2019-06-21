rmax <- function(
  tm
){
  
  result <- abs(
    eigen(tm)$values[1]
  )
  
  return(result)
  
}