# This function calculates the average aspect on a circular scale from x > 0 to x = 360 degrees

aspect.avg <- function(x, ...){
  
  opp <- sapply(
    X = x,
    FUN = function(y){
      sin(y/360*2*pi)
    }
  )
  
  adj <- sapply(
    X = x,
    FUN = function(y){
      cos(y/360*2*pi)
    }
  )
  
  theta <- atan(sum(opp, na.rm = TRUE)/sum(adj, na.rm = TRUE))
  
  theta.degree <- theta*360/(2*pi)
  
  result <- ifelse(
    test = theta.degree > 0,
    yes = theta.degree,
    no = theta.degree + 360
  )
  
  return(result)
  
}