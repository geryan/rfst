# This function calculates the average aspect on a circular scale from x > 0 to x = 360 degrees
aspect.avg <- function(x, na.rm = TRUE){
  
  if(na.rm == FALSE){
    if(any(is.na(x))){
      return(NA_integer_)
    }
  }
  
  if(all(is.na(x))){
    return(NA_integer_)
  }
  
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
  
  n <- length(which(!is.na(x)))
  
  oppmean <- sum(opp, na.rm = TRUE)/n
  
  adjmean <- sum(adj, na.rm = TRUE)/n
  # NB: this should really work whether means are taken or not
  # As in this case the hypotenuse is not relevant to angle calculation
  
  theta <- atan(oppmean/adjmean)
  
  theta.degree <- theta*360/(2*pi)
  
  if (adjmean >= 0) {
    if (oppmean > 0) {
      theta.degree.mean <- theta.degree
    } else {
      theta.degree.mean <- theta.degree + 360
    }
  } else {
    theta.degree.mean <- theta.degree + 180
  }
  
  
  result <- round(theta.degree.mean)
  
  result <- as.integer(ifelse(result == 0, 360, result))
  
  return(result)
  
}