most.freq <- function(x, ...){
  
  ux <- unique(x)
  
  if(length(ux) == 1){
    return(x[1])
  }
  
  tx <- table(x)
  
  mx <- max(tx)
  
  wx <- which(tx==mx)
  
  nx <- as.numeric(dimnames(tx)[[1]])
  
  if(length(wx) == 1){
    return(nx[wx])
  } else {
    return(sample(nx, size = 1))
  }
  
}