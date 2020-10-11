most.freq <- function(x, na.rm = TRUE){
  
  if(na.rm == FALSE){
    if(any(is.na(x))){
      return(NA)
    }
  }
  
  if(all(is.na(x))){
    return(NA)
  }
  
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
    return(base::sample(x = nx[wx], size = 1))
  }
  
}
