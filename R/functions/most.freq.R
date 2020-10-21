most.freq <- function(x, na.rm = TRUE){
  
  if(na.rm == FALSE){
    if(any(is.na(x))){
      return(NA_integer_)
    }
  }
  
  if(all(is.na(x))){
    return(NA_integer_)
  }
  
  ux <- unique(x)
  
  if(length(ux) == 1){
    return(as.integer(x[1]))
  }
  
  tx <- table(x)
  
  mx <- max(tx)
  
  wx <- which(tx==mx)
  
  nx <- as.numeric(dimnames(tx)[[1]])
  
  if(length(wx) == 1){
    return(as.integer(nx[wx]))
  } else {
    return(as.integer(base::sample(x = nx[wx], size = 1)))
  }
  
}
