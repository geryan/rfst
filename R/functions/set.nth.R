set.nth <- function(x, na.rm = TRUE){
  
  if(na.rm == FALSE){
    if(any(is.na(x))){
      return(NA_integer_)
    }
  }
  
  if(all(is.na(x))){
    return(NA_integer_)
  }
  
  return(as.integer(x[1]))
  
  # for(i in 1:length(x)){
  # 
  #   if(is.na(x[i])){
  #     next
  #   } else {
  #     return(as.integer(x[i]))
  #   }
  # 
  # }
  
}
