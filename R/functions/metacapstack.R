metacapstack <- function(x, f, year0 = NULL){
  
  library(raster)
  library(metacapa)
  
  nlayers <- dim(x)[3]
  
  reslist <- vector("list", nlayers)
  
  for(i in 1:nlayers){
    
    reslist[[i]] <- patch_config(x[[i]], "m")
    
  }
  
  metapopulation_capacity <- sapply(
    X = reslist,
    FUN = function(x, f){
      
      result <- try(meta_capacity(x = x, f = f))
      
      result <- ifelse(is.character(result), 0, result)
      
      return(result)
      
    },
    f = f
  )
  
  if(is.null(year0)){
    years <- 0:(length(metapopulation_capacity) - 1)
  } else {
    years <- year0:(year0 + (length(metapopulation_capacity) - 1))
  }
  
  res <- tibble(
    metapopulation_capacity = metapopulation_capacity,
    year = years
  )
  
  return(res)
  
}