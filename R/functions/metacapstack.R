metacapstack <- function(x, f){
  
  library(raster)
  library(metacapa)
  
  nlayers <- dim(x)[3]
  
  reslist <- vector("list", nlayers)
  
  for(i in 1:nlayers){
    
    reslist[[i]] <- patch_config(x[[i]], "m")
    
  }
  
  result <- sapply(
    X = reslist,
    FUN = meta_capacity,
    f = f
  )
  
  return(result)
  
}