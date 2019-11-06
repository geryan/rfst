lxtract <- function(x, y, layernames = NULL){
  
  result <- lapply(
    X = x,
    FUN = function(z){z[[y]]}
  )
  
  result <- stack(result)
  
  
  if(!is.null(layernames)){
    
    if(length(layernames) != dim(result)[3]){
      stop("Number of layers is different from length of layernames")
    }
    
    names(result) <- layernames
  }
  
  return(result)
}