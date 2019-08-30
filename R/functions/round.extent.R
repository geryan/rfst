round.extent <- function(x){
  
  library(raster)
  
  extent(x) <- round(extent(x))
  
  return(x)
  
}