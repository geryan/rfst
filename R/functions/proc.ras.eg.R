proc.ras.eg <- function(x, dp = dirpath, epsg = 32755){
  
  library(raster)
  
  result <- raster(
    x = sprintf(
      fmt = "%s/%s",
      dp,
      x
    )
  )
  
  crs(result) <- CRS(
    projargs = sprintf(
      fmt = "+init=epsg:%s",
      epsg
    )
  )
  
  extent(result) <- round(extent(result))
  
  return(result)
  
}