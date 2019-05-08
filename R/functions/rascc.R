rascc <- function(dat, new.proj.layer = FALSE, input.projection = "+proj=longlat +datum=WGS84"){
  
  library(tidyr)
  library(dplyr)
  library(raster)
  
  
  var <- colnames(dat)[[4]]
  var <- enquo(var)
  
  result <- dat %>%
    mutate(year = sub("-.*", "", time)) %>%
    select(longitude, latitude, everything(), -time) %>%
    spread(year, !!var) %>%
    rasterFromXYZ(crs = input.projection)
  
  if(!is.logical(new.proj.layer)){
    
    result <- projectRaster(from = result,
                            to = new.proj.layer,
                            method = "ngb")
    
    result <- mask(x = result,
                   mask = new.proj.layer)
  }
  
  return(result)
}