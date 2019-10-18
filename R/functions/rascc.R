rascc <- function(dat, new.proj.layer, filename, input.projection = "+proj=longlat +datum=WGS84"){
  
  library(tidyr)
  library(dplyr)
  library(raster)
  library(rlang)
  
  
  var <- colnames(dat$data)[[4]]
  var <- enquo(var)
  
  result <- dat$data %>%
    mutate(year = sub("-.*", "", time)) %>%
    dplyr::select(lon, lat, everything(), -time) %>%
    spread(year, !!var) %>%
    rasterFromXYZ(crs = input.projection)
  
  if(!missing(new.proj.layer)){
    
    result <- projectRaster(from = result,
                            to = new.proj.layer,
                            method = "bilinear"
                            )
    
    
    if(missing(filename)){
      result <- mask(x = result,
                     mask = new.proj.layer)
    } else {
      result <- mask(x = result,
                     mask = new.proj.layer,
                     filename = filename,
                     overwrite = TRUE)
    }
    
  }
  
  return(result)
}