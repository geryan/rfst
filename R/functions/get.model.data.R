get.model.data <- function(
  x, # species occurrence records
  y, # variable set
  na.omit = TRUE
){
  
  library(sf)
  library(raster)
  library(dplyr)
  
  xx <- x
  yy <- y
  
  if(class(yy) == "list"){
    yy <- yy[[1]]
  }
  
  result <- yy %>%
    raster::extract(
      x = .,
      y = st_coordinates(xx)
    ) %>%
    cbind("PA" = xx$PA, .) %>%
    as.data.frame %>%
    cbind("date" = xx$date, .) %>%
    as_tibble %>%
    bind_cols(
      st_coordinates(xx) %>%
        as_tibble
    ) %>%
    select(-lon, -lat) %>% #this is a temporary hack to remove the raster layer derived lon and lat for each point, rather than exact. Needs to be fully excised from 07...R
    rename(
     "lon" = X,
     "lat" = Y
    ) %>%
    as.data.frame
  
  
  if(na.omit){
    result <- result %>%
      na.omit
  }
  
  return(result)
  
}