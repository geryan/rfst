get.model.data <- function(
  x, # species occurrence records
  y, # variable set
  na.omit = TRUE
){
  
  library(sf)
  library(raster)
  library(dplyr)
  
  if(class(y) == "list"){
    y <- y[[1]]
  }
  
  result <- y %>%
    raster::extract(y = st_coordinates(x)) %>%
    cbind("PA" = x$PA, .) %>%
    as.data.frame %>%
    cbind("date" = x$date, .) %>%
    as_tibble %>%
    bind_cols(
      st_coordinates(x) %>%
        as_tibble
    ) %>%
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