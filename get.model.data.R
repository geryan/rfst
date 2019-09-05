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
    cbind("date" = x$date, .)
  
  if(na.omit){
    
    result <- result %>% na.omit
    
  }
  
  return(result)
  
}