gsfplot <- function(x){
  
  library(ggplot2)
  
  z <- ggplot(x) +
    geom_sf() +
    theme_minimal() +
    coord_sf(datum = st_crs(x))
  
  return(z)
}