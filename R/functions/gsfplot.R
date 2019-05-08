gsfplot <- function(x){
  
  library(ggplot2)
  
  z <- ggplot(x) + geom_sf() + theme_minimal()
  
  return(z)
}