cc_6 <- function (landscape, timestep) {
  
  library(raster)
  
  fun <- function(suitability) {
    6 - round(6 * dlogis(suitability, scale = 0.25))
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}   