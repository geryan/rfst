cc_12 <- function (landscape, timestep) {
  
  library(raster)
  
  fun <- function(suitability) {
    round(12 * 1/(1 + exp(-10*(suitability - 0.5))))
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}   