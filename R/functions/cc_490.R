cc_490 <- function (landscape, timestep) {
  
  library(raster)
  
  fun <- function(suitability) {
    round(490 * 1/(1 + exp(-10*(suitability - 0.5))))
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}   