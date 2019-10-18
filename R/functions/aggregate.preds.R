aggregate.preds <- function(
  x,
  fact,
  out_path = "output/habitat_pred/",
  scn_id,
  varset,
  species
){
  
  library(raster)

  result <- raster::aggregate(
      x = x,
      fact = fact,
      fun = mean,
      na.rm = TRUE,
      filename = sprintf(
        "%s/aggpred_%s_%s_%s.grd",
        out_path,
        scn_id,
        varset,
        species
      )
    )
    
    return(result)
    
  
}