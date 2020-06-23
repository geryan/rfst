maggregate <- function(
  x,
  fact,
  out_path = "output/habitat_pred_aggregated/",
  aggname = "aggpred",
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
        "%s/%s_%s_%s_%s.grd",
        out_path,
        aggname,
        scn_id,
        varset,
        species
      ),
      overwrite = TRUE
    )
    
    return(result)
    
  
}