get.landis.fire <- function(
  scn_path,
  proj_path,
  out_path,
  scn_id,
  proj_mask,
  timesteps
){
  
  library(raster)
  library(dplyr)
  library(foreach)
  
  source(file = "R/functions/rst.op.R")
  
  
  fipath <- paste0(scn_path, "/fire/")
  
  
  
  result <- foreach(j = 0:timesteps) %do% {
    
    if(j == 0){
      
      firesev <- proj_mask
      
      names(firesev) <- "firesev"
      
      firesev[] <- 0
      
      firesev <- mask(x = firesev,
                      mask = proj_mask,
                      filename = sprintf("%s/%s/%s_firesev_%03d.grd",
                                         proj_path,
                                         out_path,
                                         scn_id,
                                         j),
                      overwrite = TRUE)
      
      
    } else {
      
      firesev <- paste0(fipath, "severity-", j,".img") %>%
        raster %>%
        rst.op(op = "sub1",
               proj_mask = proj_mask,
               filename = sprintf("%s/%s/%s_firesev_%03d.grd",
                                  proj_path,
                                  out_path,
                                  scn_id,
                                  j),
               layernames = "firesev")
    }
 
    
    
  }
  
  #result <- stack(result)
  
  return(result) 
}