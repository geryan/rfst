get.disturbance <- function(
  disturbances,
  disturbance_history,
  out_path,
  scn_id,
  proj_mask,
  layernames,
  timesteps,
  year0
){
  
  library(raster)
  
  hi <- vector("list", timesteps + 1)
  ts <- vector("list", timesteps + 1)
  
  
  hi[[1]] <- rst.op(input1 = disturbances[[1]],
                    input2 = disturbance_history,
                    op = "history",
                    proj_mask = proj_mask,
                    filename = sprintf("%s/%s/%s_%s_%s.grd",
                                       out_path,
                                       layernames[1],
                                       scn_id,
                                       layernames[1],
                                       year0),
                    layernames = layernames[1],
                    year = year0)
  
  for (i in 2:(timesteps+1)){
    hi[[i]] <- rst.op(input1 = disturbances[[i]],
                      input2 = hi[[i-1]],
                      op = "history",
                      proj_mask = proj_mask,
                      filename = sprintf("%s/%s/%s_%s_%s.grd",
                                         out_path,
                                         layernames[1],
                                         scn_id,
                                         layernames[1],
                                         year0 + i - 1),
                      layernames = layernames[1],
                      year = year0 + i - 1)
  }
  
  for(i in 1:(timesteps + 1)){
    
    ts[[i]] <- rst.op(input1 = disturbances[[i]],
                      input2 = hi[[i]],
                      op = "timesince",
                      proj_mask = proj_mask,
                      filename = sprintf("%s/%s/%s_%s_%s.grd",
                                         out_path,
                                         layernames[2],
                                         scn_id,
                                         layernames[2],
                                         year0 + i - 1),
                      layernames = layernames[2],
                      year = year0 + i - 1)
  
  }

  result <- mapply(hi, ts, FUN = stack)
  
  return(result)
  
}