get.dist <- function(
  fire_history,
  logging_history,
  fs,
  ha,
  out_path,
  scn_id,
  proj_mask,
  timesteps,
  year0
){
  
  pb <- vector("list", timesteps + 1)
  lo <- vector("list", timesteps + 1)
  fi <- vector("list", timesteps + 1)
  lohi <- vector("list", timesteps + 1)
  fihi  <- vector("list", timesteps + 1)
  tsl <- vector("list", timesteps + 1)
  tsf <- vector("list", timesteps + 1)
  
  for (i in 1:(timesteps + 1)){
    
    pb[[i]] <- rst.op(
      input1 = ha[[i]],
      op = "pb",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/pb/pb_%s_%s.grd",
        outpath,
        scn_id,
        year0 + i - 1
      ),
      layernames = "pb"
    )
    
  }
  
  
}