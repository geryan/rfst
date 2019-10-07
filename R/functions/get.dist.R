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
  
  library(raster)
  
  # Create empty lists to fill with layers
  
  pb <- vector("list", timesteps + 1)
  lo <- vector("list", timesteps + 1)
  fi <- vector("list", timesteps + 1)
  lohi <- vector("list", timesteps + 1)
  fihi  <- vector("list", timesteps + 1)
  tsl <- vector("list", timesteps + 1)
  tsf <- vector("list", timesteps + 1)
  mort <- vector("list", timesteps + 1)
  
  # Get planned burning and logging layers separated from harvest layer
  # Create fire layer from planned burning and fire severity
  # -----------------------------------------------------------------------
  
  for (i in 1:(timesteps + 1)){
    
    pb[[i]] <- rst.op(
      input1 = ha[[i]],
      op = "pb",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/pb/pb_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "pb"
    )
    
    
    lo[[i]] <- rst.op(
      input1 = ha[[i]],
      op = "harvest",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/lo/lo_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "lo"
    )
    
    fi[[i]] <- rst.op(
      input1 = fs[[i]],
      input2 = pb[[i]],
      op = "fire",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/fi/fi_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "fi"
    )
    
  }
  
  # Create initial fire and logging history from external history layers and initial fire and logging layers
  # --------------------------------------
  
  fihi[[1]] <- rst.op(
    input1 = fi[[1]],
    input2 = fire_history,
    op = "history",
    proj_mask = proj_mask,
    filename = sprintf(
      "%s/fihi/fihi_%s_%s.grd",
      out_path,
      scn_id,
      year0
    ),
    layernames = "fihi",
    year = year0
  )
  
  lohi[[1]] <- rst.op(
    input1 = lo[[1]],
    input2 = logging_history,
    op = "history",
    proj_mask = proj_mask,
    filename = sprintf(
      "%s/lohi/lohi_%s_%s.grd",
      out_path,
      scn_id,
      year0
    ),
    layernames = "lohi",
    year = year0
  )
  
  # Create subsequent fire and logging history layers from preceeding year's fire and logging history layer and current fire and logging layer
  # ----
  
  for (i in 2:(timesteps + 1)){
    
    fihi[[i]] <- rst.op(
      input1 = fi[[i]],
      input2 = fihi[[i-1]],
      op = "history",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/fihi/fihi_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "fihi",
      year = year0 + i - 1
    )
    
    lohi[[i]] <- rst.op(
      input1 = lo[[i]],
      input2 = lohi[[i-1]],
      op = "history",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/lohi/lohi_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "lohi",
      year = year0 + i - 1
    )
  }
  
  # Create time since fire and time since logging layers
  # -------------------------------------------------------
  
  for (i in 1:(timesteps + 1)){
    
    tsf[[i]] <- rst.op(
      input1 = fi[[i]],
      input2 = fihi[[i]],
      op = "timesince",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/tsf/tsf_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "tsf",
      year = year0 + i - 1
    )
    
    tsl[[i]] <- rst.op(
      input1 = lo[[i]],
      input2 = lohi[[i]],
      op = "timesince",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/tsl/tsl_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "tsl",
      year = year0 + i - 1
    )
    
    mort[[i]] <- rst.op(
      input1 = lo[[i]],
      input2 = fs[[i]],
      op = "mort",
      proj_mask = proj_mask,
      filename = sprintf(
        "%s/mort/mort_%s_%s.grd",
        out_path,
        scn_id,
        year0 + i - 1
      ),
      layernames = "mort",
      year = year0 + i - 1
    )
  }
  
  
  # Combine lists of rasters into a list of raster stacks
  # ----
  
  result <- mapply(pb, fi, lo, fihi, lohi, tsf, tsl, mort, FUN = stack)
  
  return(result)
  
}