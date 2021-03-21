write_pop_rasters <- function(
  x,
  path,
  id,
  sp,
  proj_mask
){
  
  nstages <- nlayers(x[[1]])
  
  nperiods <- length(x)
  
  periods <- rep(0:(nperiods-1), times = 1, each = nstages)
  
  stages <- rep(1:nstages, times = nperiods)
  
  layernames <- sprintf(
    "period_%s_stage_%s",
    periods,
    stages
  )
  
  z <- rst.op(
    input1 = brick(x),
    op = "writeBrick",
    proj_mask = proj_mask,
    filename = sprintf(
      "%s/pva_pop_%s_%s.grd",
      path,
      id,
      sp
    ),
    layernames = layernames
  )
  
  
  y <- vector(
    mode = "list",
    length = nperiods
  )
  
  
  for(i in 1:nperiods){
    y[[i]] <- z[[(i-1)*nstages +(1:nstages)]]
  }
  
  return(y)
  
}