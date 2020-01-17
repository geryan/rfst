stack.climate <- function(
  prec_djf,
  prec_jja,
  tmax_djf,
  tmin_jja,
  ntimesteps
){
  
  library(raster)
  
  result <- vector(
    mode = "list",
    length = (ntimesteps + 1)
  )
  
  
  for(i in 1:(ntimesteps + 1)){
    
    result[[i]] <- stack(
      prec_djf[[i]],
      prec_jja[[i]],
      tmax_djf[[i]],
      tmin_jja[[i]]
    )
    
  }
  
  return(result)
  
}