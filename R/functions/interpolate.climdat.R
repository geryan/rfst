interpolate.climdat <- function(
  initras,
  futras,
  ntimesteps,
  data_years,
  year0,
  varname,
  proj_mask,
  filename
){
  
  if(ntimesteps > (max(data_years) - year0)){
    stop("")
  }
  
  
  ns <- c(year0, data_years)
  
  zz <- c(year0:(year0 + ntimesteps))
  
  allras <- stack(initras, futras)
  
  z <- rep(list(proj_mask), (ntimesteps + 1))
  
  for (i in 1:(ntimesteps + 1)){
    
    q <- which.min(abs(zz[i] - ns))
    qq <- which.min(abs(zz[i] - ns[-q]))
    
    r <- min(abs(zz[i] - ns))
    rr <- min(abs(zz[i] - ns[-q]))
    
    
    if(r == 0){
      
      z[[i]][] <- getValues(allras[[q]])
      
    } else {

      v2 <- getValues(allras[[qq]])
      
      v1 <- getValues(allras[[q]])
      
      w <- r + rr
      
      v <- v1*(w - r)/w + v2*(w - rr)/w
      
      
      z[[i]][] <- v
      
    }
  }
  
  brickz <- brick(z)
  
  result <- rst.op(
    input1 = brickz,
    op = "writeBrick",
    proj_mask = proj_mask,
    filename = filename,
    layernames = sprintf(
      "%s_%s",
      varname,
      zz
    )
  )
  
  return(result)
  
}