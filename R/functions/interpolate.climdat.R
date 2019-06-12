interpolate.climdat <- function(initras, futras, ntimesteps, data_years, year0, varname, proj_mask, filename){
  
  ns <- c(year0, data_years)
  
  zz <- c(year0:(year0 + ntimesteps))
  
  allras <- stack(initras, futras)
  
  z <- vector("list", (ntimesteps + 1))
  
  for (i in 1:(ntimesteps + 1)){
    
    q <- which.min(abs(zz[i] - ns))
    qq <- which.min(abs(zz[i] - ns[-q]))
    
    r <- min(abs(zz[i] - ns))
    rr <- min(abs(zz[i] - ns[-q]))
    
    
    if(r == 0){
      
      z[[i]] <- allras[[q]]
      
    } else {

      
      z[[i]] <- rst.op(input1 = allras[[q]],
                       input2 = allras[[qq]], 
                       op = "weightsum",
                       proj_mask = proj_mask,
                       filename = sprintf("%s_%s.grd", filename, zz[i]),
                       layernames = sprintf("%s_%s", varname, zz[i]),
                       weight1 = r,
                       weight2 = rr)
      
    }
  }
    
  z <- lapply(z, function(x){
    names(x) <- varname
    x
  })
  
  return(z)
}