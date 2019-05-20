interpolate.climdat <- function(initras, futras, ny, nf, n0, varname){
  
  ns <- c(n0, nf)
  
  zz <- c((n0+1):(n0+ny))
  
  z <- vector("list", ny)
  
  for (i in 1:ny){
    q <- which.min(abs(zz[i] - nf))
    
    if(q == 1){
      z[[i]] <- initras
    } else{
      z[[i]] <- futras[[q-1]]
    }
  }
  
  
  z <- lapply(z, function(x){
    names(x) <- varname
    x
  })
  
  return(z)
}