get.rst.dat <- function(inputs, proj_mask, filename, sub1 = FALSE){
  
  out <- proj_mask
  
  bs <- blockSize(out)
  
  out <- writeStart(x = out, filename = filename, overwrite = TRUE)
  
  for (i in 1:bs$n){
    
    v <- getValues(inputs, row = bs$row[i], nrows = bs$nrows[i])
    
    if(sub1){
      v <- ifelse(v == 0, v, v-1)
    }
    
    out <- writeValues(out, v, bs$row[i])
  }
  
  out <- writeStop(out)

  out <- mask(x = out, mask = proj_mask, filename = filename, overwrite = TRUE)
  
  return(out)
  
}