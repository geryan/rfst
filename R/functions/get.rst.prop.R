get.rst.prop <- function(inputs, total, proj_mask, filename, window = NA){
  
  out <- proj_mask
  
  bs <- blockSize(out)
  
  out <- writeStart(x = out, filename = filename, overwrite = TRUE)
  
  for (i in 1:bs$n){
    
    v <- getValues(inputs, row = bs$row[i], nrows = bs$nrows[i])
    
    if(class(inputs) == "RasterStack"){
      
      v <- rowSums(v)
    }
    
    t <- getValues(total, row = bs$row[i], nrows = bs$nrows[i])
    
    v <- v/t
    
    out <- writeValues(out, v, bs$row[i])
  }
  
  out <- writeStop(out)
  
  if(!any(is.na(window))){
    out <- focal(out, window, fun = mean, na.rm = TRUE, filename = filename, overwrite = TRUE)
  }
  
  return(out)
  
}