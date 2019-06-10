rst.op <- function(
  input1,
  input2,
  op = c(
    "writeonly",
    "sub1",
    "prop",
    "addabs",
    "addper"),
  proj_mask,
  filename,
  window,
  layernames){
  
  out <- proj_mask
  
  bs <- blockSize(out)
  
  if(missing(op)){
    op <- "writeonly"
  }
  
  
  if(!missing(input2)){
    
    if(class(input2) == "RasterStack" | class(input2) == "RasterBrick"){
      
      nl <- nlayers(input2)
      
      out <- stack(replicate(nl, ch_mask))
      
      for (i in 1:bs$n){
        
        v1 <- getValues(input1, row = bs$row[i], nrows = bs$nrows[i])
        v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
            
       for(j in 1:nl){
         
         out[[j]] <- writeStart(x = out[[j]], filename = sprintf("%s_%s.grd", filename, layernames[j]), overwrite = TRUE)
         
         if(op == "addabs") {
           
           v <- v1 + v2[,j]
           out[[j]] <- writeValues(out[[j]], v, bs$row[i])
           
         } else if (op == "addper"){
           
           v <- v1 * (1 + v2[,j]/100)
           out[[j]] <- writeValues(out[[j]], v, bs$row[i])
         }
         
         out[[j]] <- writeStop(out[[j]])
         
         names(out[[j]]) <- names(input2[[j]])
         
         out[[j]] <- mask(x = out[[j]], mask = proj_mask, filename = sprintf("%s_%s.grd", filename, layernames[j]), overwrite = TRUE)
         
       }
      }
    }
  } else {
    
    out <- writeStart(x = out, filename = filename, overwrite = TRUE)
    
    for (i in 1:bs$n){
      
      v1 <- getValues(input1, row = bs$row[i], nrows = bs$nrows[i])
      
      if(op == "writeonly"){
        
        v <- v1
        
      } else if(op == "sub1"){
        
        v <- ifelse(v1 == 0, v1, v1-1)
        
      } else if(op == "prop"){
        
        if(class(input1) == "RasterStack" | class(input2) == "RasterBrick"){
          
          v1 <- rowSums(v1)
        }
        
        v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
        v <- ifelse(t == 0 , 0, v1/v2)
        
      } else if(op == "addabs"){
        
        v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
        v <- v1 + v2
        
      } else if(op == "addper"){
        
        v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
        v <- v1 * (1 + v2/100)
      }
      
      out <- writeValues(out, v, bs$row[i])
    }
    
    out <- writeStop(out)
    
    if(!missing(window)){
      out <- focal(out, window, fun = mean, na.rm = TRUE, filename = filename, overwrite = TRUE)
    }
    
    names(out) <- layernames
    
    out <- mask(x = out, mask = proj_mask, filename = filename, overwrite = TRUE)
    
  }
  
  return(out)
  
}