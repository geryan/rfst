rst.op <- function(
  input1,
  input2,
  op = c(
    "writeonly",
    "sub1",
    "prop",
    "addabs",
    "addper",
    "lessthan",
    "lessthanscale",
    "weightsum",
    "history",
    "timesince"),
  proj_mask,
  filename,
  layernames,
  window,
  lessthan,
  scaleto,
  weight1,
  weight2,
  year){
  
  library("raster")
  
  if(missing(op)){
    op <- "writeonly"
  }
  
  bs <- blockSize(proj_mask)
  
  if(!missing(input2)){
    
    if(class(input2) == "RasterStack" | class(input2) == "RasterBrick"){
      
      nl <- nlayers(input2)
      
      out <- stack(replicate(nl, ch_mask))

      for (j in 1:nl){
        
        out[[j]] <- writeStart(x = out[[j]], filename = sprintf("%s_%s.grd", filename, layernames[j]), overwrite = TRUE)
        
        for (i in 1:bs$n){
          
          v1 <- getValues(input1, row = bs$row[i], nrows = bs$nrows[i])
          v2 <- getValues(input2[[j]], row = bs$row[i], nrows = bs$nrows[i])
          
          if(op == "addabs") {
            
            v <- v1 + v2
            out[[j]] <- writeValues(out[[j]], v, bs$row[i])
            
          } else if (op == "addper"){
            
            v <- v1 * (1 + v2/100)
            out[[j]] <- writeValues(out[[j]], v, bs$row[i])
          }
        }
        
        out[[j]] <- writeStop(out[[j]])
        names(out[[j]]) <- names(input2[[j]])
        out[[j]] <- mask(x = out[[j]], mask = proj_mask, filename = sprintf("%s_%s.grd", filename, layernames[j]), overwrite = TRUE)
        
      }
      
      return(out)
      
    }
  } 
  
  out <- proj_mask
  
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
      v <- ifelse(v2 == 0 , 0, v1/v2)
        
    } else if(op == "addabs"){
        
      v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
      v <- v1 + v2
        
    } else if(op == "addper"){
        
      v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
      v <- v1 * (1 + v2/100)
      
    } else if(op == "lessthan"){
      
      v <- v1
      v[v < lessthan] <- 0
      v[v != 0] <- 1
      
    } else if(op == "lessthanscale"){
      
      v <- v1
      v[v < lessthan] <- 0
      v[v > scaleto] <- 1
      v[v > 1] <- (v[v > 1] - lessthan)/(scaleto - lessthan)
      
    } else if (op == "weightsum"){
      
      v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
      
      w <- weight1 + weight2
      
      v <- v1*(w - weight1)/w + v2*(w - weight2)/w
      
    } else if (op == "history"){
      
      v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
      
      v <- ifelse(v1 > 0, year, v2)
      
    } else if (op == "timesince"){
      
      v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
      
      v2 <- year - v2
      
      v <- ifelse(v1 > 0, 0, v2)
      
    }
      
    out <- writeValues(out, v, bs$row[i])
    
  }
    
  out <- writeStop(out)
    
  if(!missing(window)){
    
    out <- focal(out, window, fun = mean, na.rm = TRUE, filename = filename, overwrite = TRUE)
  }
    
  names(out) <- layernames
  out <- mask(x = out, mask = proj_mask, filename = filename, overwrite = TRUE)
    
  
  return(out)
  
}