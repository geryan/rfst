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
    "timesince",
    "cc",
    "pop",
    "pb",
    "harvest",
    "fire",
    "div10",
    "mort",
    "threshold"),
  proj_mask,
  filename,
  layernames,
  window,
  lessthan,
  scaleto,
  weight1,
  weight2,
  year,
  size,
  popsize,
  cc,
  threshold = 0){
  
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
  
  if(op == "pop"){
    
    v1 <- getValues(input1)
    
    v <- ifelse(is.na(v1), NA_real_, 0)
    
    
    y <- which(v1 >= threshold)
    
    while(sum(v, na.rm = TRUE) < popsize){
      
      z <- sample(y, 1)
      
      x <- suppressWarnings(rbinom(n = 1, size = 1, p = v1[z]))
      
      if(!is.na(x)){
        if(x == 1){
          if(v[z] <= (round(cc * 1/(1 + exp(-10*(v1[z] - 0.5)))))){
            v[z] <- v[z] + 1
          }
        }
      }
      
    }
    
    out <- writeValues(out, v, 1)
    
  } else{
    for (i in 1:bs$n){
      
      v1 <- getValues(input1, row = bs$row[i], nrows = bs$nrows[i])
      
      if(op == "writeonly"){
        
        if(class(input1) == "RasterStack" | class(input1) == "RasterBrick"){
          
          v1 <- rowSums(v1)
          
        }
        
        v <- v1
        
      } else if(op == "sub1"){
        
        v <- ifelse(v1 == 0, v1, v1-1)
        
      } else if(op == "prop"){
        
        if(class(input1) == "RasterStack" | class(input1) == "RasterBrick"){
          
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
        
      } else if (op == "cc"){
        
        v <- round(v1*size)
        
      } else if (op == "pb"){
        
        if(max(v1, na.rm = TRUE) == 1){
          
          v <- v1
          
        } else if (max(v1, na.rm = TRUE) > 1){
          
          v <- ifelse(v1 == 5, 1, 0)
          
        } else if (max(v1, na.rm = TRUE) == 0){
          
          v <- v1
          
        }
        
      } else if (op == "harvest"){
        
        if(max(v1, na.rm = TRUE) == 1){
          
          v <- rep_len(0, length.out = length(v1))
          
        } else if (max(v1, na.rm = TRUE) > 1){
          
          v <- ifelse(is.na(v1), 0, ifelse(v1 == 0, 0, ifelse(v1 == 5, 0, 1)))
          
        } else if (max(v1, na.rm = TRUE) == 0){
          
          v <- v1
          
        }
        
      } else if (op == "fire"){
        
        v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
        
        v <- ifelse(v1 > 0, 1, ifelse(v2 > 0, 1, 0))
        
      } else if (op == "div10"){
        
        v <- v1/10
        
      } else if (op == "mort"){
        
        v2 <- getValues(input2, row = bs$row[i], nrows = bs$nrows[i])
        
        v <- ifelse(v1 == 1, 1, v2/6)
        
        v <- 1 - v
      } else if(op == "threshold"){
       
        v <- ifelse(v < threshold, 0, threshold) 
      }
      
      out <- writeValues(out, v, bs$row[i])
      
    }
  }
    
    
  out <- writeStop(out)
    
  if(!missing(window)){
    
    out <- focal(out, window, fun = mean, na.rm = TRUE, filename = filename, overwrite = TRUE)
  }
    
  names(out) <- layernames
  out <- mask(x = out, mask = proj_mask, filename = filename, overwrite = TRUE)
    
  
  return(out)
  
}