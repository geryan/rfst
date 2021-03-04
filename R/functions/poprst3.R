poprst <- function(x, popsize, cc, pp = 0.7){
  
  # notworking
  
  v1 <- getValues(x)
  
  threshold <- quantile(v1, probs = pp, na.rm = TRUE)
  
  v <- ifelse(is.na(v1), NA_real_, 0)
  
  
  y <- which(v1 >= threshold)
  
  zz <- sum(round(cc * 1/(1 + exp(-10*(v1[y] - 0.5)))), na.rm = TRUE)
  
  if(zz < popsize){stop("Threshold too high, population too great for threshold")}
  
  while(sum(v, na.rm = TRUE) < popsize){
    
    z <- sample(y, 1)
    
    w <- suppressWarnings(rbinom(n = 1, size = 1, p = v1[z]))
    
    if(!is.na(w)){
      if(w == 1){
        if(v[z] <= (round(cc * 1/(1 + exp(-10*(v1[z] - 0.5)))))){
          v[z] <- v[z] + 1
        } else {
          y <- y[-z]
        }
      }
    }
    
  }
  
  result <- x
  
  result[] <- v
  
  return(result)
  
}

