initpop5 <- function(
  hs,
  popsize,
  cc,
  ss,
  pp = 0.7,
  z = 0.4
){
  
  
  
  pops <- ceiling(ss*popsize)
  
  
  ip <- lapply(
    X = pops,
    FUN = function(x, hs){
      hs
    },hs = hs
  ) %>%
    stack
  
  v1 <- getValues(ip)
  
  threshold <- quantile(v1[,1], probs = pp, na.rm = TRUE)
  
  y <- which(v1[,1] > threshold)
  
  
  ccy <- round(cc * 1/(1 + exp(-10*(v1[y,1] - z))))
  
  
  maxpopcc <- sum(round(cc * 1/(1 + exp(-10*(v1[y,1] - z)))), na.rm = TRUE)
  
  if(missing(popsize)){
    popsize <- maxpopcc
  }
  
  if(popsize > maxpopcc){
    warning(sprintf(
      "Population size of %s is being reduced to %s given threshold of %s",
      popsize,
      maxpopcc,
      pp
    ))
    popsize <- maxpopcc
    
  }
  
  
  v <- ifelse(is.na(v1), NA_real_, 0)
  
  nstages <- length(pops)
  
  yl <- 1:length(y)
  yi <- yl
  
  while(sum(v, na.rm = TRUE) < popsize){
    
    z <- sample(
      x = y[yi],
      size = nstages,
      replace = TRUE
    )
   
    for(i in 1:nstages){
      
      w <- suppressWarnings(rbinom(n = 1, size = 1, p = v1[z]))
      
      if(!is.na(w)){
        if(w == 1){
          
          v[z[i],i] <- v[z[i],i] + 1
          
        }
      }
      
    }
    
    rvy <- apply(
      X = v[y,],
      MARGIN = 1,
      FUN = sum,
      na.rm = TRUE
    )
    
    cci <- rvy == ccy
    
     yi <- yl[!cci]
    
  }
  
  
  ip[] <- v
  
  return(ip)
}