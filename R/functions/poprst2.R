poprst2 <- function(x, ss, ps, cc, fact = 0.9){
  
  v <- getValues(x)
  
  n <-  round(fact*ss*cc * 1/(1 + exp(-10*(v - 0.5))))
  
  ordn <- order(
    n,
    decreasing = TRUE
  )
  
  nord <- n[ordn]
  
  i <- 1
  z <- 0
  
  while(sum(z) < ps){
    
    z <- nord[1:i]
    
    i <- i +1
  }
  
  y <- x
  
  y[!is.na(v)] <- 0
  
  y[ordn[1:(i-1)]] <- nord[1:i-1]
  
  return(y)
  
}