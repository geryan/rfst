cadispersal <- function(p){
  
  stay <- 0
  
  x <- 0
  
  y <- 0
  
  n <- 0
  
  z <- list(c( 0, 1),
            c( 1, 0),
            c( 0,-1),
            c(-1, 0))
  
  
  while(stay == 0){
    
    n <- n + 1
    
    s <- sample(x = c(1, 2, 3, 4), size = 1, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))
    
    x <- x + z[[s]][[1]]
    
    y <- y + z[[s]][[2]]
    
    stay <- rbinom(n = 1, size = 1, prob = p)
    
  }
  
  d <- (x^2 + y ^2)^(1/2)
  
  return(list(n = n,
              x = x,
              y = y,
              d = d))
  
  
}