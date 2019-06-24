compareemp <- function(x){
  
  minpop <- vector("numeric", length(x))
  rawratios <- vector("numeric", length(x))
  relratios <- vector("numeric", length(x))
  pop0  <- vector("numeric", length(x))
  varnames  <- vector("character", length(x))
  
  
  for(i in 1:length(x)){
    
    pop0[i] <- sum(x[[i]][[1]])
    
    minpop[i] <- min(pop0[i], min(apply(x[[i]][[2]], 3, function(z) min(rowSums(z)))))
    
    rawratios[i] <- minpop[i]/pop0[i]
    
    relratios[i] <- rawratios[i]/rawratios[1] - 1
    
    varnames[i] <- x[[i]][[3]]
  }
  
  
  library(ggplot2)
  
  zz <- data.frame(relratios, as.factor(varnames))
  
  result <- ggplot(zz) +
    geom_bar(aes(x = varnames, y = relratios), stat = "identity", fill = "springgreen") +
    labs(x = "Scenario", y = "Relative ratio")
  
  return(result)
  
}