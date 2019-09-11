add.buffered <- function(x, y, buff.dist = 500){ # x is target species, y is background species
  
  library(dplyr)
  
  z <- outside.buffer(x, y, buff.dist)
  
  result <- rbind(x, z) %>%
    dplyr::arrange(date)
  
  return(result)
  
}