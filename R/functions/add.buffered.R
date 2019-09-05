add.buffered <- function(x, y){ # x is target species, y is background species
  
  library(dplyr)
  
  z <- outside.buffer(x, y)
  
  result <- rbind(x, z) %>%
    dplyr::arrange(date)
  
  return(result)
  
}