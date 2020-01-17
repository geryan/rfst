read.vba <- function(x){
  
  library(dplyr)
  library(readr)
  
  gm <- length(readLines(x))
  
  z <- readLines(x) %>%
    head(-7) %>%
    #paste0(collapse = "\n") %>%
    read_csv(skip = 15, guess_max = gm - 7)
  
  return(z)
  
}