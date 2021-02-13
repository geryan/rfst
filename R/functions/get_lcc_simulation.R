get_lcc_simulation <- function(x) {
  
  
  lcc <- lapply(
    X = x[[1]],
    FUN = function(y){
      
      z <- y$carrying_capacity %>%
        getValues %>%
        sum(na.rm = TRUE)
      
      return(z)
      
    }
  ) %>%
    unlist
  
  return(lcc)
  
}