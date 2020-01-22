add.buffered <- function(
  x, # target species
  y = NA, # background species
  buff.dist = 500,
  species = NA,
  survey_method = NA
){
  
  library(dplyr)
  
  z <- outside.buffer(x, y, buff.dist, species, survey_method)
  
  spcs <- species
  
  if(!is.na(spcs)){
    x <- x %>%
      filter(species == spcs)
  }
  
  
  result <- rbind(x, z) %>%
    dplyr::arrange(date)
  
  return(result)
  
}

