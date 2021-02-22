buff.sample.pa <- function(
  x,
  y = NA,
  rfa,
  cellsize = 500,
  buff.dist = 500,
  species = NA,
  survey_method = NA,
  sg = NULL
){
  
  z <- add.buffered(
    x = x,
    y = y,
    buff.dist = buff.dist,
    species = species,
    survey_method
  )
  
  result <- sample.pa(
    z = z,
    rfa = rfa,
    cellsize = cellsize,
    sg = sg
  )
  
  return(result)
  
}