buff.sample.pa <- function(x, y, rfa, cellsize = 500, buff.dist = 500){
  
  z <- add.buffered(
    x = x,
    y = y,
    buff.dist = buff.dist
  )
  
  result <- sample.pa(
    z = z,
    rfa = rfa,
    cellsize = cellsize
  )
  
  return(result)
  
}