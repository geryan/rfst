initpop2 <- function(
  hs,
  popsize,
  cc,
  ss
){
  
  maxpop <- sum(cc - round(cc * dlogis(getValues(hs), scale = 0.25)), na.rm = TRUE)
  
  if(missing(popsize)){
    popsize <- maxpop
  }
  
  if(popsize > maxpop){
    popsize <- maxpop
  }
  
  pops <- ceiling(ss*popsize)
  
  initpops <- lapply(
    X = pops,
    FUN = function(
      y,
      cc,
      hs
    ){
      poprst(x = hs, popsize = y, cc = cc)
    },
    hs = hs,
    cc = cc
  )
  
  result <- stack(initpops)
  
  return(result)
}