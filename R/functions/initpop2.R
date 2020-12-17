initpop2 <- function(
  hs,
  popsize,
  cc,
  ss,
  pp = 0.7
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
      hs,
      pp
    ){
      poprst(x = hs, popsize = y, cc = cc, pp = pp)
    },
    hs = hs,
    cc = cc,
    pp = pp
  )
  
  result <- stack(initpops)
  
  return(result)
}