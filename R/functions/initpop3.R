initpop3 <- function(
  hs,
  popsize,
  cc,
  ss,
  fact = 0.9
){
  
  maxpop <- sum(cc - round(cc * dlogis(getValues(hs), scale = 0.25)), na.rm = TRUE)
  
  if(missing(popsize)){
    popsize <- maxpop
  }
  
  if(popsize > maxpop){
    popsize <- maxpop
  }
  
  pops <- ceiling(ss*popsize)
  
  initpops <- mapply(
    ps = pops,
    ss = ss,
    FUN = poprst2,
    MoreArgs = list(
      x = hs,
      cc = cc,
      fact = fact
    )
  )
  
  result <- stack(initpops)
  
  return(result)
}