initpop4 <- function(
  hs,
  popsize,
  cc,
  ss,
  pp = 0.7
){
  
  
  ## not functional
  
  maxpop <- sum(cc - round(cc * dlogis(getValues(hs), scale = 0.5)), na.rm = TRUE)
  
  if(missing(popsize)){
    popsize <- maxpop
  }
  
  v1 <- getValues(hs)
  
  threshold <- quantile(v1, probs = pp, na.rm = TRUE)
  
  v <- ifelse(is.na(v1), NA_real_, 0)
  
  y <- which(v1 >= threshold)
  
  zz <- sum(round(cc * 1/(1 + exp(-10*(v1[y] - 0.5)))), na.rm = TRUE)
  
  if(zz < popsize){warning ("Threshold too high, population reduced to maxpop")}
  
  
  
  
  if(popsize > maxpop){
    popsize <- maxpop
    warning("popsize reduced to maxpop")
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