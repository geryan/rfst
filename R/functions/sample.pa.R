sample.pa <- function(
  z, # PA data to sample
  rfa, # sf object (RFA boundary) to sample from
  cellsize = 500 # length of square cell edge
){
  
  library(sf)
  library(dplyr)
  library(rlang)
  
  
  sg <- st_make_grid(
    x = rfa,
    cellsize = cellsize
  )
  
  z0 <- z %>% filter(PA == 0)
  z1 <- z %>% filter(PA == 1)
  
  p0 <- t(st_within(z, sg))
  p1 <- t(st_within(z, sg))
  
  
  n0 <- numeric(length(sg))
  n1 <- numeric(length(sg))
  
  for(i in 1:length(sg)){
    
    n0[i] <- ifelse(is_empty(p0[[i]]), 0, ifelse(length(p0[[i]]) == 1, p0[[i]][1], sample(p0[[i]], 1)))
    n1[i] <- ifelse(is_empty(p1[[i]]), 0, ifelse(length(p1[[i]]) == 1, p1[[i]][1], sample(p1[[i]], 1)))
    
  }
  
  s0 <- z[n0,]
  s1 <- z[n1,]
  
  result <- rbind(s0, s1) %>%
    dplyr::arrange(date)
  
  return(result)
  
}