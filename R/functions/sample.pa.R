sample.pa <- function(
  z, # PA data to sample
  rfa, # sf object (RFA boundary) to sample from
  cellsize = 500, # length of square cell edge,
  species = NA,
  sg = NULL
){
  
  library(sf)
  library(dplyr)

  
  if(is.null(sg)){
    sg <- st_make_grid(
      x = rfa,
      cellsize = cellsize
    )
  }
  
  
  spcs <- species
  
  if(!is.na(spcs)){
    
    z <- z %>%
      filter(species == spcs)
  }
  
  # new  code uses group by and filters out to most recent presence and absence per cell
  
  result <- z %>%
    mutate(
      gridcell = st_within(
        x = .,
        y = sg
      ) %>%
        as.numeric
    ) %>%
    #unnest(gridcell) %>%
    group_by(gridcell, PA) %>%
    arrange(gridcell, PA, desc(date)) %>%
    filter(row_number() == 1) %>%
    ungroup %>%
    dplyr::select(-gridcell)
  
  
  # old code randomly sampled 1 P and 1 A per cell
  # z0 <- z %>% filter(PA == 0)
  # z1 <- z %>% filter(PA == 1)
  # 
  # p0 <- t(st_within(z0, sg))
  # p1 <- t(st_within(z1, sg)) # if re-using this check if this should be z or z1 and z2...
  # 
  # 
  # n0 <- numeric(length(sg))
  # n1 <- numeric(length(sg))
  # 
  # for(i in 1:length(sg)){
  #   
  #   n0[i] <- ifelse(is_empty(p0[[i]]), 0, ifelse(length(p0[[i]]) == 1, p0[[i]][1], sample(p0[[i]], 1)))
  #   n1[i] <- ifelse(is_empty(p1[[i]]), 0, ifelse(length(p1[[i]]) == 1, p1[[i]][1], sample(p1[[i]], 1)))
  #   
  # }
  # 
  # s0 <- z[n0,]
  # s1 <- z[n1,]
  # 
  # result <- rbind(s0, s1) %>%
  #   dplyr::arrange(date)
  
  return(result)
  
}