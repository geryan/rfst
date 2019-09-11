outside.buffer <- function(x, y, buff.dist = 500){ # x is target species y is background species
  
  library(sf)
  library(dplyr)
  
  buff <- st_buffer(x %>% dplyr::select(-date) %>% filter(PA == 1),
                    dist = buff.dist)
  
  yinbuff <- y[buff,] %>%
    mutate(inbuff = 1)
  
  ynotinbuff <- st_join(y, yinbuff) %>%
    filter(is.na(inbuff)) %>%
    dplyr::select(-inbuff) %>%
    mutate(PA = PA.x,
           date = date.x) %>%
    dplyr::select(PA, date, geometry)
    
  return(ynotinbuff)
  
}