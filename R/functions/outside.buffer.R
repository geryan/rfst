outside.buffer <- function(
  x, # target species or all species data
  y = NA, # background data set. If NA will filter by species
  buff.dist = 500,
  species = NA
){ 
  
  library(sf)
  library(dplyr)
  
  spcs <- species
  
  if(!is.na(spcs)){
    
    if(is.na(y)){
      y <- x %>%
        filter(species != spcs)
    }
    
    x <- x %>%
      filter(species == spcs)
    
    xprojids <- unique(x$proj_id)
    
    y <- y %>%
      filter(proj_id != 1) %>% # this project id is just general observations and doesn't actually collate a project
      filter(proj_id %in% xprojids) 
    
  }
  
  
  buff <- st_buffer(
    x = x,
    dist = buff.dist
  )
  
  yinbuff <- y[buff,] %>%
    mutate(inbuff = 1)
  
  ynotinbuff <- st_join(y, yinbuff) %>%
    filter(is.na(inbuff)) %>%
    dplyr::select(
      species = species.x,
      PA = PA.x,
      date = date.x,
      proj_id = proj_id.x,
      survey_method = survey_method.x,
      geometry
    ) %>%
    filter(PA == 1) %>%
    mutate(
      species = spcs,
      PA = 0
    ) %>%
    distinct(
      species,
      PA,
      date,
      proj_id,
      survey_method,
      geometry
    )
    
  return(ynotinbuff)
  
}

