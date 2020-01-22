outside.buffer <- function(
  x, # target species or all species data
  y = NA, # background data set. If NA will filter by species
  buff.dist = 500,
  species = NA,
  survey_method = NA
){ 
  
  library(sf)
  library(dplyr)
  
  spcs <- species
  nay <- is.na(y)
  
  
  if(!is.na(spcs)){
    
    if(nay){
      y <- x %>%
        filter(species != spcs)
    }
    
    x <- x %>%
      filter(species == spcs)
    
    
    if(nay){
      xps <- x %>%
        mutate(
          ps = paste0(
            proj_id,
            survey_method
          )
        ) %$%
        unique(.$ps)
      
      y1 <- y %>%
        filter(proj_id != 1) %>% # this project id and doesn't actually collate a project
        mutate(
          ps = paste0(
            proj_id,
            survey_method
          )
        ) %>%
        filter(ps %in% xps) %>%
        dplyr::select(-ps)
      
      sm <- survey_method
      
      if(!is.na(sm[1])){
        
        y2 <- y %>%
          filter(survey_method %in% sm)
       
        y <- rbind(
          y1,
          y2
        )
         
      } else{
        y <- y1
      }
    }
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

