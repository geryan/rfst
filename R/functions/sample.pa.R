sample.pa <- function(
  z, # PA data to sample
  rfa, # sf object (RFA boundary) to sample from
  cellsize = 500, # length of square cell edge,
  species = NA
){
  
  library(sf)
  library(dplyr)

  
  sg <- st_make_grid(
    x = rfa,
    cellsize = cellsize
  )
  
  
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
  
  
  
  return(result)
  
}