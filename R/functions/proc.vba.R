proc.vba <- function(
  x,
  project.crs,
  vba.crs = 4283,
  cutoff.date = "2009-03-01",
  sm = FALSE,
  pattern = "spotlight"
){
  
  library(dplyr)
  library(lubridate)
  
  z <- read.vba(x) %>%
    dplyr::rename(
      "species" = `Scientific Name`,
      "date" = `Survey Start Date`,
      "lon" = `Longitude GDA94`,
      "lat" = `Latitude GDA94`,
      "count" = `Total Count`,
      "proj_id" = `Project ID`,
      "survey_method" = `Survey method`
    ) %>%
    mutate(date = dmy(date)) %>%
    filter(date > ymd(cutoff.date)) %>%
    mutate(PA = ifelse(count != 0 | is.na(count), 1, 0)) %>%
    dplyr::select(
      species,
      lon,
      lat,
      PA,
      date,
      proj_id,
      survey_method
    ) %>%
    dplyr::arrange(
      species,
      date,
      PA,
      lon,
      lat,
      proj_id,
      survey_method
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = vba.crs) %>%
    st_transform(crs = project.crs)
  
  
  if(sm){
    
    z <- z[grep(pattern = pattern,
                x = z$survey_method,
                ignore.case = TRUE),] %>%
      mutate(PA = 0) %>%
      dplyr::select(
        species,
        PA,
        date,
        proj_id,
        survey_method,
        geometry
      )
    
  } else{
    
    z <- z %>%
      dplyr::select(
        species,
        PA,
        date,
        proj_id,
        survey_method,
        geometry
      )
    
  }

  return(z)
  
}