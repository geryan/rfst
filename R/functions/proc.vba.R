proc.vba <- function(x, project.crs, vba.crs = 4283, cutoff.date = "2009-03-01", sm = FALSE, pattern = "spotlight"){
  
  library(dplyr)
  library(lubridate)
  
  z <- read.vba(x) %>%
    dplyr::rename("date" = `Survey Start Date`,
                  "lon" = `Longitude GDA94`,
                  "lat" = `Latitude GDA94`,
                  "count" = `Total Count`,
                  "sm" = `Survey method`) %>%
    dplyr::select(sm, date, lon, lat, count) %>%
    mutate(date = dmy(date)) %>%
    filter(date > ymd(cutoff.date)) %>%
    mutate(PA = ifelse(count != 0 | is.na(count), 1, 0)) %>%
    dplyr::select(lon, lat, PA, date, sm) %>%
    dplyr::arrange(date, PA, lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = vba.crs) %>%
    st_transform(crs = project.crs)
  
  
  if(sm){
    
    z <- z[grep(pattern = pattern,
                x = z$sm,
                ignore.case = TRUE),] %>%
      mutate(PA = 0) %>%
      dplyr::select(PA, date, geometry)
    
  } else{
    
    z <- z %>%
      dplyr::select(PA, date, geometry)
    
  }

  
  return(z)
}