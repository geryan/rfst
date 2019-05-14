proc.vba <- function(x, vba.crs = 4283, project.crs = 32755, cutoff.date = "2009-03-01"){
  
  library(dplyr)
  
  z <- read.vba(x) %>%
    dplyr::rename("date" = `Survey Start Date`,
                  "lon" = `Longitude GDA94`,
                  "lat" = `Latitude GDA94`,
                  "count" = `Total Count`) %>%
    select(date, lon, lat, count) %>%
    mutate(date = dmy(date)) %>%
    filter(date > ymd(cutoff.date)) %>%
    mutate(PA = ifelse(count != 0 | is.na(count), 1, 0)) %>%
    select(lon, lat, PA) %>%
    st_as_sf(coords = c("lon", "lat"), crs = vba.crs) %>%
    st_transform(crs = project.crs)
  
  return(z)
}