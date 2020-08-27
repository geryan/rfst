# 01 Landscape variables

source("R/spartan/spartan_settings.R")

library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(lwgeom)

load(file = "output/RData/00_controls.RData")

source.functions("R/functions")

# load landscape objects ----

ch_eco <- raster("data/grids/eco_v12.img") %>%
  round.extent

ch_aspect <- raster("data/grids/aspect_v5.img")%>%
  round.extent

ch_fire_eco <- raster("data/grids/fire_eco_v7.img")%>%
  round.extent

ch_ic <- raster("data/grids/IC_2019_V6.img")%>%
  round.extent

ch_mgt <- raster("data/grids/mgt_zone_V2.img")%>%
  round.extent

ch_slope <- raster("data/grids/Slope_V7.img")%>%
  round.extent

ch_stands <- raster("data/grids/stands_CHR_V2.img")%>%
  round.extent


# Create project landscape derivatives ----

ch_mask <- ch_eco
ch_mask[!is.na(ch_mask)] <- 1
ch_mask[ch_eco == 132] <- NA

ch_mask <- mask(x = ch_mask,
                mask = ch_mask,
                filename = "output/landscape_vars/ch_mask.grd",
                overwrite = TRUE)

ch_proj <- ch_eco@crs
ch_extent <- extent(ch_eco)
ch_res <- res(ch_eco)

ch_mask_agg <- aggregate(
  x = ch_mask,
  fact = 10,
  fun = mean,
  na.rm = TRUE,
  filename = "output/landscape_vars/ch_mask_agg.grd",
  overwrite = TRUE
  )

# RFA shapefile ---------

rfa <- read_sf("data/shapefiles/RFA/")%>%
  st_transform(crs = ch_proj)

ch_rfa <- rfa[rfa$NAME== "CENTRAL HIGHLANDS",] 

ch_rfa


# Fire and logging histories ----

ch_fire_history <- read_sf("data/shapefiles/DELWP_2019_interim_fire/FIRE_HISTOY_VICGRID_updated.shp") %>%
  st_transform(crs = ch_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(ch_mask)) %>%
  rasterize(y = ch_mask, field = "season", fun = max, background = NA) %>%
  mask(mask = ch_mask, filename = "output/landscape_vars/fire_history.grd", overwrite = TRUE)


firech <- read_sf("data/shapefiles/DELWP_2019_interim_fire/FIRE_HISTOY_VICGRID_updated.shp") %>%
  st_transform(crs = ch_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(ch_mask))

fire_seasons <- unique(firech$season)[order(unique(firech$season))]

firelist <- lapply(
  X = fire_seasons,
  FUN = function(seasons, data, mask){
    result <-  data %>% 
      filter(
        season == seasons
      ) %>%
      rasterize(
        y = mask,
        field = "season",
        fun = max,
        background = NA
      )
    
    return(result)
  },
  data = firech,
  mask = ch_mask
)

filist <- lapply(
  X = firelist,
  FUN = function(x){
    x[!is.na(x)] <- 1
    return(x)
  }
) %>%
  stack

names(filist) <- fire_seasons

ch_fire_history_brick <- brick(
  x = filist
) %>%
  mask(
    mask = ch_mask,
    filename = "output/landscape_vars/ch_fire_history_brick.grd",
    overwrite = TRUE
  )


ch_logging_history <- read_sf("data/shapefiles/viclogging/lastlog25.shp") %>%
  st_transform(crs = ch_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(ch_mask)) %>%
  mutate(season = SEASON %>% substr(1,4) %>% as.numeric) %>%
  rasterize(y = ch_mask, field = "season", fun = max, background = NA) %>%
  mask(mask = ch_mask, filename = "output/landscape_vars/logging_history.grd", overwrite = TRUE)

## consider making logging history brick as per fire history?

# Save outputs ----

save(
  ch_eco,
  ch_aspect,
  ch_fire_eco,
  ch_ic,
  ch_mgt,
  ch_slope,
  ch_stands,
  ch_mask,
  ch_mask_agg,
  ch_proj,
  ch_extent,
  ch_res,
  ch_rfa,
  ch_fire_history,
  ch_fire_history_brick,
  ch_logging_history,
  rfa,
  file = "output/RData/01_landscape_variables.RData"
)
