# 01 Landscape variables

library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(lwgeom)

load(file = "output/RData/00_comp_controls.RData")

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


# Fire and logging histories ----

ch_fire_history <- read_sf("data/shapefiles/DELWP_2019_interim_fire/FIRE_HISTOY_VICGRID_updated.shp") %>%
  st_transform(crs = ch_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(ch_mask)) %>%
  rasterize(y = ch_mask, field = "season", fun = max, background = NA) %>%
  mask(mask = ch_mask, filename = "output/landscape_vars/fire_history.grd", overwrite = TRUE)


ch_logging_history <- read_sf("data/shapefiles/viclogging/lastlog25.shp") %>%
  st_transform(crs = ch_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(ch_mask)) %>%
  mutate(season = SEASON %>% substr(1,4) %>% as.numeric) %>%
  rasterize(y = ch_mask, field = "season", fun = max, background = NA) %>%
  mask(mask = ch_mask, filename = "output/landscape_vars/logging_history.grd", overwrite = TRUE)

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
  ch_proj,
  ch_extent,
  ch_fire_history,
  ch_logging_history,
  file = "output/RData/01_landscape_variables.RData"
)

print("~fin~")