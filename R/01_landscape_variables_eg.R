# 01 Landscape variables

source("R/spartan/spartan_settings.R")

library(raster)
library(magrittr)
library(dplyr)
library(sf)
library(lwgeom)

load(file = "output/RData/00_controls_eg.RData")

source.functions("R/functions")

# load landscape objects ----

eg_eco <- raster("data/grids/EG/Ecoregion_EG.img") %>%
  round.extent

eg_aspect <- raster("data/grids/EG/Aspect_EG.img")%>%
  round.extent

eg_fire_eco <- raster("data/grids/EG/FireEco.img")%>%
  round.extent

eg_ic_19 <- raster("data/grids/EG/IC_EG_2019.img")%>%
  round.extent

eg_ic_20 <- raster("data/grids/EG/IC_EG_2020.img")%>%
  round.extent

eg_mgt <- raster("data/grids/EG/Zones_EG.img")%>%
  round.extent

eg_slope <- raster("data/grids/EG/Slope_EG.img")%>%
  round.extent

eg_stands <- raster("data/grids/EG/Stands_EG.img")%>%
  round.extent


# Create project landscape derivatives ----

eg_mask <- eg_eco
eg_mask[!is.na(eg_mask)] <- 1
eg_mask[eg_eco == 132] <- NA

eg_mask <- mask(x = eg_mask,
                mask = eg_mask,
                filename = sprintf(
                  "%s/%s",
                  out_path_eg,
                  "/landscape_vars/eg_mask.grd"
                ),
                overwrite = TRUE)

eg_proj <- eg_eco@crs
eg_extent <- extent(eg_eco)
eg_res <- res(eg_eco)

eg_mask_agg <- aggregate(
  x = eg_mask,
  fact = 10,
  fun = mean,
  na.rm = TRUE,
  filename = sprintf(
    "%s/%s",
    out_path_eg,
    "/landscape_vars/eg_mask_agg.grd"
  ),
  overwrite = TRUE
  )

# RFA shapefile ---------

rfa <- read_sf("data/shapefiles/RFA/")%>%
  st_transform(crs = eg_proj)

eg_rfa <- rfa[rfa$NAME== "EAST GIPPSLAND",] 

eg_rfa


# Fire and logging histories ----


fh20 <- read_sf("data/shapefiles/fire_history_2020/fire_history.shp") %>%
  st_transform(crs = eg_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(eg_mask))


fh19 <- fh20 %>%
  filter(SEASON != 2020)


eg_fire_history_19 <- fh19 %>%
  rasterize(y = eg_mask, field = "SEASON", fun = max, background = NA) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/%s",
      out_path_eg,
      "landscape_vars/fire_history_19.grd"
    ),
    overwrite = TRUE
  )

eg_fire_history_20 <- fh20 %>%
  rasterize(y = eg_mask, field = "SEASON", fun = max, background = NA) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/%s",
      out_path_eg,
      "landscape_vars/fire_history_20.grd"
    ),
    overwrite = TRUE
  )



fire_seasons <- unique(fh20$SEASON)[order(unique(fh20$SEASON))]

firelist <- lapply(
  X = fire_seasons,
  FUN = function(seasons, data, mask){
    result <-  data %>% 
      filter(
        SEASON == seasons
      ) %>%
      rasterize(
        y = mask,
        field = "SEASON",
        fun = max,
        background = NA
      )
    
    return(result)
  },
  data = fh20,
  mask = eg_mask
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

eg_fire_history_brick <- brick(
  x = filist
) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/%s",
      out_path_eg,
      "landscape_vars/eg_fire_history_brick.grd"
    ),
    overwrite = TRUE
  )


eg_logging_history <- read_sf("data/shapefiles/viclogging/lastlog25.shp") %>%
  st_transform(crs = eg_proj) %>%
  st_make_valid %>%
  st_crop(y = st_bbox(eg_mask)) %>%
  mutate(season = SEASON %>% substr(1,4) %>% as.numeric) %>%
  rasterize(y = eg_mask, field = "season", fun = max, background = NA) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/%s",
      out_path_eg,
      "landscape_vars/logging_history.grd"
    ),
    overwrite = TRUE
  )

## consider making logging history brick as per fire history?

# Save outputs ----

save(
  eg_eco,
  eg_aspect,
  eg_fire_eco,
  eg_ic_19,
  eg_ic_20,
  eg_mgt,
  eg_slope,
  eg_stands,
  eg_mask,
  eg_mask_agg,
  eg_proj,
  eg_extent,
  eg_res,
  eg_rfa,
  eg_fire_history_19,
  eg_fire_history_20,
  eg_fire_history_brick,
  eg_logging_history,
  rfa,
  file = "output/RData/01_landscape_variables_eg.RData"
)
