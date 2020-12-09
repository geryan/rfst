# 05 Geophysical variables

source(file = "R/spartan/spartan_settings.R")

library(raster)
library(magrittr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


# -------------------------------------------------

ahr <-raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/Anisotrophic_Heating_Ruggedness") %>%
  projectRaster(to = eg_mask) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/geophys_vars/ari_ahr_eg.grd",
      out_path_eg
    ),
    overwrite = TRUE
  )

lvdaw <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_all_wetlands_sept2012") %>%
  projectRaster(to = eg_mask) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/geophys_vars/ari_lvdaw_eg.grd",
      out_path_eg
    ),
    overwrite = TRUE
  )

lvdma <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_major_streams_sept2012") %>%
  projectRaster(to = eg_mask) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/geophys_vars/ari_lvdma_eg.grd",
      out_path_eg
    ),
    overwrite = TRUE
  )

lvdmi <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_minor_streams_sept2012") %>%
  projectRaster(to = eg_mask) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/geophys_vars/ari_lvdmi_eg.grd",
      out_path_eg
    ),
    overwrite = TRUE
  )

lvdsw <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_saline_wetlands_sept2012") %>%
  projectRaster(to = eg_mask) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/geophys_vars/ari_lvdsw_eg.grd",
      out_path_eg
    ),
    overwrite = TRUE
  )

tho <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/sept2014thorium") %>%
  projectRaster(to = eg_mask) %>%
  mask(
    mask = eg_mask,
    filename = sprintf(
      "%s/geophys_vars/ari_tho_eg.grd",
      out_path_eg
    ),
    overwrite = TRUE
  )


gv_i <- stack(lvdaw, lvdma, lvdmi, lvdsw, ahr, tho)

names(gv_i) <- c("lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho")

geo_vars_eg <- gv_i

gv_eg <- vector("list", ntimesteps + 1)

for(i in 1:(ntimesteps+1)){
  gv_eg[[i]] <- gv_i
}

save(
  gv_eg,
  geo_vars_eg,
  file = "output/RData/05_geophys_vars_eg.RData"
)