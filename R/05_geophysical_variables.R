# 05 Geophysical variables

library(raster)
library(magrittr)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


# -------------------------------------------------

ahr <-raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/Anisotrophic_Heating_Ruggedness") %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/geophys_vars/ari_ahr.grd", overwrite = TRUE)

lvdaw <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_all_wetlands_sept2012") %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/geophys_vars/ari_lvdaw.grd", overwrite = TRUE)

lvdma <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_major_streams_sept2012") %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/geophys_vars/ari_lvdma.grd", overwrite = TRUE)

lvdmi <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_minor_streams_sept2012") %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/geophys_vars/ari_lvdmi.grd", overwrite = TRUE)

lvdsw <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/log_vertical_distance_saline_wetlands_sept2012") %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/geophys_vars/ari_lvdsw.grd", overwrite = TRUE)

tho <- raster(x = "data/grids/Env_covariates_noClimate_VicGrid94/sept2014thorium") %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/habitat_vars/ari_tho.grd", overwrite = TRUE)


save(
  ahr,
  lvdaw,
  lvdma,
  lvdmi,
  lvdsw,
  tho,
  file = "output/RData/05_geophys_vars.RData"
)