# 4.1 aggregate mortality

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(magrittr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/04_disturbance_variables_eg.RData")


source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


mort <- lapply(
  X = disturbance_variables_eg$dist_vars[[i]],
  FUN = function(x){
    x$mort
  }
)

mort_agg <- lapply(
  X = mort,
  FUN = raster::aggregate,
  fact = 2,
  fun = min,
  na.rm = TRUE
) %>%
  brick

writeRaster(
  mort_agg,
  filename = sprintf(
    fmt = "%s/mort_agg4_%s.grd",
    "/data/gpfs/projects/punim1340/rfst_eg/output/mort_aggregated",
    disturbance_variables_eg$yscn_id[i]
  ),
  overwrite = TRUE
)

mort_agg <- brick(
  x = sprintf(
    fmt = "%s/mort_agg4_%s.grd",
    "/data/gpfs/projects/punim1340/rfst_eg/output/mort_aggregated",
    disturbance_variables_eg$yscn_id[i]
  )
)

mort_table <- tibble(
  yscn_id = disturbance_variables_eg$yscn_id[i],
  mort_agg = list(mort_agg)
)


saveRDS(
  object = mort_table,
  file = sprintf(
    fmt = "%s/mort_agg4_%s.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mort_agg",
    disturbance_variables_eg$yscn_id[i]
  )
)

