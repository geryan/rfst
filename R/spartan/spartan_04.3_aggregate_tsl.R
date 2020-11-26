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

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")


source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


tsl <- lapply(
  X = disturbance_variables$dist_vars[[i]],
  FUN = function(x){
    x$tsl
  }
)

tsl_agg <- lapply(
  X = tsl,
  FUN = raster::aggregate,
  fact = 10,
  fun = tsl.agg,
  na.rm = TRUE
) %>%
  brick

writeRaster(
  tsl_agg,
  filename = sprintf(
    fmt = "%s/tsl_agg_%s.grd",
    "/data/gpfs/projects/punim0995/rfst/output/tsl_aggregated",
    disturbance_variables$scn_id[i]
  ),
  overwrite = TRUE
)

tsl_agg <- brick(
  x = sprintf(
    fmt = "%s/tsl_agg_%s.grd",
    "/data/gpfs/projects/punim0995/rfst/output/tsl_aggregated",
    disturbance_variables$scn_id[i]
  )
)


tsl_agg_5 <- lapply(
  X = tsl,
  FUN = raster::aggregate,
  fact = 5,
  fun = tsl.agg,
  na.rm = TRUE
) %>%
  brick

writeRaster(
  tsl_agg_5,
  filename = sprintf(
    fmt = "%s/tsl_agg_5_%s.grd",
    "/data/gpfs/projects/punim0995/rfst/output/tsl_aggregated",
    disturbance_variables$scn_id[i]
  ),
  overwrite = TRUE
)

tsl_agg_5 <- brick(
  x = sprintf(
    fmt = "%s/tsl_agg_5_%s.grd",
    "/data/gpfs/projects/punim0995/rfst/output/tsl_aggregated",
    disturbance_variables$scn_id[i]
  )
)

tsl_table <- tibble(
  scn_id = disturbance_variables$scn_id[i],
  tsl_agg = list(tsl_agg),
  tsl_agg5 = list(tsl_agg5)
)

saveRDS(
  object = tsl_table,
  file = sprintf(
    fmt = "%s/tsl_agg_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/tsl_agg",
    disturbance_variables$scn_id[i]
  )
)

