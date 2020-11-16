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


mort <- lapply(
  X = disturbance_variables$dist_vars[[i]],
  FUN = function(x){
    x$mort
  }
)

mort_agg <- lapply(
  X = mort,
  FUN = raster::aggregate,
  fact = 10,
  fun = mean,
  na.rm = TRUE
) %>%
  brick

writeRaster(
  mort_agg,
  filename = sprintf(
    fmt = "%s/mort_agg_%s.grd",
    "/data/gpfs/projects/punim0995/rfst/output/mort_aggregated",
    disturbance_variables$scn_id[i]
  ),
  overwrite = TRUE
)

mort_agg <- brick(
  x = sprintf(
    fmt = "%s/mort_agg_%s.grd",
    "/data/gpfs/projects/punim0995/rfst/output/mort_aggregated",
    disturbance_variables$scn_id[i]
  )
)

mort_table <- tibble(
  scn_id = disturbance_variables$scn_id[i],
  mort_agg = list(mort_agg)
)


saveRDS(
  object = mort_table,
  file = sprintf(
    fmt = "%s/mort_agg_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mort_agg",
    disturbance_variables$scn_id[i]
  )
)

