source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

scn_id <- landis_variables$scn_id[[i]]

firesev <- lapply(
  X = landis_variables$landis_vars[[i]],
  FUN = function(y){
    y[["firesev"]]
  }
)

harv <- lapply(
  X = landis_variables$landis_vars[[i]],
  FUN = function(z){
    z[["harvest"]]
  }
)



result <- get.dist(
  fire_history = ch_fire_history,
  logging_history = ch_logging_history,
  fs = firesev,
  ha = harv,
  out_path = "output/landscape_vars",
  scn_id = scn_id,
  proj_mask = ch_mask,
  timesteps = ntimesteps,
  year0 = year0
)

saveRDS(
  object = result,
  file = sprintf(
    "output/spartan_RData/dist_vars/dvs_%s.Rds",
    scn_id
  )
)
