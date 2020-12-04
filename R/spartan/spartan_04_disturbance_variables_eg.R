source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/03_LANDIS_variables_eg.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

yscn_id <- landis_variables_eg$yscn_id[[i]]

firesev <- lapply(
  X = landis_variables_eg$landis_vars[[i]],
  FUN = function(y){
    y[["firesev"]]
  }
)

harv <- lapply(
  X = landis_variables_eg$landis_vars[[i]],
  FUN = function(z){
    z[["harvest"]]
  }
)

yearid <- landis_variables_eg$yearid[[i]]

if(yearid == "EG19"){
  fire_history <- eg_fire_history_19
} else if(yearid == "EG20"){
  fire_history <- eg_fire_history_20
}


result <- get.dist(
  fire_history = fire_history,
  logging_history = eg_logging_history,
  fs = firesev,
  ha = harv,
  out_path = sprintf(
    "%s/landscape_vars",
    out_path_eg
  ),
  scn_id = yscn_id,
  proj_mask = eg_mask,
  timesteps = ntimesteps,
  year0 = year0
)

saveRDS(
  object = result,
  file = sprintf(
    "%s/spartan_RData/dist_vars/dvs_%s_eg.Rds",
    out_path_eg,
    yscn_id
  )
)
