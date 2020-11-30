# 03 LANDIS variables

source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)
library(foreach)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

print(i)

landis_paths <- list.dirs(
  path = landis_path_eg,
  recursive = FALSE
)


scn_path <- landis_paths[i]

split_path <- strsplit(
  x = scn_path,
  split = "/"
)

scn_id <- split_path[[1]][length(split_path[[1]])]

print(scn_id)

print(scn_path)

ht <- !grepl(
  pattern = "TH00",
  scn_id
)


result <- get.landis.vars.eg(
  scn_path = scn_path,
  proj_path = proj_path,
  out_path = sprintf(
    "%s/%s",
    out_path_eg,
    "habitat_vars"
  ),
  scn_id = scn_id,
  proj_mask = eg_mask,
  timesteps = ntimesteps,
  cores = ncores,
  harvest_timber = ht
)

saveRDS(
  object = result,
  file = sprintf(
    "%s/landscape_vars/landis_RDS/landis_vars_%s.Rds",
    out_path_eg,
    scn_id
  )
)


 
