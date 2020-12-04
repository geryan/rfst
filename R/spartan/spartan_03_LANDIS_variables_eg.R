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


yscn_path <- landis_paths[i]

split_path <- strsplit(
  x = yscn_path,
  split = "/"
)

yscn_id <- split_path[[1]][length(split_path[[1]])]

print(yscn_id)

print(yscn_path)

ht <- !grepl(
  pattern = "TH00",
  yscn_id
)


result <- get.landis.vars.eg(
  scn_path = scn_path,
  proj_path = proj_path,
  out_path = sprintf(
    "%s/%s",
    out_path_eg,
    "habitat_vars"
  ),
  scn_id = yscn_id,
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
    yscn_id
  )
)


 
