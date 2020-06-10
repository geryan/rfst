# 03 LANDIS variables

source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

print(i)

landis_paths <- list.dirs(
  path = "/scratch/punim0995",
  recursive = FALSE
)


scn_path <- landis_paths[i]


scn_id <- strsplit(
  x = scn_path,
  split = "/"
)[[1]][4]

print(scn_id)

print(scn_path)


bmpath <- paste0(scn_path, "/output/biomass/")

zz <- raster(
  sprintf("%s/bio-eucacama-37.img",
          bmpath)
)


getValues(zz)

save(
  zz,
  file = sprintf(
    "output/RData/03_LANDIS_variables_%s.RData",
    scn_id
  )
 )
 
